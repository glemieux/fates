Module FatesTwoStreamUtilsMod

  ! This module holds routines that are specific to connecting FATES with
  ! the two-stream radiation module. These routines are used to
  ! describe the scattering elements from cohort and patch data, and are
  ! used to decompose the scattering elements to return values
  ! at the cohort, or patch-pft scale.
  use FatesConstantsMod     , only : r8 => fates_r8
  use FatesConstantsMod     , only : ifalse
  use FatesConstantsMod     , only : itrue
  use FatesConstantsMod     , only : nearzero,nocomp_bareground
  use shr_log_mod           , only : errMsg => shr_log_errMsg
  use FatesGlobals          , only : fates_log
  use FatesGlobals          , only : endrun => fates_endrun
  use shr_infnan_mod        , only : nan => shr_infnan_nan, assignment(=)
  use FatesInterfaceTypesMod, only : numpft
  use FatesRadiationMemMod  , only : num_swb
  use FatesRadiationMemMod  , only : ivis, inir
  use FatesRadiationMemMod  , only : rho_snow,tau_snow
  use TwoStreamMLPEMod      , only : normalized_upper_boundary
  use TwoStreamMLPEMod      , only : air_ft, AllocateRadParams, rad_params
  use FatesCohortMod        , only : fates_cohort_type
  use FatesPatchMod         , only : fates_patch_type
  use EDTypesMod            , only : ed_site_type
  use EDParamsMod           , only : nclmax
  use TwoStreamMLPEMod      , only : twostream_type
  use TwoStreamMLPEMod      , only : RadParamPrep
  use TwoStreamMLPEMod      , only : AllocateRadParams
  use TwoStreamMLPEMod      , only : rel_err_thresh,area_err_thresh
  use EDPftvarcon           , only : EDPftvarcon_inst
  use FatesAllometryMod     , only : VegAreaLayer
  use EDParamsMod           , only : maxpatch_total
  
  implicit none

  logical, parameter :: debug  = .false. ! local debug flag
  character(len=*), parameter, private :: sourcefile = &
       __FILE__


  public :: FatesConstructRadElements
  public :: FatesGetCohortAbsRad
  public :: FatesPatchFSun
  public :: CheckPatchRadiationBalance
  
contains


  subroutine FatesConstructRadElements(site)

    type(ed_site_type)  :: site

    type(fates_patch_type),pointer :: patch
    type(fates_cohort_type), pointer :: cohort
    integer :: n_col(nclmax) ! Number of parallel column elements per layer
    integer :: ican,ft,icol
    type(twostream_type), pointer :: twostr


    ! DO NOT MAKE CANOPY_OPEN_FRAC >0 UNTIL LAI COMPRESSION
    ! HAS BEEN THOUGHT THROUGH. WE CANT JUST DECREASE THE
    ! AREA WITHOUT CONSERVING TOTAL LEAF AND STEM AREA
    real(r8), parameter :: canopy_open_frac = 0.00_r8

    integer :: maxcol
    real(r8) :: canopy_frac(nclmax)
    integer  :: ifp
    integer  :: ib
    ! Area indices for the cohort [m2 media / m2 crown footprint]
    real(r8) :: elai_cohort,tlai_cohort,esai_cohort,tsai_cohort
    real(r8) :: vai_top,vai_bot  ! veg area index at top and bottom of cohort (dummy vars)

    real(r8) :: area_ratio ! If elements are over 100% of available
                           ! canopy area, this is how much we squeeze
                           ! the area down by, as a ratio. This is also
                           ! applied to increase LAI and SAI in the cohorts
                           ! and elements as well (to preserve mass and volume).

    integer :: max_elements     ! Maximum number of scattering elements on the site
    integer :: n_scr            ! The size of the scratch arrays
    logical :: allocate_scratch ! Whether to re-allocate the scratch arrays

    integer  :: icolmax         ! Column index for each layer with largest area footprint
    real(r8) :: areamax         ! The area footprint of the largest column
    
    ! its possible that there is more horizontal area taken up by the cohorts
    ! than there is ground, which is simply a result numerical and algorithmic
    ! imprecision in the rest of FATES. If this is true, then we need
    ! to somehow force the total area of our scattering elements to be exactly
    ! 1 and not slightly more. One way is to just chop off some area of the
    ! largest scattering element (simple way), the other is to chop off that
    ! area but also increase the LAI+SAI. The latter is conservative, but
    ! could create indexing problems when transfering fluxes back into FATES arrays
    
    logical, parameter :: do_simple_area_correct = .true.
    
    ! These parameters are not used yet
    !real(r8) :: max_vai_diff_per_elem ! The maximum vai difference in any element
    !                                  ! between the least and most vai of constituting
    !                                  ! cohorts.  THe objective is to reduce this.
    !integer, parameter  :: max_el_per_layer = 10
    !real(r8), parameter :: init_max_vai_diff_per_elem = 0.2_r8
    !type(fates_cohort_type), pointer :: elem_co_ptrs(ncl*max_el_per_layer,100)

    max_elements = -1

    ifp = 0
    patch => site%oldest_patch
    do while (associated(patch))

       if_notbareground: if(patch%nocomp_pft_label.ne.nocomp_bareground)then

       ifp = ifp + 1
          
       !ifp = patch%patchno
       associate(twostr => patch%twostr)

         
         ! Identify how many elements we need, and possibly consolidate
         ! cohorts into elements where they are very similar (LAI and PFT)
         ! -------------------------------------------------------------------------------------------

         !max_vai_diff_per_elem = init_max_vai_diff_per_elem
         !iterate_count_do: do while(iterate_element_count)then

         ! Identify how many elements we need
         n_col(1:nclmax) = 0
         cohort => patch%tallest
         do while (associated(cohort))
            ft = cohort%pft
            ican = cohort%canopy_layer
            n_col(ican) = n_col(ican) + 1
            cohort => cohort%shorter
         enddo

         ! If there is only one layer, then we don't
         ! need to add an air element to the only
         ! layer. This is because all non-veg
         ! area will be attributed to a ground patch
         ! But if there is more than one layer, then
         ! an air element is needed for all the non
         ! occupied space, even if the canopy_open_frac
         ! is zero.
         ! If the area of the elements does not match
         ! the area of the canopy space within 1.e-7_r8
         ! then we either add the space in the form of air
         ! or we compress the space by literally squeezing
         ! the elements (which consequently increases their
         ! LAI and SAI to conserve area)
        
            
         if(patch%total_canopy_area>nearzero)then
            canopy_frac(:) = 0._r8
            cohort => patch%tallest
            do while (associated(cohort))
               ican = cohort%canopy_layer
               canopy_frac(ican) = canopy_frac(ican) + cohort%c_area/patch%total_canopy_area
               cohort => cohort%shorter
            enddo
         else
            canopy_frac(:) = 0._r8
         end if

         ! Add the air element if the canopy area is not filled
         do ican = 1,patch%ncl_p
            if( (1._r8-canopy_frac(ican))>area_err_thresh ) then
               n_col(ican) = n_col(ican) + 1
            end if
         end do


         ! Handle memory
         ! If the two-stream object is not large enough
         ! or if it is way larger than what is needed
         ! re-allocate the object
         ! -------------------------------------------------------------------------------------------

         maxcol = 0
         do ican = 1,patch%ncl_p
            if (n_col(ican)>maxcol) maxcol=n_col(ican)
         end do

         if(.not.associated(twostr%scelg)) then

            call twostr%AllocInitTwoStream((/ivis,inir/),patch%ncl_p,maxcol+2)

         else

            if(ubound(twostr%scelg,2) <  maxcol .or. &
               ubound(twostr%scelg,2) > (maxcol+4) .or. &
               ubound(twostr%scelg,1) < patch%ncl_p ) then

               call twostr%DeallocTwoStream()

               ! Add a little more space than necessary so
               ! we don't have to keep allocating/deallocating
               call twostr%AllocInitTwoStream((/ivis,inir/),patch%ncl_p,maxcol+2)

            end if
            
         end if


         ! Fill the elements with their basic data and
         ! reference the cohort to the elements
         ! -------------------------------------------------------------------------------------------

         n_col(1:nclmax) = 0
         cohort => patch%tallest
         do while (associated(cohort))

            ft = cohort%pft
            ican = cohort%canopy_layer

            patch%canopy_mask(ican,ft) = 1
            
            ! Every cohort gets its own element right now
            n_col(ican) = n_col(ican)+1

            ! If we pass layer index 0 to this routine
            ! it will return the total plant LAIs and SAIs
            call VegAreaLayer(cohort%treelai, &
                              cohort%treesai, &
                              cohort%height,    &
                              0,                     &
                              cohort%nv,      &
                              cohort%pft,     &
                              site%snow_depth,       &
                              vai_top, vai_bot,      & 
                              elai_cohort,esai_cohort)

            ! Its possible that this layer is covered by snow
            ! if so, then just consider it an air layer
            if((elai_cohort+esai_cohort)>nearzero)then
               twostr%scelg(ican,n_col(ican))%pft = ft
            else
               twostr%scelg(ican,n_col(ican))%pft = air_ft
            end if

            twostr%scelg(ican,n_col(ican))%area = cohort%c_area/patch%total_canopy_area
            twostr%scelg(ican,n_col(ican))%lai  = elai_cohort
            twostr%scelg(ican,n_col(ican))%sai  = esai_cohort
            
            ! Cohort needs to know which column its in
            cohort%twostr_col = n_col(ican)

            if (debug) then
               if ( twostr%scelg(ican,n_col(ican))%area .gt. 1.1_r8) then
                  write(fates_log(),*) 'error in calc of twostr%scelg(ican,n_col(ican))%area.'
                  write(fates_log(),*) 'should be less than 1'
                  write(fates_log(),*) twostr%scelg(ican,n_col(ican))%area
                  write(fates_log(),*) cohort%c_area, patch%total_canopy_area
                  call patch%Dump()
                  cohort => patch%tallest
                  do while (associated(cohort))
                     write(fates_log(),*) ' ------- dumping cohort ------'
                     call cohort%Dump()
                     write(fates_log(),*) ''
                     cohort => cohort%shorter
                  enddo
                  call endrun(msg=errMsg(sourcefile, __LINE__))
               endif
            end if

            cohort => cohort%shorter
         enddo


         do ican = 1,patch%ncl_p

            ! If the canopy is not full, add an air element
            if( (1._r8-canopy_frac(ican))>area_err_thresh ) then
               n_col(ican) = n_col(ican) + 1
               twostr%scelg(ican,n_col(ican))%pft  = air_ft
               twostr%scelg(ican,n_col(ican))%area = 1._r8-canopy_frac(ican)
               twostr%scelg(ican,n_col(ican))%lai  = 0._r8
               twostr%scelg(ican,n_col(ican))%sai  = 0._r8
            end if

            ! Check to see if any of these layers are extremely over-full and stop the
            ! model if so. This should not happen and would most likely be an issue
            ! with the canopy ppa promotion/demotion logic
            ! This is more of a sanity check, so we use a 1% threshold
            if(debug)then
               if_very_overfull: if( (canopy_frac(ican)-1._r8)>0.01_r8 ) then
                  write(fates_log(),*) 'One of the fates canopy layers takes up'
                  write(fates_log(),*) 'more than 100% of the area footprint, exceeding a'
                  write(fates_log(),*) 'precision threshold of 0.01'
                  write(fates_log(),*) 'Aborting'
                  write(fates_log(),*) 'canopy layer: ',ican,' canopy_frac:',canopy_frac(ican)
                  call endrun(msg=errMsg(sourcefile, __LINE__))
               end if if_very_overfull
            end if
            ! If the layer is overfull, remove some from area from
            ! the element with the largest footprint

            if_overfull: if( (canopy_frac(ican)-1._r8)>area_err_thresh ) then

               ! First find the element with the largest footprint
               icolmax = -1
               areamax = 0
               do icol = 1,n_col(ican)
                  if(twostr%scelg(ican,icol)%area>areamax) then
                     icolmax = icol
                     areamax = twostr%scelg(ican,icol)%area
                  end if
               end do

               ! Test out a simpler way to correct area errors
               if(do_simple_area_correct) then
                  if(debug) then
                     if((canopy_frac(ican)-1._r8)>0.5_r8*twostr%scelg(ican,icolmax)%area)then
                        write(fates_log(),*) 'An area correction is being applied where '
                        write(fates_log(),*) 'the correction is greater than 50% of the area of the largest donor.'
                        write(fates_log(),*) 'This will have to large of an impact on the donor and is not representative'
                        write(fates_log(),*) 'of the actual composition of the canopy'
                        write(fates_log(),*) 'Aborting'
                        write(fates_log(),*) 'canopy layer: ',ican,' canopy_frac:',canopy_frac(ican)
                        write(fates_log(),*) 'existing donor area',twostr%scelg(ican,icolmax)%area
                        call endrun(msg=errMsg(sourcefile, __LINE__))
                     end if
                  end if
                  twostr%scelg(ican,icolmax)%area = twostr%scelg(ican,icolmax)%area - (canopy_frac(ican)-1._r8)
               else
                  area_ratio = (twostr%scelg(ican,icolmax)%area + (1._r8-canopy_frac(ican)))/twostr%scelg(ican,icolmax)%area
                  twostr%scelg(ican,icolmax)%area = twostr%scelg(ican,icolmax)%area * area_ratio
                  twostr%scelg(ican,icolmax)%lai  = twostr%scelg(ican,icolmax)%lai / area_ratio
                  twostr%scelg(ican,icolmax)%sai  = twostr%scelg(ican,icolmax)%sai / area_ratio
               end if

            end if if_overfull

         end do

         twostr%n_col(1:patch%ncl_p) = n_col(1:patch%ncl_p)

         ! Set up some non-element parameters
         ! -------------------------------------------------------------------------------------------

         twostr%n_lyr = patch%ncl_p   ! Number of layers

         call twostr%GetNSCel()       ! Total number of elements

         max_elements = max(max_elements,twostr%n_scel)
         
         twostr%force_prep = .true.   ! This signals that two-stream scattering coefficients
         
       end associate

       end if if_notbareground
       
       patch => patch%younger
    end do

    ! Re-evaluate the scratch space used for solving two-stream radiation
    ! The scratch space needs to be 2x the number of computational elements
    ! for the patch with the most elements.
    
    if(allocated(site%taulambda_2str)) then
       n_scr = ubound(site%taulambda_2str,dim=1)
       allocate_scratch = .false.
       if(2*max_elements > n_scr) then
          allocate_scratch = .true.
          deallocate(site%taulambda_2str,site%ipiv_2str,site%omega_2str)
       elseif(2*max_elements < (n_scr-24)) then
          allocate_scratch = .true.
          deallocate(site%taulambda_2str,site%ipiv_2str,site%omega_2str)
       end if
    else
       allocate_scratch = .true.
    end if

    if(allocate_scratch)then
       ! Twice as many spaces as there are elements, plus some
       ! extra to prevent allocating/deallocating on the next step
       n_scr = 2*max_elements+8
       allocate(site%taulambda_2str(n_scr))
       allocate(site%omega_2str(n_scr,n_scr))
       allocate(site%ipiv_2str(n_scr))
    end if
    
    return
  end subroutine FatesConstructRadElements

  ! =============================================================================================
  
  subroutine FatesPatchFSun(site,patch,fsun,laisun,laisha)

    type(ed_site_type) :: site
    type(fates_patch_type) :: patch
    type(fates_patch_type), pointer :: fpatch
    real(r8)            :: fsun    ! Patch average sunlit fraction
    real(r8)            :: laisun  ! Patch average LAI of leaves in sun
    real(r8)            :: laisha  ! Patch average LAI of leaves in shade

    integer :: ican, icol  ! Canopy vertical and horizontal element index
    logical :: call_fail
    
    ! Dummy variables
    real(r8)            :: Rb_abs,Rd_abs,Rd_abs_leaf,Rb_abs_leaf,R_abs_stem,R_abs_snow

    real(r8)            :: leaf_sun_frac  ! Element specific sunlit fraction of leaf
    real(r8)            :: in_fab
    
    laisun = 0._r8
    laisha = 0._r8

    associate(twostr => patch%twostr)
    
      do ican = 1,twostr%n_lyr
         do icol = 1,twostr%n_col(ican)

            associate(scelg => patch%twostr%scelg(ican,icol))
            
              call twostr%GetAbsRad(ican,icol,ivis,0._r8,scelg%lai+scelg%sai, &
                   Rb_abs,Rd_abs,Rd_abs_leaf,Rb_abs_leaf,R_abs_stem, &
                   R_abs_snow,leaf_sun_frac,call_fail)
              
              if(call_fail) then
                 write(fates_log(),*) 'patch failure:',patch%patchno,' of:'
                 fpatch => site%oldest_patch
                 do while (associated(fpatch))
                    write(fates_log(),*) fpatch%patchno
                    fpatch => fpatch%younger
                 end do
                 call twostr%Dump(ivis,lat=site%lat,lon=site%lon)
                 call endrun(msg=errMsg(sourcefile, __LINE__))
              end if
              
              laisun = laisun + scelg%area*scelg%lai*leaf_sun_frac
              laisha = laisha + scelg%area*scelg%lai*(1._r8-leaf_sun_frac)
            end associate
         end do
      end do

      if((laisun+laisha)>nearzero)then
         fsun = laisun / (laisun+laisha)
      else
         fsun = 0.5_r8  ! Nominal value, should not affect results if no leaves or light!
      end if
         
    end associate
    return
  end subroutine FatesPatchFSun

  ! ============================================================================================
  
  subroutine CheckPatchRadiationBalance(patch, snow_depth, ib, fabd, fabi)

    ! Loop through the cohorts in the patch, get the
    ! absorbed radiation, then compare the amount absorbed
    ! to the fraction the solver calculated

    type(fates_patch_type) :: patch
    integer             :: ib      ! broadband index
    real(r8)            :: snow_depth
    real(r8)            :: fabd    ! Fraction of absorbed direct radiation by vegetation
    real(r8)            :: fabi    ! Fraction of absorbed indirect radiation by vegetation
    
    type(fates_cohort_type), pointer :: cohort
    integer :: iv,ican,icol
    real(r8),dimension(50) :: cohort_vaitop
    real(r8),dimension(50) :: cohort_vaibot
    real(r8),dimension(50) :: cohort_layer_elai
    real(r8),dimension(50) :: cohort_layer_esai
    real(r8)               :: cohort_elai
    real(r8)               :: cohort_esai
    real(r8) :: rb_abs,rd_abs,rb_abs_leaf,rd_abs_leaf,leaf_sun_frac,check_fab,in_fab
    logical :: call_fail
    
    associate(twostr => patch%twostr)

      check_fab = 0._r8
      
      cohort => patch%tallest
      do while (associated(cohort))
         
         do iv = 1,cohort%nv
            call VegAreaLayer(cohort%treelai, &
                 cohort%treesai,              &
                 cohort%height,                 &
                 iv,                                 &
                 cohort%nv,                   &
                 cohort%pft,                  &
                 snow_depth,                &
                 cohort_vaitop(iv),                  &
                 cohort_vaibot(iv),                  & 
                 cohort_layer_elai(iv),              &
                 cohort_layer_esai(iv))
         end do

         cohort_elai = sum(cohort_layer_elai(1:cohort%nv))
         cohort_esai = sum(cohort_layer_esai(1:cohort%nv))
         
         do iv = 1,cohort%nv

            ican = cohort%canopy_layer
            icol = cohort%twostr_col
            
            call FatesGetCohortAbsRad(patch,cohort,ib,cohort_vaitop(iv),cohort_vaibot(iv), &
                 cohort_elai,cohort_esai,rb_abs,rd_abs,rb_abs_leaf,rd_abs_leaf,leaf_sun_frac)
            
            check_fab = check_fab + (Rb_abs+Rd_abs) * cohort%c_area/patch%total_canopy_area
            
         end do
         cohort => cohort%shorter
      enddo
      
      in_fab = fabd*twostr%band(ib)%Rbeam_atm +  fabi*twostr%band(ib)%Rdiff_atm

      if( abs(check_fab-in_fab) > in_fab*10._r8*rel_err_thresh ) then
         write(fates_log(),*)'Absorbed radiation didnt balance after cohort sum'
         write(fates_log(),*) ib,in_fab,check_fab,snow_depth
         ! Remove the comment below for more information about the failure
         ! We keep it commented for now because the output is significant
         !call twostr%Dump(ib,patch%site%coszen)
         call endrun(msg=errMsg(sourcefile, __LINE__))
      end if
      
      
    end associate
    
    return
  end subroutine CheckPatchRadiationBalance
  
  ! =============================================================================================
  
  subroutine FatesGetCohortAbsRad(patch,cohort,ib,vaitop,vaibot,cohort_elai,cohort_esai, &
       rb_abs,rd_abs,rb_abs_leaf,rd_abs_leaf,leaf_sun_frac )

    ! This subroutine retrieves the absorbed radiation on
    ! leaves and stems, as well as the leaf sunlit fraction
    ! over a specified interval of VAI (vegetation area index)
    ! VAI is exposed leaf + stem area index

    type(fates_patch_type)  :: patch
    type(fates_cohort_type) :: cohort
    integer,intent(in)   :: ib
    real(r8),intent(in)  :: vaitop
    real(r8),intent(in)  :: vaibot
    real(r8),intent(in)  :: cohort_elai
    real(r8),intent(in)  :: cohort_esai
    real(r8),intent(out) :: rb_abs
    real(r8),intent(out) :: rd_abs
    real(r8),intent(out) :: rb_abs_leaf
    real(r8),intent(out) :: rd_abs_leaf
    real(r8),intent(out) :: leaf_sun_frac

    real(r8) :: rd_abs_el,rb_abs_el
    real(r8) :: vai_top_el
    real(r8) :: vai_bot_el
    real(r8) :: rd_abs_leaf_el
    real(r8) :: rb_abs_leaf_el
    real(r8) :: r_abs_stem_el
    real(r8) :: r_abs_snow_el
    real(r8) :: diff_wt_leaf,diff_wt_elem
    real(r8) :: beam_wt_leaf,beam_wt_elem
    real(r8) :: evai_cvai  ! element VAI / cohort VAI
    logical  :: call_fail
    
    associate(scelg => patch%twostr%scelg(cohort%canopy_layer,cohort%twostr_col), &
         scelb => patch%twostr%band(ib)%scelb(cohort%canopy_layer,cohort%twostr_col) )

      if((cohort_elai+cohort_esai)<nearzero)then
          rb_abs  = 0._r8
          rd_abs  = 0._r8
          rb_abs_leaf = 0._r8
          rd_abs_leaf = 0._r8
          leaf_sun_frac = 0._r8
         return
      end if

      evai_cvai = (scelg%lai+scelg%sai)/(cohort_elai+cohort_esai)
      
      ! Convert the vai coordinate from the cohort to the element
      vai_top_el = vaitop * evai_cvai
      vai_bot_el = vaibot * evai_cvai

      ! Return the absorbed radiation for the element over that band
      call patch%twostr%GetAbsRad(cohort%canopy_layer,cohort%twostr_col,ib,vai_top_el,vai_bot_el, & 
           Rb_abs_el,Rd_abs_el,rd_abs_leaf_el,rb_abs_leaf_el,r_abs_stem_el,r_abs_snow_el,leaf_sun_frac,call_fail)

      ! Note that rd_abs_el and rb_abs_el both contain absorption by water, the abs_leaf terms do not
      rd_abs = rd_abs_el / evai_cvai
      rb_abs = rb_abs_el / evai_cvai
      
      diff_wt_leaf = (1._r8-patch%twostr%frac_snow)*cohort_elai*(1._r8-rad_params%om_leaf(ib,cohort%pft))*rad_params%Kd_leaf(cohort%pft)
      diff_wt_elem = (cohort_elai+cohort_esai)*(1._r8-scelb%om)*scelg%Kd

      beam_wt_leaf = (1._r8-patch%twostr%frac_snow)*cohort_elai*(1._r8-rad_params%om_leaf(ib,cohort%pft))*scelg%Kb_leaf
      beam_wt_elem = (cohort_elai+cohort_esai)*(1._r8-scelb%om)*scelg%Kb

      rd_abs_leaf = rd_abs * min(1.0_r8,diff_wt_leaf / diff_wt_elem)
      rb_abs_leaf = rb_abs * min(1.0_r8,beam_wt_leaf / beam_wt_elem)

      
    end associate
  end subroutine FatesGetCohortAbsRad

  ! =============================================================================================

  subroutine TransferRadParams()

    integer :: ft,ib  ! loop indices

    call AllocateRadParams(numpft,num_swb)

    do ft = 1,numpft
       do ib = 1,num_swb

          rad_params%rhol(ib,ft) = EDPftvarcon_inst%rhol(ft,ib)
          rad_params%rhos(ib,ft) = EDPftvarcon_inst%rhos(ft,ib)
          rad_params%taul(ib,ft) = EDPftvarcon_inst%taul(ft,ib)
          rad_params%taus(ib,ft) = EDPftvarcon_inst%taus(ft,ib)

       end do
       rad_params%xl(ft) = EDPftvarcon_inst%xl(ft)
       rad_params%clumping_index(ft) = EDPftvarcon_inst%clumping_index(ft)
    end do

    call RadParamPrep()

    return
  end subroutine TransferRadParams


end Module FatesTwoStreamUtilsMod
