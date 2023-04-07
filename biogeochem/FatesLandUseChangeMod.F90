module FatesLandUseChangeMod

  ! Controls the transfer and initialization of patch structure to land use types

  use FatesGlobals         , only : fates_log
  use FatesConstantsMod    , only : primarylands, secondarylands, pasture, rangelands, crops
  use FatesConstantsMod    , only : n_landuse_cats
  use FatesGlobals         , only : endrun => fates_endrun
  use FatesConstantsMod    , only : r8 => fates_r8
  use FatesConstantsMod    , only : itrue, ifalse
  use FatesInterfaceTypesMod    , only : bc_in_type
  use FatesInterfaceTypesMod    , only : hlm_num_luh2_transitions
  use EDTypesMod           , only : area_site => area

  ! CIME globals
  use shr_infnan_mod       , only : nan => shr_infnan_nan, assignment(=)
  use shr_log_mod          , only : errMsg => shr_log_errMsg
  
  !
  implicit none
  private
  !
  public :: get_landuse_transition_rates
  public :: init_luh2_fates_mapping
  public :: get_landusechange_rules
  public :: get_luh_statedata

  ! module data
  integer :: max_luh2_types_per_fates_lu_type = 5
  CHARACTER(len = 5), protected, DIMENSION(n_landuse_cats,max_luh2_types_per_fates_lu_type) :: luh2_fates_luype_map

  ! 03/10/2023 Created By Charlie Koven
  ! ============================================================================

contains

  ! ============================================================================
  subroutine get_landuse_transition_rates(bc_in, landuse_transition_matrix)


    ! The purpose of this routine is to ingest the land use transition rate information that the host model has read in from a dataset,
    ! aggregate land use types to those being used in the simulation, and output a transition matrix that can be used to drive patch
    ! disturbance rates.

    ! !ARGUMENTS:
    type(bc_in_type) , intent(in) :: bc_in
    real(r8), intent(inout) :: landuse_transition_matrix(n_landuse_cats, n_landuse_cats)  ! [m2/m2/year]

    ! !LOCAL VARIABLES:
    integer :: i_donor, i_receiver, i_luh2_transitions, i_luh2_states
    character(5) :: donor_name, receiver_name
    character(14) :: transition_name
    real(r8) :: urban_fraction

    ! zero the transition matrix
    landuse_transition_matrix(:,:) = 0._r8

    use_luh_if: if ( hlm_use_luh .eq. itrue ) then
       
       !!may need some logic here to ask whether or not ot perform land use cahnge on this timestep. current code occurs every day.
       
       ! identify urban fraction so that it can be removed.
       urban_fraction = 0._r8
       do i_luh2_states = 1, hlm_num_luh2_states
          if (bc_in%hlm_luh_state_names(i_luh2_states) .eq. 'urban') then
             urban_fraction = bc_in%hlm_luh_states(i_luh2_states)
          end if
       end do
    
       ! loop over FATES donor and receiver land use types
       donor_loop: do i_donor = 1,n_landuse_cats
          receiver_loop: do i_receiver = 1,n_landuse_cats

             ! ignore diagonals of transition matrix
             not_diagonal: if ( i_donor .ne. i_receiver ) then

                ! ignore special case of primary -> secondary, which is handled by harvest mechanism
                not_primary_to_secondary: if ( .not. ((i_donor .eq. primarylands) .and. (i_receiver .eq. secondarylands)) ) then

                   transitions_loop: do i_luh2_transitions = 1, hlm_num_luh2_transitions

                      ! transition names are written in form xxxxx_to_yyyyy where x and y are donor and receiver state names
                      transition_name = bc_in%hlm_luh_transition_names(i_luh2_transitions)
                      donor_name = transition_name(1:5)
                      receiver_name = transition_name(10:14)

                      if (any(luh2_fates_luype_map(:,i_donor) == donor_name) .and. &
                           any(luh2_fates_luype_map(:,i_receiver) == receiver_name)) then

                         landuse_transition_matrix(i_donor,i_receiver) = &
                              landuse_transition_matrix(i_donor,i_receiver) +  bc_in%hlm_luh_transitions(i_luh2_transitions) / (1._r8 - urban_fraction)

                      end if
                   end do transitions_loop
                end if not_primary_to_secondary
             end if not_diagonal
          end do receiver_loop
       end do donor_loop
    end if use_luh_if
  end subroutine get_landuse_transition_rates

  !----------------------------------------------------------------------------------------------------

  subroutine init_luh2_fates_mapping

    ! initialize the character mapping of the LUH2 : FATES correspondance
    luh2_fates_luype_map(:,:) = ''
    
    luh2_fates_luype_map(1,primarylands) = 'primf'
    luh2_fates_luype_map(2,primarylands) = 'primn'

    luh2_fates_luype_map(1,secondarylands) = 'secdf'
    luh2_fates_luype_map(2,secondarylands) = 'secdn'

    luh2_fates_luype_map(1,crops) = 'c3ann'
    luh2_fates_luype_map(2,crops) = 'c4ann'
    luh2_fates_luype_map(3,crops) = 'c3per'
    luh2_fates_luype_map(4,crops) = 'c4per'
    luh2_fates_luype_map(5,crops) = 'c3nfx'

    luh2_fates_luype_map(1,pasture) = 'pastr'

    luh2_fates_luype_map(1,rangelands) = 'range'
    
  end subroutine init_luh2_fates_mapping

  !----------------------------------------------------------------------------------------------------

  subroutine get_landusechange_rules(clearing_matrix)

    ! the purpose of this is to define a ruleset for when to clear the vegetation in transitioning from one land use type to another

    logical, intent(out) :: clearing_matrix(n_landuse_cats,n_landuse_cats)
    integer, parameter    :: ruleset = 1   ! ruleset to apply from table 1 of Ma et al (2020) https://doi.org/10.5194/gmd-13-3203-2020

    ! clearing matrix applies from the donor to the receiver land use type of the newly-transferred patch area
    ! values of clearing matrix: false => do not clear; true => clear

    clearing_matrix(:,:) = .false.

    select case(ruleset)

    case(1)

       clearing_matrix(:,crops) = .true.
       clearing_matrix(:,pasture) = .true.
       clearing_matrix(pasture,rangelands) = .true.
       clearing_matrix(crops,rangelands) = .true.       

    case(2)

       clearing_matrix(:,crops) = .true.
       clearing_matrix(rangelands,pasture) = .true.
       clearing_matrix(crops,pasture) = .true.
       clearing_matrix(pasture,rangelands) = .true.
       clearing_matrix(crops,rangelands) = .true.

    case(3)

       clearing_matrix(:,crops) = .true.
       clearing_matrix(:,pasture) = .true.
       clearing_matrix(:,rangelands) = .true.

    case(4)

       clearing_matrix(:,crops) = .true.
       clearing_matrix(:,pasture) = .true.
       clearing_matrix(:,rangelands) = .false.

    case(5)

       clearing_matrix(:,crops) = .true.
       clearing_matrix(:,pasture) = .false.
       clearing_matrix(:,rangelands) = .true.

    case(6)

       clearing_matrix(:,crops) = .true.
       clearing_matrix(:,pasture) = .false.
       clearing_matrix(:,rangelands) = .false.

    case(7)

       clearing_matrix(:,crops) = .false.
       clearing_matrix(:,pasture) = .true.
       clearing_matrix(:,rangelands) = .true.

    case(8)

       clearing_matrix(:,crops) = .false.
       clearing_matrix(:,pasture) = .true.
       clearing_matrix(:,rangelands) = .false.

    case(9)

       clearing_matrix(:,crops) = .false.
       clearing_matrix(:,pasture) = .false.
       clearing_matrix(:,rangelands) = .true.

    case(default)

       write(fates_log(),*) 'unknown clearing ruleset?'
       write(fates_log(),*) 'ruleset: ', ruleset
       call endrun(msg=errMsg(sourcefile, __LINE__))

    end select

  end subroutine get_landusechange_rules

  !----------------------------------------------------------------------------------------------------

  subroutine get_luh_statedata(bc_in, state_vector)

    type(bc_in_type) , intent(in) :: bc_in
    real(r8), intent(out) :: state_vector(n_landuse_cats)  ! [m2/m2]
    real(r8) :: urban_fraction
    integer  :: i_luh2_states
    integer  :: ii

    ! zero state vector
    state_vector(:) = 0._r8

    ! identify urban fraction so that it can be removed.
    urban_fraction = 0._r8
    do i_luh2_states = 1, hlm_num_luh2_states
       if (bc_in%hlm_luh_state_names(i_luh2_states) .eq. 'urban') then
          urban_fraction = bc_in%hlm_luh_states(i_luh2_states)
       end if
    end do

    ! loop over all states and add up the ones that correspond to a given fates land use type
    do i_luh2_states = 1, hlm_num_luh2_states
       state_name = bc_in%hlm_luh_state_names(i_luh2_states)
       do ii = 1, max_luh2_types_per_fates_lu_type
          if (state_name .eq. luh2_fates_luype_map(i_luh2_states, ii)) then
             state_vector(i_luh2_states) = state_vector(i_luh2_states) + &
                  bc_in%hlm_luh_states(i_luh2_states) / (1._r8 - urban_fraction)
          end if
       end do
    end do

    ! check to ensure total area == 1, and correct if not
    if ( abs(sum(state_vector(:)) - 1._r8) .gt. nearzero ) then
       write(fates_log(),*) 'warning: sum(state_vector) = ', sum(state_vector(:))
       do ii = 1, n_landuse_cats
          state_vector(ii) = state_vector(ii) / sum(state_vector(:))
       end do
    end if

  end subroutine get_luh_statedata

end module FatesLandUseChangeMod