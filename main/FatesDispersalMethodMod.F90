module FatesDispersalMethodMod

   use shr_log_mod           , only : errMsg => shr_log_errMsg
   use FatesGlobals          , only : endrun => fates_endrun
   use FatesGlobals          , only : fates_log
   use FatesConstantsMod     , only : r8 => fates_r8

   use FatesDispersalBaseMod, only : dispersal_kernel_type
   use FatesDispersalOneParamMod, only : dispersal_one_param_type
   use FatesDispersalTwoParamMod, only : dispersal_two_param_type

   implicit none
   private
   
   character(len=*), parameter, private :: sourcefile = __FILE__

contains
   
   ! ======================================================================================

   subroutine CreateDispersalKernelMethod(dispersal_kernel_method)
      
      ! This procedure constructs the dispersal kernel method based on the case selected
      ! by the user.  The kernel methods can have 
      
      ! USES
      use FatesInterfaceTypesMod, only : hlm_dispersal_kernel_exponential, &
                                         hlm_dispersal_kernel_exppower, &
                                         hlm_dispersal_kernel_logsech, &
                                         hlm_dispersal_kernel_mode

      ! ARGUMENTS
      class(dispersal_kernel_type), allocatable, intent(inout) :: dispersal_kernel_method

      select case(hlm_dispersal_kernel_mode)

      ! For one parameter cases
      case(hlm_dispersal_kernel_exponential)
         allocate(dispersal_one_param_type :: dispersal_kernel_method)
      
      ! For two parameter cases
      case(hlm_dispersal_kernel_exppower:hlm_dispersal_kernel_logsech)
         allocate(dispersal_two_param_type :: dispersal_kernel_method)

      case default
         write(fates_log(),*) 'ERROR: unknown dispersal kernal method: ', hlm_dispersal_kernel_mode
         call endrun(msg=errMsg(sourcefile, __LINE__))

      end select

   end subroutine CreateDispersalKernelMethod

   ! ======================================================================================

end module FatesDispersalMethodMod