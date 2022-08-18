module FatesDispersalBaseMod

   use FatesInterfaceTypesMod, only : neighbor_type
   use FatesGlobals          , only : endrun => fates_endrun

   implicit none
   private
   
   ! Dispersal kernal type and associated abstract interface for pdf function
   type, abstract, extends(neighbor_type) :: dispersal_kernel_type
      private
      contains
         procedure(ProbabilityDensity_interface), public, deferred :: ProbabilityDensity
   end type dispersal_kernel_type

   abstract interface

      function ProbabilityDensity_interface(this) result (pdf)
         import :: dispersal_kernel_type   ! Why do we need import here?
         class(dispersal_kernel_type) :: this
      end function ProbabilityDensity_interface

   end interface

   ! One parameter dispersal kernel type
   type, extends(dispersal_kernel_type) :: dispersal_one_param_type
   end type dispersal_one_param_type

   ! Two parameter dispersal kernel type
   type, extends(dispersal_kernel_type) :: dispersal_two_param_type
   end type dispersal_two_param_type

   procedure, public :: CreateDispersalKernelMethod

   character(len=*), parameter, private :: sourcefile = __FILE__

contains
   
   ! ======================================================================================

   subroutine CreateDispersalKernelMethod(dispersal_kernel_method)
      
      ! This procedure constructs the dispersal kernel method based on the case selected
      ! by the user
      
      ! USES
      use FatesInterfaceTypesMod, only : hlm_dispersal_kernel_exponential, &
                                         hlm_dispersal_kernel_exppower, &
                                         hlm_dispersal_kernel_logsech, &
                                         hlm_dispersal_kernel_mode

      ! ARGUMENTS
      class(dispersal_kernel_type), allocatable, intent(inout) :: dispersal_kernel_method

      ! LOCAL

      select case(hlm_dispersal_kernel_mode)

      ! For simple one parameter cases
      case(hlm_dispersal_kernel_exponential)
         allocate(dispersal_one_param_type :: dispersal_one_param_type)
      
      ! For two parameter cases
      case(hlm_dispersal_kernel_exppower:hlm_dispersal_kernel_logsech)
         allocate(dispersal_one_param_type :: dispersal_one_param_type)

      case default
         write(fates_log(),*) 'ERROR: unknown dispersal kernal method: ', hlm_dispersal_kernel_mode
         call endrun(msg=errMsg(sourcefile, __LINE__))

      end select

      
   end subroutine CreateDispersalKernelMethod

   ! ======================================================================================


end module FatesDispersalBaseMod