module FatesDispersalOneParamMod

   use FatesDispersalBaseMod, only : dispersal_kernel_type
   use FatesGlobals         , only : endrun => fates_endrun

   implicit none
   private
   
   ! One parameter dispersal kernel type
   type, extends(dispersal_kernel_type) :: dispersal_one_param_type
      private
      real(r8) :: exp_param
      contains
         procedure, public :: ProbabilityDensity
   end type dispersal_one_param_type

   public :: dispersal_one_param_type

   character(len=*), parameter, private :: sourcefile = __FILE__

contains
   
   ! ======================================================================================

   function ProbabilityDensity(this, g2g_dist) result(pdf)

      ! Assuming simple exponential decay.  In the future perhaps this could be an interface
      ! for different weight calculations (and could be held only in fates)

      ! ARGUMENTS
      class(dispersal_one_param_type) :: this
      real(r8), intent(in) :: g2g_dist
      real(r8)             :: pdf
    
      pdf = exp(-this%exp_param*g2g_dist)

   end function ProbabilityDensity

   ! ======================================================================================

   ! Initialization routine to get pdf parameters

   ! subroutine Init()
   !    type1, intent(in) :: arg1
   !    type2, intent(out) ::  arg2
         
   ! end subroutine Init

   ! ======================================================================================


end module FatesDispersalOneParamMod