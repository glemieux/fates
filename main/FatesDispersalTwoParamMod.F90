module FatesDispersalTwoParamMod

   use FatesDispersalBaseMod, only : dispersal_kernel_type
   use FatesGlobals         , only : endrun => fates_endrun
   use FatesConstantsMod    , only : pi_const

   implicit none
   private
   
   ! One parameter dispersal kernel type
   type, extends(dispersal_kernel_type) :: dispersal_two_param_type
      private
      real(r8) :: exp_param_a
      real(r8) :: exp_param_b
      contains
         procedure, public :: ProbabilityDensity
   end type dispersal_two_param_type

   ! ! Two parameter dispersal kernel type
   ! type, extends(dispersal_kernel_type) :: dispersal_two_param_type
   ! end type dispersal_two_param_type

   public :: dispersal_two_param_type

   character(len=*), parameter, private :: sourcefile = __FILE__

contains
   
   ! ======================================================================================

   function ProbabilityDensity_ExponentialPower(this, g2g_dist) result(pdf)
            
      ! Arguments
      class(dispersal_two_param_type) :: this
      real(r8), intent(in) :: g2g_dist
      real(r8)             :: pdf
      
      ! Assuming simple exponential decay.  In the future perhaps this could be an interface
      ! for different weight calculations (and could be held only in fates)
      
      pdf = (this%exp_param_b / (2*pi_const*gamma(2/this%exp_param_b))) * &
            exp(-(g2g_dist**this%exp_param_b)/(this%exp_param_a**this%exp_param_b))

   end function ProbabilityDensity_ExponentialPower

   ! ======================================================================================

   function ProbabilityDensity_Logsech(this, g2g_dist) result(pdf)
            
      ! Arguments
      class(dispersal_two_param_type) :: this
      real(r8), intent(in) :: g2g_dist
      real(r8)             :: pdf
      
      ! Assuming simple exponential decay.  In the future perhaps this could be an interface
      ! for different weight calculations (and could be held only in fates)
      
      pdf = (1/(pi_const**2 * this%exp_param_b * g2g_dist**2)) / &
            ((g2g_dist/this%exp_param_a)**(1/this%exp_param_b) + (g2g_dist/this%exp_param_a)**(-1/this%exp_param_b))

   end function ProbabilityDensity_Logsech

   ! ======================================================================================

   ! Initialization routine to get the parameters   

   ! ======================================================================================


end module FatesDispersalTwoParamMod