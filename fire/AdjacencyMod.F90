module AdjacencyMod
   
   implicit none
   private
   
   public :: AdjacencyMatrix

   logical, parameter :: debug = .false.  ! uncomment for additional output
   
   contains
   
      
   subroutine Geometric(a,r,n,g,len)
      double precision, intent(in) :: a,r                   ! initial value, rate
      integer, intent(in) :: n                              ! length of array
      integer, intent(in), optional :: len                  ! length of series, could be smaller than input array
      double precision, intent(inout) :: g(0:n-1)           ! input array
      
      ! Local
      integer :: i,d
      
      ! Initialize the maximum iteration as the max index
      d = n-1
      
      ! If len is present use that instead
      if (present(len)) then
         if (len < n) then
            d = len-1
         else
            if (len > n) then
               print *, 'len provided is too large.  Using n'
            end if
         end if
      end if
      
      if (debug) print *, 'd: ', d 
      
      do i =0,d
         g(i) = a*(r**i);
      end do
      
   end subroutine Geometric
   
   subroutine UnitDecay(a,n,g,len)
      double precision, intent(in) :: a               ! initial value
      integer, intent(in) :: n                        ! length of input array
      integer, intent(in), optional :: len            ! length of series, could be smaller than input array
      double precision, intent(inout) :: g(0:n-1)     ! input array
      
      if (present(len)) then
         call Geometric(a,1.0-a,n,g,len)
      else
         call Geometric(a,1.0-a,n,g)
      end if
            
   end subroutine UnitDecay
   
   subroutine AdjacencyMatrix(a00,r,d,A)
         
      double precision, intent(in)    :: a00          ! zero age class self-adjacency, i.e A(0,0)
      double precision, intent(in)    :: r            ! rate of decay for diagonals
      integer, intent(in)             :: d            ! matrix dimension, output is square
      double precision, intent(inout) :: A(0:d-1,0:d-1)   ! adjacency matrix
   
      ! Local
      double precision :: a0(0:d-1)
      double precision :: temp(0:d-1)
      integer          :: i,j
     
      A(:,:) = 0.0
     
      ! Define the ageclass zero adjacencies row vector to follow a unit decay series, i.e. sums to 1
      call unitdecay(a00,d,a0)
      a0(d-1) = a0(d-1) + (1.0-sum(a0))
      
      if (debug) print *, 'a0: ', a0(0:d-1)
      if (debug) print *, 'sum of a0 check: ', 1.0-sum(a0) == 0.0
      
      ! To Do: The following do loops could be refactored into generic matrix functions of
      ! possibly replaced by an existing fortran library
      
      ! Construct the upper diagonal matrix using a geometric decay of 'r'
      upper: do i = 0,d-1

         ! Generate the off-diagonal based on the first row entries
         call Geometric(a0(i),r,d,temp,d-i)
         if (debug) print *, 'temp up: ', temp(0:d-1)
         if (debug) print *, 'd-i: ', d-i

         ! Fill in the adjacency matrix upper-diagonal using entries from temp
         do j = 0,d-i-1
            A(j,j+i) = temp(j)  ! this is necessary since temp is fixed length
            if (debug) print *, 'i, j,i+j: ', i,j,i+j
         end do
         
      end do upper
    
      if (debug) print *, 'upper done'
    
      ! Fill in the lower diagonal matrix one diagonal at a time
      lower: do i = 1,d-1
         
         if (debug) print *, 'first entry lower diag: ', 1.0-sum(A(i,:))
      
         ! Generate the off-diagonal based on the first row entries
         call Geometric(1.0-sum(A(i,:)),r,d,temp,d-i)
         
         if (debug) print *, 'temp lower: ', temp(0:d-1)
         if (debug) print *, 'd-i: ', d-i
         
         ! Fill in the adjacency matrix lower-diagonal using entries from temp
         do j = 0,d-i-1
            A(j+i,j) = temp(j)
            if (debug) print *, 'i, j, i+j: ', i,j,i+j
         end do
        
      end do lower
      
      if (debug) print *, 'lower done'
          
   end subroutine AdjacencyMatrix
      

end module AdjacencyMod