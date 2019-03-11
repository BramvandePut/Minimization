
module SteepestDescent
! This module will contain the subroutines to perform minimization using the Conjugate Steepest Descent Method
  use GradientCalculator
  
  implicit none
  private
!  public

contains

  subroutine Steep
	real(8)					:: convLim, xi, xiOld, v
    real(8), pointer		:: gradient(:)
	integer					:: iterations
  
    iterations = 1
  
	do while (abs(xi - xiOld) > convLim .or. iterations > 10000)
	
  
    enddo
	
  end subroutine

end module




module ConjugateGradient
! This module will contain the subroutines to perform minimization using the Conjugate Gradient Method
  implicit none
  private
!  public
end module




module InputReader
! This module will read form standard input or a file to assess what the program will do
  implicit none
  private
!  public

!  type(Formula)
!   integer			:: order
!	real(8)			:: h, x1(order), convergeLimit
!   character(40)	:: function, algorithm, mode, input
!  end type
 
end module




module OutputWriter
! This module will contain the subroutines to write output to standard output or a file as defined
  implicit none
  private
!  public
end module




module Logger
  implicit none
  private
!  public 
end module




module Testing
! This module will contain the subroutines used to perform tests on the program
  implicit none
  private
!  Public
end module




module MatFunction
! This module contains the mathematical function (Formula) which is to be minimized	
  implicit none
  private
  public Formula

  integer, parameter					:: order = 2

contains
  function Formula(x) result(y)
    real(8), intent(in), pointer		:: x(:)
    real(8), allocatable				:: y(:)

    allocate(y(size(x)))

    y = x(1)**2+x(2)**3
  end function

end module




module GradientCalculator
! This module contains the subroutine which is used to calculate the gradient for the given values of x
  use MatFunction
  implicit none
  private
  public CalculateGradient

contains

  subroutine CalculateGradient(Input, gradient)
    real(8), intent(in), pointer		:: input(:)
    real(8), intent(out), pointer		:: gradient(:)
	real(8), parameter					:: h = 0.0000001
    real(8), pointer					:: f(:,:), x(:)
    integer	 							:: order, i, j

    order = size(input)
    allocate(gradient(order))
	allocate(f(2,order))
	allocate(x(order))


    do i = 1, order
	  x = input
      x(i) = x(i) + h
      f(1,:) = Formula(x)
	  x(i) = x(i) - 2*h
      f(2,:) = Formula(x)
	 
	  gradient(i) = (f(1,i) - f(2,i)) / (2 * h)
	enddo
	
  end subroutine
  
end module




Program Minimization
! This is the main program from which the subroutines in the program modules will be called
use SteepestDescent
use ConjugateGradient
use InputReader
use OutputWriter
use Logger
use Testing
use GradientCalculator
use Matfunction

implicit none

  ! real(8), pointer		:: gradient(:), input(:)
  ! integer				:: order
  ! order = 2
  ! allocate(input(order))
  ! input = (/ 1, 2 /)  
  ! allocate(gradient(order)) 
  ! call CalculateGradient(input, gradient)
  ! print *, gradient

end Program



