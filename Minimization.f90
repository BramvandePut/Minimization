
module SteepestDescent
! This module will contain the subroutines to perform minimization using the Conjugate Steepest Descent Method
  use GradientCalculator
  
  implicit none
  private
  public Steep

contains

  subroutine Steep
	real(8)					:: convLim
    real(8), pointer		:: gradient(:), gradientOld(:), xi(:), xiOld(:), gamma(:)
	integer					:: iterations, i
    
	integer					:: order
    order = 2
	
	allocate(xi(order))
	allocate(xiOld(order))
	allocate(gradient(order)) 
	allocate(gradientOld(order))
	allocate(gamma(order))	

	xi = (/ -1.0, -3.1415 /)  	
	convLim = 0.000001
	gamma = 0.1
	iterations = 0
	xiOld = xi + 1
	
	do while (abs(sum(xi - xiOld)) > convLim .and. iterations < 10000)
	  
	  gradientOld = gradient
	  call CalculateGradient(xi, gradient)
	  
	  if (iterations > 1) then
	    gamma = (dot_product((xi-xiOld) , (gradient-gradientOld))) / sqrt(sum((gradient-gradientOld)**2))**2
	  endif
	  
	  xiOld = xi
	  xi = xiOld - (gamma * gradient)
	  
	  iterations = iterations + 1
	  
	  print *, gradient
	  print *, gamma
    enddo
	
	print *, '# of iterations until convergence = ', iterations
	print *, 'xi = ', xi

  end subroutine

end module




module ConjugateGradient
! This module will contain the subroutines to perform minimization using the Conjugate Gradient Method
  implicit none
  private
!  public
end module




module ConfigHandler
! This module will read form standard input or a file to assess what the program will do
  implicit none
  private
  save
  public ConfigType, Initialize

  type ConfigType
    integer							:: order
    real(8)							:: h, convergeLimit
	real(8), allocatable			:: x1(:)
    character(10)					:: mode, algorithm, input
  end type
  
  contains
  
  subroutine Initialize(self)
    type(ConfigType), intent(out)		:: self
	! integer								:: order, maxIterations
    ! real(8)								:: h, convergeLimit
	! real(8), allocatable				:: x1(:)
    ! character(10)						:: mode, algorithm, input
	
	open(7, file = 'config.txt')
	
	read(7,*) self%mode
	read(7,*) self%algorithm
	read(7,*) self%order
	allocate(self%x1(self%order))
	read(7,*) self%x1
	read(7,*) self%convergeLimit
	read(7,*) self%input
	
	! call SetMode(self, mode)
	! call SetAlgorithm(self, algorithm)
	! call SetOrder(self, order)
	! call SetConvergeLimit(self, convergeLimit)
	
	call UserInterface(self)
	
  end subroutine

  subroutine SetMode(self, mode)
	type(ConfigType), intent(out)		:: self
	character(10)						:: mode
	
	self%mode = mode
  end subroutine
  
  subroutine SetAlgorithm(self, algorithm)
	type(ConfigType), intent(out)		:: self
	character(10)						:: algorithm
	
	self%algorithm = algorithm
  end subroutine
  
  subroutine SetOrder(self, order)
    type(ConfigType), intent(out)		:: self
	integer								:: order
	
	self%order = order
  end subroutine
  
  subroutine SetConvergeLimit(self, convergeLimit)
	type(ConfigType), intent(out)		:: self
    real(8)								:: convergeLimit
	
	self%convergeLimit = convergeLimit
  end subroutine
  
  subroutine UserInterface(self)
    type(ConfigType), intent(out)		:: self
	character(10)						:: inputChar
	real(8)								:: inputReal
	
	do
	  print *, 'Would you like to configure Minimizer? (y/n)'
	  read *, inputChar
	
	  if (inputChar == 'y') then
	    call UserConfig(self)
	    return
	  elseif (inputChar == 'n') then
	    return
	  else
	    print *, 'Input y or n'
	    cycle
	  endif
	enddo
  end subroutine

  subroutine UserConfig(self)
	type(ConfigType), intent(out)		:: self
	character(10)						:: inputChar
	real(8)								:: inputReal
	
	do
	  print *, ''
	  print *, 'Welcome to minimizer configuration,'
	  print *, 'Enter the parameter you would like to configure:'
	  print *, '"mode" "algorithm" "order" "convergeLimit"'
	
	  read *, inputChar
	  
	  if(inputChar == 'mode') then
	    print *, 'enter the new value'
		read *, inputChar
		
	  elseif(inputChar == 'algorithm') then
	    print *, 'enter the new value'
		read *, inputChar
	  
	  else
	  
	  print *, 'That was not a valid argument'
	  
	  endif
	  
	  print *, 'Would you like to edit anything else? (y/n)'
	    read *, inputChar
		if(inputChar == 'y') then
		  cycle
		elseif(inputChar == 'n')then
   		  return
		endif
	enddo
  end subroutine

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

    y = (x(1)**2)+(2*x(1))+(cos(x(2)))
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
use ConfigHandler
use OutputWriter
use Logger
use Testing
use GradientCalculator
use Matfunction

implicit none

  type(ConfigType)		:: config
  
  call Steep
 ! call Initialize(config)
  
  ! real(8), pointer		:: gradient(:), input(:)
  ! integer				:: order
  ! order = 2
  ! allocate(input(order))
  ! input = (/ 1, 2 /)  
  ! allocate(gradient(order)) 
  ! call CalculateGradient(input, gradient)
  ! print *, gradient

end Program



