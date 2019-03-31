
module ConfigHandler
! This module will read form standard input or a file to assess what the program will do
  implicit none
  private
  save
  public ConfigType, Initialize

  type ConfigType
    integer                         :: order, iterationmax
    real(8)                         :: h, convergeLimit, plotDomain, plotStep
    real(8), allocatable            :: x1(:)
    character(10)                   :: mode, algorithm, direction, list, plot
  end type
  
  contains
  
  subroutine Initialize(self)
    type(ConfigType), intent(inout)     :: self
    integer                             :: U
    
    U = 7

    open(U, file = 'config.txt')
    
    read(U,*) self%mode
    read(U,*) self%algorithm
    read(U,*) self%direction
    read(U,*) self%plot
    read(U,*) self%plotDomain
    read(U,*) self%plotStep
    read(U,*) self%list 
    read(U,*) self%order
    allocate(self%x1(self%order))
    read(U,*) self%x1
    read(U,*) self%h
    read(U,*) self%convergeLimit
    read(U,*) self%iterationmax

    close(U)

    call UserInterface(self)
    
  end subroutine
  
  
  subroutine UserInterface(self)
    type(ConfigType), intent(inout)     :: self
    character(10)                       :: inputChar
    real(8)                             :: inputReal
    
    do
      print *, 'Would you like to configure Minimizer? (y/n)'
      read *, inputChar
    
      if (inputChar == 'y') then
        call UserConfig(self)
        return
      elseif (inputChar == 'n') then
        return
      else
        print *, 'Input "y" or "n"'
        cycle
      endif
    enddo
  end subroutine

  subroutine UserConfig(self)
    type(ConfigType), intent(inout)     :: self
    character(20)                       :: inputChar
    real(8)                             :: inputReal
    integer                             :: inputInt, i, U
    
    U = 7
    
    do
      print *, ''
      print *, 'Welcome to minimizer configuration,'
      print *, 'Enter the parameter you would like to configure:'
      
      if (self%mode == 'rastrigin')then
        print *, '"mode" "algorithm" "direction" "plot" "list" "order" "h" "x1" "convergelimit" "iterationmax"'
      else
        print *, '"mode" "algorithm" "direction" "plot" "list" "order" "h" "convergelimit" "iterationmax"'
      endif
      
      read *, inputChar
      
      if (inputChar == 'mode')then
        print *, 'Enter: "default" for default mode or "rastrigin" for rastrigin surface mode'
        read *, inputChar
        
        if (inputChar == 'default')then 
          self%mode = inputChar
        elseif (inputChar == 'rastrigin')then
          self%mode = inputChar
          self%x1 = 0
        else
          print *, 'invalid argument, no new value set'
        endif

        
      elseif (inputChar == 'algorithm')then
        print *, 'Choose the minimization algorithm: "steep" for SteepestDescent or "conjugate" for ConjugateGradient'
        read *, inputChar
        if (inputChar == 'steep' .or. inputChar == 'conjugate')then
          self%algorithm = inputChar
        else
          print *, 'Invalid argument, no new value set'
        endif


      elseif (inputChar == 'direction')then
        print *, 'Choose the optimiziation direction: "max" for maximization or "min" for minimization'
        read *, inputChar
        if (inputChar == 'min' .or. inputChar == 'max')then
          self%direction = inputChar
        else
          print *, 'Invalid argument, no new value set'
        endif

      
      elseif (inputchar == 'plot')then
        print *, 'Toggle plot: "on" or "off". plots are only available for functions of order 2'
        read *, inputChar
        if (inputChar == 'on')then
          self%plot = inputChar
          print *, 'input a real number for the domain of the plot, the domain is square'
          read *, inputReal
          self%plotDomain = inputReal
          print *, 'input a real number for the step size of the plot'
          read *, inputReal
          self%plotStep = inputReal
        elseif (inputChar == 'off')then
          self%plot = inputChar
        else
          print *, 'Invalid argument, no new value set'
        endif
      
      
      elseif (inputChar == 'list')then
        print *, 'Toggle listing all iterations: "on" or "off".'
        read *, inputChar
        if (inputChar == 'on' .or. inputChar == 'off') then
          self%list = inputChar
        else
          print *, 'Invalid argument, no new value set'
        endif
      
      
      elseif (inputChar == 'order')then
        print *, 'enter an integer value to assign the order of the function'
        read *, inputInt
        self%order = inputInt
        if (self%mode == 'rastrigin')then
          deallocate(self%x1)
          allocate(self%x1(self%order))
          self%x1 = 0
        endif
        
      elseif (inputChar == 'h')then
        print *, 'enter a low real value for h'
        read *, inputReal
        self%h = inputReal
        
        
      elseif (inputChar == 'x1' .and. self%mode /= 'rastrigin')then
        deallocate(self%x1)
        allocate(self%x1(self%order))
        do i = 1, self%order
          print *, 'enter a starting value for coefficient:', i
          read *, inputReal
          self%x1(i) = inputReal
        enddo
      
      
      elseif (inputChar == 'convergelimit')then
        print *, 'enter a low real value for the convergence limit'
        read *, inputReal
        self%convergeLimit = inputReal


      elseif (inputChar == 'iterationmax') then
        print *, 'enter an integer value for the maximum number of iterations'
        read *, inputInt
        self%iterationmax = inputInt

        
      else
        print *, 'Invalid argument'
      
      endif
      
      print *, 'Would you like to edit anything else? (y/n)'
        read *, inputChar
        if(inputChar == 'y')then
          cycle
        elseif(inputChar == 'n')then
          if(self%order /= size(self%x1))then
            print *, 'The order of the function and the number of x values are not equal, please reconfigure'
            print *, self%x1, self%order
            cycle
          endif

          open(U, file = 'config.txt', status = 'replace')
    
          write(U,*) self%mode
          write(U,*) self%algorithm
          write(U,*) self%direction
          write(U,*) self%plot
          write(U,*) self%plotDomain
          write(U,*) self%plotStep
          write(U,*) self%list
          write(U,*) self%order
          write(U,*) self%x1
          write(U,*) self%h
          write(U,*) self%convergeLimit
          write(U,*) self%iterationmax
    
          close(U)
          
          return
        endif
    enddo

    
    
  end subroutine

end module




module MatFunction
! This module contains the mathematical function (Formula) which is to be minimized 
  implicit none
  private
  public Formula

contains

  function Formula(x) result(y)
    real(8), intent(in), pointer        :: x(:)
    real(8)                             :: y

    y = (x(1)**2)+(cos(x(2)))
    !y = 5*(x(1)**2) + 3*(x(2)**2)
    
  end function

end module




module RastriginFunction
! This module contains the mathematical function (Formula) which is to be minimized 
  use ConfigHandler
  implicit none
  private
  public Rastrigin
  
contains

  function Rastrigin(config,q) result(y)
    type(ConfigType), intent(in)        :: config
    integer                             :: N, i
    real(8), pointer, intent(in)        :: q(:)
    real(8)                             :: y, pi, sigma

    N = config%order

    pi = acos(-1.0)
    
    sigma = 0
    do i = 1, N
      sigma = sigma + (q(i)**2 - 10*cos(2*pi*q(i)))
    enddo
    
    y = 10 * N + sigma
  end function

end module




module GradientCalculator
! This module contains the subroutine which is used to calculate the gradient for the given values of x
  use MatFunction
  use RastriginFunction
  use ConfigHandler
  implicit none
  private
  public CalculateGradient, CalculateHessian

contains

  subroutine CalculateGradient(config, Input, gradient)
    type(ConfigType), intent(in)        :: config
    real(8), intent(in), pointer        :: input(:)
    real(8), pointer                    :: gradient(:), x(:)
    real(8), parameter                  :: h = 0.0000001
    real(8)                             :: f(2)
    integer                             :: order, i, j

    order = size(input)
    allocate(gradient(order))
    allocate(x(order))
    
    if (config%mode == 'default')then
      do i = 1, order
        x = input
        x(i) = x(i) + h
        f(1) = Formula(x)
        x(i) = x(i) - 2*h
        f(2) = Formula(x)
     
        gradient(i) = (f(1) - f(2)) / (2 * h)
      enddo
    
    elseif (config%mode == 'rastrigin')then
      do i = 1, order
        x = input
        x(i) = x(i) + h
        f(1) = Rastrigin(config,x)
        x(i) = x(i) - 2*h
        f(2) = Rastrigin(config,x)
        
        gradient(i) = (f(1) - f(2)) / (2 * h)
      enddo
    endif
    
  end subroutine
  
  subroutine CalculateHessian(config, input, hessian)
    type(ConfigType), intent(in)        :: config
    real(8), pointer                    :: hessian(:,:), input(:), x(:)
    integer                             :: i, j, order, k
    real(8)                             :: h, f(4)

    order = config%order
    h = config%h
    
    allocate(hessian(order,order))
    allocate(x(order))
    
    hessian = 0
    
    if(config%mode == 'default')then
      do i = 1, order
        do j = 1, order
          x = input
          x(i) = x(i) + h
          x(j) = x(j) + h
        
          f(1) = Formula(x)
          x(i) = x(i) - 2*h
        
          f(2) = Formula(x)
          x(j) = x(j) - 2*h
        
          f(3) = Formula(x)
          x(i) = x(i) + 2*h
        
          f(4) = Formula(x)
     
          hessian(i,j) = (f(1) - f(2) + f(3) - f(4)) / (4 * h**2)
        enddo
      enddo
    
    elseif(config%mode == 'rastrigin')then
      do i = 1, order
        do j = 1, order
          x = input
          x(i) = x(i) + h
          x(j) = x(j) + h
        
          f(1) = Rastrigin(config,x)
          x(i) = x(i) - 2*h
        
          f(2) = Rastrigin(config,x)
          x(j) = x(j) - 2*h
        
          f(3) = Rastrigin(config,x)
          x(i) = x(i) + 2*h
        
          f(4) = Rastrigin(config,x)
     
          hessian(i,j) = (f(1) - f(2) + f(3) - f(4)) / (4 * h**2)
        enddo
      enddo
    endif
  end subroutine
  
end module




module SteepestDescent
! This module will contain the subroutines to perform minimization using the Conjugate Steepest Descent Method
  use GradientCalculator
  use ConfigHandler
  
  implicit none
  private
  public Steep

contains

  subroutine Steep(config)
    type(ConfigType), intent(in)        :: config
    real(8)                             :: convLim, gamma
    real(8), pointer                    :: gradient(:), gradientOld(:), xi(:), xiOld(:)
    integer                             :: iterations, i, order, U

    open (unit=U, action='write', file='Vectors.txt', status='replace')

    order = config%order
    
    allocate(xi(order))
    allocate(xiOld(order))
    allocate(gradient(order)) 
    allocate(gradientOld(order))

    xi = config%x1
    convLim = config%convergeLimit
    
    gamma = 0.0001
    iterations = 0
    xiOld = xi + 1
    
    do while (maxval(abs(xi - xiOld)) > convLim .and. iterations < config%iterationmax)
      if (config%list == 'on')then
        print *, 'iteration', iterations
        print *, 'x =' ; print '(f10.4)', xi
      endif
      
      write (U, *) xiOld, xi-xiOld
      write (U, *) ' '
      
      gradientOld = gradient
      call CalculateGradient(config, xi, gradient)
      
      if(iterations > 0)then
        gamma = (dot_product((xi-xiOld) , (gradient-gradientOld))) / sqrt(sum((gradient-gradientOld)**2))**2
      endif
      
      xiOld = xi
      
      if (config%direction == 'min')then
        xi = xiOld - (gamma * gradient)
      elseif (config%direction == 'max')then
        xi = xiOld + (gamma * gradient)
      endif
    
      iterations = iterations + 1
      
    enddo
    
    print *, '# of iterations until convergence = ', iterations
    print *, 'location of local minimum = ' ; print '(f10.4)', xi

    close(U)
  end subroutine

end module




module ConjugateGradient
! This module will contain the subroutines to perform minimization using the Conjugate Gradient Method
  use GradientCalculator
  use ConfigHandler
  use MatFunction

  implicit none
  private
  public Conjugate

contains

  subroutine Conjugate(config)
    type(ConfigType), intent(inout)     :: config
    real(8)                             :: convLim, alpha, beta, a, aOld, norm
    real(8), pointer                    :: hessian(:,:), gradient(:)
    real(8), pointer                    :: xi(:), r(:), p(:), Hp(:)
    integer                             :: iterations, i, order
    
    order = config%order
    
    allocate(xi(order))
    allocate(r(order))
    allocate(gradient(order))
    allocate(p(order))
    allocate(Hp(order))
    
    xi = config%x1
    convLim = config%convergeLimit
    
    call CalculateGradient(config, xi, gradient)
    call CalculateHessian(config, xi, hessian)
     
    r = gradient - matmul(hessian, xi)
    norm = sqrt(sum(r**2))
    
    iterations = 0
    
    p = r
    
    a = dot_product(r,r)

    do while (norm > convLim .and. iterations < config%iterationmax)
    if (config%list == 'on')then
     print *, 'iteration', iterations
     print *, 'x =' ; print '(f10.4)', xi
    endif  
      Hp = matmul(hessian, p)
      alpha = a / dot_product(p,Hp)

      xi = xi + alpha * p

      r = gradient - matmul(hessian,xi)
      aOld = a
      a = dot_product(r,r)
      
      p = r + a / aOld * p

      norm = sqrt(sum(r**2))

      iterations = iterations + 1
    enddo
    
    print *, '# of iterations until convergence = ', iterations
    print *, 'location of local minimum = ' ; print '(f10.4)', xi
  end subroutine
    
    
end module




module OutputWriter
! This module will contain the subroutines to write output to standard output or a file as defined
  implicit none
  private
!  public
end module




module PlotModule
  use MatFunction
  use RastriginFunction
  use ConfigHandler
  implicit none
  private
  public Plot

contains

  subroutine Plot(config)
  type(ConfigType), intent(in)  :: config
  
  character(len=*), parameter   :: F = 'data.txt'   ! File name.
  integer,          parameter   :: U = 20         ! Output unit.
  character(20)                 :: Name = 'plot'
  character(30)                 :: Gnu

  integer                       :: i, j
  real(8)                       :: domain
  real(8)                       :: step
  real(8)                       :: lengthR
  integer                       :: length
  
  !real(8)                      :: x(2,length), z(length,length)
  real(8), pointer              :: q(:), x(:,:), z(:,:)

  domain = config%plotDomain
  step = config%plotStep
  lengthR = (2*domain)/step
  length = int(lengthR) 
 
  allocate(q(2))
  allocate(x(2,length))
  allocate(z(length,length))

  do i = 1, length
    x(1,i) = i*step-domain
    x(2,i) = i*step-domain
  end do

  do i = 1, length
    do j = 1, length
      q(1) = x(1,i)
      q(2) = x(2,j)
      if(config%mode == 'rastrigin')then
        z(j,i) = Rastrigin(config,q)
      elseif(config%mode == 'default')then
        z(j,i) = Formula(q)
      endif
    enddo
  enddo

  ! Open data file and write values into it.
  open (unit=U, action='write', file=F, status='replace')
  
  do i = 1, length
    do j = 1, length
      write (U, *) x(2,i), x(1,j), z(j,i)
    enddo
         write (U, *) ' '
  end do

  close (U)
    
  Gnu = 'gnuplot -p contour.plt'
  call system(Gnu)
  
  end subroutine

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




Program Minimization
! This is the main program from which the subroutines in the program modules will be called
use SteepestDescent
use ConjugateGradient
use ConfigHandler
use OutputWriter
use PlotModule
use Logger
use Testing
use GradientCalculator
use Matfunction

implicit none

  type(ConfigType)      :: config
  
  call Initialize(config)

  if (config%algorithm == 'steep') then
    call Steep(config)
  elseif (config%algorithm == 'conjugate')then
    call Conjugate(config)
  endif
  
  if (config%order == 2 .and. config%plot == 'on') then
    call Plot(config)
  endif
  
end Program



