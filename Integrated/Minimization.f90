
module ConfigHandler
! Created by: Bram van de Put, 2019
!
! The ConfigHandler module sets all the initial variables and constants needed to run the
! minimizations.

  implicit none
  private
  save
  public ConfigType, Initialize

  type ConfigType
    integer                         :: order, iterationmax
    real(8)                         :: h, convergeLimit, plotDomain, plotStep, gamma
    real(8), allocatable            :: x1(:)
    character(10)                   :: mode, algorithm, direction, list, plot, plotVector
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
    read(U,*) self%plotVector
    read(U,*) self%list 
    read(U,*) self%order
    allocate(self%x1(self%order))
    read(U,*) self%x1
    read(U,*) self%h
    read(U,*) self%gamma
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
      
      if (self%algorithm == 'conjugate')then
        print *, '"mode" "algorithm" "direction" "plot" "list" "order" "h" "convergelimit" "iterationmax"'
      else
        print *, '"mode" "algorithm" "direction" "plot" "list" "order" "h" "x1" "gamma" "convergelimit" "iterationmax"'
      endif
      
      read *, inputChar
      
      if (inputChar == 'mode')then
        print *, 'Enter: "default" for default mode, "rastrigin" for rastrigin surface mode or "test" to perform a unit test'
        read *, inputChar
        
        if (inputChar == 'default' .or. inputChar == 'rastrigin' .or. inputchar == 'test')then 
          self%mode = inputChar
        else
          print *, 'invalid argument, no new value set'
        endif

        
      elseif (inputChar == 'algorithm')then
        print *, 'Choose the minimization algorithm: "steep" for SteepestDescent or "conjugate" for ConjugateGradient'
        read *, inputChar
        if (inputChar == 'steep')then 
          self%algorithm = inputChar
        elseif (inputChar == 'conjugate')then
          self%algorithm = inputChar
          self%x1 = 0
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
          
          print *, 'Toggle vector overlay: "on" or "off"'
          read *, inputChar
          if (inputChar == 'on' .or. inputchar == 'off')then
            self%plotVector = inputChar
          endif
          
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
        if (self%algorithm == 'conjugate')then
          deallocate(self%x1)
          allocate(self%x1(self%order))
          self%x1 = 0
        endif
        
      elseif (inputChar == 'h')then
        print *, 'enter a low real value for h'
        read *, inputReal
        self%h = inputReal
        
      elseif (inputChar == 'gamma')then
        print *, 'enter a real value for the step size of the SteepestDescent algorithm (enter 0 for automatic)'
        read *, inputReal
        self%gamma = inputReal
        
      elseif (inputChar == 'x1' .and. self%algorithm /= 'conjugate')then
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
          write(U,*) self%plotVector
          write(U,*) self%list
          write(U,*) self%order
          write(U,*) self%x1
          write(U,*) self%h
          write(U,*) self%gamma
          write(U,*) self%convergeLimit
          write(U,*) self%iterationmax
    
          close(U)
          
          return
        endif
    enddo

  end subroutine

end module




module MatFunction
! Created by: Bram van de Put, 2019
!
! The MatFunction module contains the mathematical function to be minimized
! as of release: x^2 + cos(y)
! This function may be altered from the source code as needed.

  implicit none
  private
  public Formula

contains

  function Formula(x) result(y)
    real(8), intent(in), pointer        :: x(:)
    real(8)                             :: y

    y = ((1+x(1))**2)+(cos(1+x(2)))
    !y = 5*(x(1)**2) + 3*(x(2)**2)
    
  end function

end module




module RastriginFunction
! Created by: Bram van de Put, 2019
!
! The RastriginFunction module contains the formula for the nth order Rastrigin surface.
! The rastrigin surface scales with the order and is therefore not dependent on manual adjustment
! of the configuration.

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
! Created by: Bram van de Put, 2019
!
! This module contains the subroutine which is used to calculate the gradient for the given values of x
! The GradientCalculator module contains the routines 'CalculateGradient' and 'CalculateHessian' 
! which are used to calculate the gradient vector and the hessian matrix respectively.
! both are based on the central difference approximation.

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
! Created by: Bram van de Put, 2019
!
! The SteepestDescent module contains the routine 'Steep' which performs the steepest descent
! algorithm for minimizing the chosen function.
! The SteepestDescent algorithm takes iterative steps towards the negative gradient (direction
! of largest decrease). This vector is multiplied by a scalar which is calculated such that the
! minimum is not overshot, but rather the algorithm converges towards the minimum.

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
    
    U = 10
    open (unit=U, action='write', file='Vectors.txt', status='replace')

    order = config%order
    
    allocate(xi(order))
    allocate(xiOld(order))
    allocate(gradient(order)) 
    allocate(gradientOld(order))

    xi = config%x1
    convLim = config%convergeLimit
    
    if (config%gamma == 0)then
      gamma = 0.1
    else
      gamma = config%gamma
    endif
    
    iterations = 0
    xiOld = xi + 1
    
    do while (maxval(abs(xi - xiOld)) > convLim .and. iterations < config%iterationmax)
      if (config%list == 'on')then
        print *, 'iteration', iterations
        print *, 'x =' ; print '(f10.4)', xi
      endif
      
      gradientOld = gradient
      call CalculateGradient(config, xi, gradient)
      
      if(iterations > 0 .and. config%gamma == 0)then
        gamma = (dot_product((xi-xiOld) , (gradient-gradientOld))) / sqrt(sum((gradient-gradientOld)**2))**2
      endif
      
      xiOld = xi
      
      if (config%direction == 'min')then
        xi = xiOld - (gamma * gradient)
      elseif (config%direction == 'max')then
        xi = xiOld + (gamma * gradient)
      endif
    
      write (U, *) xiOld, xi-xiOld
    
      iterations = iterations + 1
      
    enddo
    
    print *, '# of iterations until convergence = ', iterations
    print *, 'location of local minimum = ' ; print '(f10.4)', xi

    close(U)
  end subroutine

end module




module ConjugateGradient
! Created by: Bram van de Put, 2019
!
! The ConjugateGradient module contains the routine 'Conjugate' which performs the conjugate
! gradient algorithm for minimizing the chosen function.
! The ConjugateGradient algorithm takes iterative steps based on the direction of largest 
! decrease, Compared to the steepest descent algorithm though, each subsequent direction is 
! chosen to be orthogonal to each previous step.
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
    real(8), pointer                    :: xi(:), xiOld(:), r(:), p(:), Hp(:)
    integer                             :: iterations, i, order, U
    
    U = 10
    open (unit=U, action='write', file='Vectors.txt', status='replace')
    
    order = config%order
    
    allocate(xi(order))
    allocate(xiOld(order))
    allocate(r(order))
    allocate(gradient(order))
    allocate(p(order))
    allocate(Hp(order))
    
    xi = config%x1
    convLim = config%convergeLimit
    
    call CalculateGradient(config, xi, gradient)
    call CalculateHessian(config, xi, hessian)

    r = matmul(hessian, xi) - gradient
    norm = sqrt(sum(r**2))
    
    iterations = 0
    
    p = -r
    xiOld = xi+1
    a = dot_product(r,r)

    do while (norm > convLim .and. iterations < config%iterationmax)
      if (config%list == 'on')then
        print *, 'iteration', iterations
        print *, 'x =' ; print '(f10.4)', xi
      endif

      Hp = matmul(hessian, p)
      alpha = a / dot_product(p,Hp)
      
      xiOld = xi
      xi = xi + alpha * p
      
      write (U, *) xiOld, xi-xiOld
      
      !r = gradient - matmul(hessian,xi)
      r = r + alpha * Hp
      
      aOld = a
      a = dot_product(r,r)
      
      beta = a/aOld
      
      !p = r + a / aOld * p
      p = -r + beta * p

      norm = sqrt(sum(r**2))

      iterations = iterations + 1
      
    enddo

    print *, '# of iterations until convergence = ', iterations
    print *, 'location of local minimum = ' ; print '(f10.4)', xi
        
    close(U)
  end subroutine
  
end module




module PlotModule
! Created by: Bram van de Put, 2019
!
! The PlotModule module contains the routine 'plot' which creates a surface plot and overlaps it
! with the minimization steps as vectors. The plot function is only enabled for functions of 2nd
! order. Use of the plot function requires gnuplot to be installed.

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
  
  if (config%plotVector == 'on')then
    Gnu = 'gnuplot -p PlotVec.plt'
  else
    Gnu = 'gnuplot -p Plot.plt'
  endif
  
  call system(Gnu)
  
  end subroutine

end module




module Testing
! Created by: Bram van de Put, 2019
!
! The Testing module performs unit tests on the routines: CalculateGradient and CalculateHessian.
! The tests are performed by initializing the settings and checking the output of the program

  use ConfigHandler
  use GradientCalculator
  use SteepestDescent
  use ConjugateGradient
  
  implicit none
  private
  Public Test

contains
  
  subroutine Test(config)
    type(ConfigType)      :: config
    real(8), pointer      :: x(:), gradient(:), hessian(:,:)
    real(8), parameter    :: limit = 0.01
    
    print *, 'Performing unit test'
    
    config%mode                         = 'rastrigin'
    config%algorithm                    = 'steep'
    config%direction                    = 'min'
    config%plot                         = 'off'
    config%plotDomain                   = 5
    config%plotStep                     = 0.1
    config%plotVector                   = 'off'
    config%list                         = 'off'
    config%order                        = 2
    deallocate(config%x1)
    allocate(config%x1(config%order))
    config%x1                           = (/1,1/)
    config%h                            = 0.001
    config%convergeLimit                = 0.000000001
    config%iterationmax                 = 30
    
    allocate(x(config%order))
    x = config%x1
    
    call CalculateGradient(config, x, gradient)
    call CalculateHessian(config, x, hessian)
    
    if (sum(gradient-(/2,2/)) < limit)then
      print *, 'Routine CalculateHessian PASS'
    endif
    if (sum(hessian-reshape((/396.779,0.0,0.0,396.779/),(/2,2/))) < limit)then
      print *, 'Routine CalculateGradient PASS'
    endif
  
  end subroutine
  
end module




Program Minimization
! Created by: Bram van de Put, 2019
!
! This is the main program from which the subroutines in the program modules will be called.

  use SteepestDescent
  use ConjugateGradient
  use ConfigHandler
  use PlotModule
  use Testing

  implicit none

  type(ConfigType)      :: config
  
  call Initialize(config)

  if (config%algorithm == 'steep' .and. config%mode /= 'test') then
  Print *, ' '
  print *, 'Performing SteepestDescent minimization on function: ', config%mode
    call Steep(config)
  elseif (config%algorithm == 'conjugate' .and. config%mode /= 'test')then
  Print *, ' '
  print *, 'Performing ConjugateGradient minimization on function: ', config%mode
    call Conjugate(config)
  endif
  
  if (config%mode == 'test')then
    call Test(config)
  endif
  
  if (config%plot == 'on') then
    if (config%order == 2) then
      call Plot(config)
    else
      print *, 'The order is not equal to 2, plotting can not be performed'
    endif
  endif
  
end Program



