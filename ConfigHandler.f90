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