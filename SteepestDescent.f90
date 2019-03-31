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
