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