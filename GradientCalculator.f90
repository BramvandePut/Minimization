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