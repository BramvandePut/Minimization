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