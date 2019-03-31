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