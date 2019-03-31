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