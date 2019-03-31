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



