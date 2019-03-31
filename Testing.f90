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
