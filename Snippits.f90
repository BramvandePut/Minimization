  subroutine SetMode(self, mode)
	type(ConfigType), intent(out)		:: self
	character(10)						:: mode
	
	self%mode = mode
  end subroutine
  
  subroutine SetAlgorithm(self, algorithm)
	type(ConfigType), intent(out)		:: self
	character(10)						:: algorithm
	
	self%algorithm = algorithm
  end subroutine
  
  subroutine SetOrder(self, order)
    type(ConfigType), intent(out)		:: self
	integer								:: order
	
	self%order = order
  end subroutine
  
  subroutine SetConvergeLimit(self, convergeLimit)
	type(ConfigType), intent(out)		:: self
    real(8)								:: convergeLimit
	
	self%convergeLimit = convergeLimit
	end subroutine
	
	  subroutine HessianCalculator(config,hessian,input)
	type(ConfigType), intent(in)		:: config
	real(8), pointer					:: hessian(:,:), input(:), x(:), gPlusH(:), gMinH(:), g(:,:,:)
	integer								:: i, j, order
	real(8)								:: h
	
	order = config%order
	h = config%h
	
	allocate(hessian(order,order))
    allocate(g(2, order, order))
	allocate(gPlusH(order))
	allocate(gMinH(order))
	allocate(x(order))
	
	hessian = 0
	
	do i = 1, order
	  x = input
      x(i) = x(i) + h
      call CalculateGradient(x, gPlusH)
	  x(i) = x(i) - 2*h
      call CalculateGradient(x, gMinH)
	  
	  g(1,i,:) = gPlusH
	  g(2,i,:) = gMinH
	enddo
	
	do i = 1, order
	  do j = 1, order
	  !  hessian(i,j) = ((gPlusH(i) - gMinH(i)) / (4 * h)) + ((gPlusH(j) - gMinH(j)) / (4 * h))
	  
	    hessian(i,:) = (g(1,i,:) - gMinH(i)) / (4 * h) + (gPlusH(j) - gMinH(j)) / (4 * h)
	  enddo
	enddo
	
  end subroutine