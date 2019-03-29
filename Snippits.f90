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
  
  
  
  subroutine Conjugate(config)
	type(ConfigType), intent(inout)		:: config
    real(8)								:: convLim, alpha, beta, b(2)
    real(8), pointer					:: hessian(:,:), gradient(:), gradientOld(:), xi(:), xiOld(:), r(:), rOld(:), a(:), p(:)
	integer								:: iterations, i, order
	
	order = config%order
	
	allocate(xi(order))
	allocate(xiOld(order))
	allocate(r(order))
	allocate(rOld(order))
	allocate(gradient(order)) 
	allocate(gradientOld(order))
	allocate(a(order))
	allocate(p(order))

	xi = config%x1
	convLim = config%convergeLimit
	iterations = 0
	
	call CalculateGradient(config, xi, gradient)
	print *, 'g = ', gradient
	call CalculateHessian(config, xi, hessian)
	print *, 'H = ', hessian
	
	print *, 'b =', matmul(hessian,xi) - gradient
	
	do i = 1, order
	  a(i) = hessian(i,i)
	enddo
	
	!b = dot_product(a,xi)
	!print *, b
	! r = a*xi-b
	
	b = matmul(hessian,xi)-gradient
	! print *, b
	! r = b-a*xi
	! print *, r
	r = b-matmul(hessian,xi)
	!r = gradient-matmul(hessian,xi)
	print *, 'r = ', r
	
	p = -r
	
	do while (abs(sum(r)) > convLim .and. iterations < 10000)
	  alpha = dot_product(r,r) / dot_product(p,a*p)
	
	  xiOld = xi
	
	  xi = xi + alpha * p
	
	  rOld = r
	
	  r = r + alpha * a * p
	
	  beta = (dot_product(r, r) / dot_product(rOld, rOld))
	
	  p = r + beta*p
	  
	  iterations = iterations + 1
	  
	  print *, xi
	enddo
	
	r = gradient - matmul(hessian, xi)
	
  end subroutine
  
  
  subroutine Conjugate(config)
	type(ConfigType), intent(inout)		:: config
    real(8)								:: convLim, alpha, beta, b(2), q(2)
    real(8), pointer					:: hessian(:,:), gradient(:), gradientOld(:), xi(:), xiOld(:), r(:), rOld(:), a(:), p(:)
	integer								:: iterations, i, order
	
	order = config%order
	
	allocate(xi(order))
	allocate(xiOld(order))
	allocate(r(order))
	allocate(rOld(order))
	allocate(gradient(order)) 
	allocate(gradientOld(order))
	allocate(a(order))
	allocate(p(order))

	xi = config%x1
	convLim = config%convergeLimit
	iterations = 0
	
	call CalculateGradient(config, xi, gradient)

	call CalculateHessian(config, xi, hessian)
	
	b = matmul(hessian,xi)-gradient
	r = -gradient
	p = r
	q = matmul(hessian,p)
	
	alpha = dot_product(r,r)/dot_product(p,q)
	
	xi = xi + (alpha * p)
	
	rOld = r
	
	r = r - (alpha * q)
	
	do while (abs(sum(r)) > convLim .and. iterations < 10000)
	  call CalculateHessian(config, xi, hessian)
	  
	  beta = (dot_product(r, r) / dot_product(rOld, rOld))
	  
	  p = r + (beta * p)
	  
	  q = matmul(hessian,p)
	  
	  alpha = dot_product(r,r) / dot_product(p,q)
	
	  xiOld = xi
	
	  xi = xi + alpha * p
	
	  rOld = r
	
	  r = r - alpha * q 
	  
	  iterations = iterations + 1
	  
	  print *, 'xi = ', xi
	  print *, abs(sum(r))
	enddo
	
  end subroutine
  
  subroutine Conjugate(config)
	type(ConfigType), intent(inout)		:: config
    real(8)								:: convLim, alpha, beta, b(2), q(2)
    real(8), pointer					:: hessian(:,:), gradient(:), gradientOld(:), xi(:), xiOld(:), r(:), rOld(:), a(:), p(:)
	integer								:: iterations, i, order
	
	order = config%order
	
	allocate(xi(order))
	allocate(xiOld(order))
	allocate(r(order))
	allocate(rOld(order))
	allocate(gradient(order)) 
	allocate(gradientOld(order))
	allocate(a(order))
	allocate(p(order))

	xi = config%x1
	convLim = config%convergeLimit
	iterations = 0
	
	call CalculateGradient(config, xi, gradient)

	call CalculateHessian(config, xi, hessian)
	
	b = matmul(hessian,xi)-gradient
	r = -gradient
	p = r
	q = matmul(hessian,p)
	
	alpha = dot_product(p,r)/dot_product(p,q)
	
	xi = xi + (alpha * p)
	
	do while (abs(sum(r)) > convLim .and. iterations < 10000)
	  call CalculateHessian(config, xi, hessian)
	  
	  beta = beta + ((dot_product(p,matmul(hessian,r))/dot_product(p,matmul(hessian,p)))*p)
	  !p = r + (beta * p)
	  
	  rOld = r
	
	  r = r - (alpha * q)
	  
	  q = matmul(hessian,p)
	  
	  alpha = dot_product(r,r) / dot_product(p,q)
	
	  xiOld = xi
	
	  xi = xi + alpha * p
	
	  rOld = r
	
	  r = r - alpha * q 
	  
	  iterations = iterations + 1
	  
	  print *, 'xi = ', xi
	  print *, abs(sum(r))
	enddo
	
  end subroutine
  
  subroutine Conjugate(config)
	type(ConfigType), intent(inout)		:: config
    real(8)								:: convLim, alpha, beta(2), b(2), q(2)
    real(8), pointer					:: hessian(:,:), gradient(:), gradientOld(:) 
	real(8), pointer					:: xi(:), xiOld(:), r(:), rOld(:), a(:), p(:), pi(:,:), piOld(:,:)
	integer								:: iterations, i, order
	
	order = config%order
	
	allocate(xi(order))
	allocate(xiOld(order))
	allocate(r(order))
	allocate(rOld(order))
	allocate(gradient(order)) 
	allocate(gradientOld(order))
	allocate(a(order))
	allocate(p(order))
	allocate(pi(order,1))
	allocate(piOld(order,i))

	xi = config%x1
	convLim = config%convergeLimit
	iterations = 1
	beta = 0
	
	call CalculateGradient(config, xi, gradient)

	call CalculateHessian(config, xi, hessian)
	
	b = matmul(hessian,xi)-gradient
	r = -gradient
	p = r
	pi(:,1) = p
	q = matmul(hessian,p)
	
	alpha = dot_product(p,r)/dot_product(p,q)
	
	xiOld = xi
	xi = xi + (alpha * r)
	rOld = r
	r = r - (alpha * q)
	
	do while (abs(sum(xi-xiOld)) > convLim .and. iterations < 10000)
	  ! call CalculateHessian(config, xi, hessian)
	  
	  ! do i = 1, size(pi,2)
	    ! beta = (dot_product(pi(:,i),matmul(hessian,r))/dot_product(pi(:,i),matmul(hessian,pi(:,i))))*pi(:,i)
	  ! enddo
	  
	  ! p = r - beta
	  
	  ! deallocate(piOld)
	  ! allocate(piOld(order,size(pi,2)))
	  ! piOld = pi
	  
	  ! deallocate(pi)
	  ! allocate(pi(order,size(piOld,2)+1))
	  ! pi(:,:size(piOld,2)) = piOld
	  ! pi(:,size(pi,2)) = p
	  
	  p = r + dot_product(r,r)/dot_product(rOld,rOld)*p
	  
	  ! rOld = r
	
	  q = matmul(hessian,p)
	
	  ! r = r - (alpha * q)
	  
	  alpha = dot_product(r,r) / dot_product(p,q)
	
	  xiOld = xi
	
	  xi = xi - alpha * p
	
	  rOld = r
	
	  r = r - alpha * q 
	  
	  iterations = iterations + 1
	  
	  print *, 'xi = ', xi
	  print *, abs(sum(r))
	enddo
	
  end subroutine