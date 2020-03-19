program main
	implicit none
	real, dimension(:,:), allocatable :: array_a, array_b, array_c
	integer, dimension(2) :: shape_a, shape_b
	integer :: i, j, x1 ,y1, x2, y2
	shape_a = (/ 10, 5 /)
	shape_b = (/ 5, 10 /)
	x1 = shape_a(1)
	y1 = shape_a(2)
	x2 = shape_b(1)
	y2 = shape_b(2)
	allocate(array_a(x1, y1))
	allocate(array_b(x2, y2))
	!print *, x1, y1
	do i = 1, x1
		do j = 1, y1
			array_a(i, j) = i*0.84
		end do
	end do
	do i = 1, x2
		do j = 1, y2
			array_a(i, j) = i*0.84
		end do
	end do
	allocate(array_c(x1,y2))
	!print *, shape(array_c)
	array_c = multiply_matrix(array_a, array_b)
	call print_array(array_a)
	call print_array(array_b)
	call print_array(array_c)
contains

function multiply_matrix(array_a, array_b) result(array_c)
	real, dimension(:,:) :: array_a, array_b
	real, dimension(lbound(array_a, dim=1):ubound(array_a, dim=1), lbound(array_b, dim=2):ubound(array_b, dim=2)) :: array_c
	integer, dimension(2) :: shape_a, shape_b
	shape_a = shape(array_a)
	shape_b = shape(array_b)
	print *, shape_a
	print *, shape_b
	array_c = matmul(array_a,array_b)
	!allocate(array_c(10,10))
end function

subroutine print_array(array)
	real, dimension(:,:), intent(in) :: array
	integer :: x,y,i,j
	integer, dimension(2) :: sh
	sh = shape(array)
	x = sh(1)
	y = sh(2)
	do i=1, x
		do j=1, y
			write(*, fmt="(1x,f8.3)", advance="no") array(i,j)
		end do
		write(*,*)
	end do
	write(*,*)
end subroutine
end program main
