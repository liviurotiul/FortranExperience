module linear_layer
	implicit none
	type, public :: linear_layer_object
		real, dimension(:,:), private, allocatable :: W, B
		integer, dimension(2), private :: shape_W, shape_B
	contains
		procedure :: print_layer

	end type linear_layer_object
contains
	subroutine print_layer(this)
		implicit none
		integer :: x, y, i, j
		print *, "Wheights"
		x = this%shape_W(1)
		y = this%shape_W(2)
		do i=1, x
			do j=1, y
				write(*, fmt="(1x,f8.3)", advance = "no") this%(i,j)
			end do
			write(*,*)
		end do
	end subroutine print_layer
	!TO DO: make function that prints matrix for convenince
	!TO DO: when making the actual module i shoul make a module for matrices as well (print and utilities)
end module linear_layer

