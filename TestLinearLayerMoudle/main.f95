module linear_layer
	implicit none
	private
	type, public :: linear_layer_object
		!real, dimension(:,:), private, allocatable :: W, B
		integer, dimension(2), private :: shape_W, shape_B
	contains
		procedure :: print_layer => print_layer_values
	end type linear_layer_object
contains
	subroutine print_layer_values(this)
		!implicit none
		class(linear_layer_object), intent(in) :: this
		print *, "sal"
	end subroutine print_layer_values
	!TO DO: make function that prints matrix for convenince
	!TO DO: when making the actual module i shoul make a module for matrices as well (print and utilities)
end module linear_layer

program main
	use linear_layer
	implicit none
	type(linear_layer_object) :: a
	call a%print_layer
end program main 

