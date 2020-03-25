module linear_layer
	implicit none
	private
	type, public :: linear_layer_object
		real, dimension(:,:), private, allocatable :: W, B
		integer, dimension(2), private :: shape_W, shape_B
		integer :: num_inputs, num_outputs
	contains
		procedure :: constructor => linear_layer_constructor
		procedure :: print_layer => print_layer_values
	end type linear_layer_object
contains
	subroutine print_layer_values(this)
		!implicit none
		class(linear_layer_object), intent(in) :: this
		print *, "linear_layer shape(", this%num_inputs, this%num_outputs, ")"
	end subroutine print_layer_values

	subroutine linear_layer_constructor(this, num_inputs_local, num_outputs_local)
		class(linear_layer_object) :: this
		integer :: num_inputs_local, num_outputs_local
		this%num_inputs = num_inputs_local
		this%num_outputs = num_outputs_local
		this%shape_W = (/ num_inputs_local, num_outputs_local /)
		this%shape_B = (/ num_outputs_local, 1 /)
		allocate(this%W(num_inputs_local,num_outputs_local))
		allocate(this%B(num_outputs_local,1))
	end subroutine linear_layer_constructor
	!TO DO: make function that prints matrix for convenince
	!TO DO: when making the actual module i shoul make a module for matrices as well (print and utilities)
end module linear_layer

program main
	use linear_layer
	implicit none
	type(linear_layer_object) :: a
	call a%constructor(10, 20)
	call a%print_layer
end program main 

