!starting from TestLinearLayer now we will implement a working linear layer with a forward function

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
		procedure :: forward => forward_function
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
		integer :: i, j
		this%num_inputs = num_inputs_local
		this%num_outputs = num_outputs_local
		this%shape_W = (/ num_inputs_local, num_outputs_local /)
		this%shape_B = (/ 1, num_outputs_local /)
		allocate(this%W(num_inputs_local,num_outputs_local))
		allocate(this%B(num_outputs_local,1))
		do i=1, size(this%W,1)
			do j=1, size(this%W,2)
				this%W(1,2) = 4
			end do
		end do
	end subroutine linear_layer_constructor
	!this one will be a function because we will be returnong the output
	function forward_function(this, inputs) result(outputs)
		class(linear_layer_object), intent(in) :: this
		real, dimension(this%num_inputs, 1) :: outputs
		real, dimension(this%num_inputs, 1), intent(in) :: inputs
		!print *, shape(inputs), shape(this%W), shape(this%B)
		print *, "the weight matrix is:"
		print *, this%W
		print *, "the bias is:"
		print *, this%B
		outputs = inputs*transpose(this%W) + transpose(this%B)
	end function forward_function
	!TO DO: make function that prints matrix for convenince
	!TO DO: when making the actual module i shoul make a module for matrices as well (print and utilities)
end module linear_layer

program main
	use linear_layer
	implicit none
	type(linear_layer_object) :: layer
	real, dimension(2,1) :: dummy_output
	real, dimension(5,1) :: dummy_input
	dummy_input = 1.0
	print *, "the input is:"
	print *, dummy_input
	call layer%constructor(5, 2)
	call layer%print_layer
	dummy_output = layer%forward(dummy_input)
	print *,  "the result is"
	print *, dummy_output
end program main 

