module LinearLayer
	implicit none
	private
	type, public :: linear_layer_object
		real, dimension(:,:), private, allocatable :: W, B, output !output is used to compute partial updates
		integer :: num_inputs, num_outputs
	contains
		procedure :: constructor => linear_layer_constructor
		procedure :: print_layer => print_layer_values
		procedure :: forward => forward_function
	end type linear_layer_object
contains
	subroutine print_layer_values(this)
		class(linear_layer_object), intent(in) :: this
		print *, "linear_layer shape(", this%num_inputs, this%num_outputs, ")"
		print *, "wheights matrix"
		print *, this%W
		print *, "bias matrix"
		print *, this%B
	end subroutine print_layer_values

	subroutine linear_layer_constructor(this, num_inputs_local, num_outputs_local)
		class(linear_layer_object) :: this
		integer :: num_inputs_local, num_outputs_local
		integer :: i, j
		this%num_inputs = num_inputs_local
		this%num_outputs = num_outputs_local
		allocate(this%W(num_inputs_local,num_outputs_local))
		allocate(this%B(num_outputs_local,1))
		allocate(this%output(num_outputs_local,1))
		do i=1, size(this%W,1)
			do j=1, size(this%W,2)
				this%W(1,2) = 4
			end do
		end do
	end subroutine linear_layer_constructor
	!this one will be a function because we will be returnong the output
	function forward_function(this, inputs) result(outputs)
		class(linear_layer_object) :: this
		real, dimension(this%num_inputs, 1) :: outputs
		real, dimension(this%num_inputs, 1), intent(in) :: inputs
		outputs = inputs*transpose(this%W) + transpose(this%B)
		this%output = outputs
	end function forward_function
	!TO DO: make function that prints matrix for convenince
	!TO DO: when making the actual module i shoul make a module for matrices as well (print and utilities)
end module LinearLayer

