program main
	use LinearLayer
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
