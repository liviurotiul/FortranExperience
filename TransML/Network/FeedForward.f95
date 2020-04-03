module FeedForward
	use LinearLayer
	implicit none
	private
	type, public Network
		integer father_vector !contains the ids for the sccesions in the graph
		integer last_node_id
	contains
		procedure :: forward_with_grad => grad
	end type Network
contains
	function forward_with_grad(this, layer, input) result(output)
		class(Network) :: this
		type(linear_layer_object) :: layer
		real (:) :: input
		real (:), allocatable :: output
		this%father_vector(this%last_node_id+1) = this%last_node_id
		this%last_node_id = this%last_node_id+1
		output = layer%forward(x) 
end module
