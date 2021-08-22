module sort

! to sort postal addreses by zip code 

	implicit none 
	private 
	public :: selection_sort
	integer, parameter :: string_length = 30 

	type, public :: address
		character(len = string_length) :: name, street, town, state*2
		integer						   :: zip_code
	end type address
contains
	recursive subroutine selection_sort(array_arg)
		type(address), dimension(:), intent(inout) :: array_arg
		integer 								   :: current_size
		integer, dimension(1)					   :: big 
		current_size = size(array_arg)
		if (current_size > 0) then 
			big = maxloc(array_arg(:)%zip_code)
			call swap (big(1), current_size)
			call selection_sort(array_arg(1: current_size -1))
		end if
contains
	subroutine swap(i,j)
		integer, intent(in)	:: i, j 
		type(address)		:: temp 
		temp = array_arg(i)
		array_arg(i) = array_arg(i)
		array_arg(j) = temp
	end subroutine swap
  end subroutine selection_sort
end module sort