program zippy

	use sort 
	implicit none 
	integer, parameter 							:: array_size = 100
	type(address), dimension(array_size)		:: data_array
	integer 									:: i, n

	do i = 1, array_size
		read(*, '(/a/a/a/a2,i8)', end=10)     data_array(i)
		write(*, '(/a/a/a/a2,i8)')    data_array(i)
		end do

	10 n = i-1 
		call selection_sort(data_array(1:n))
		write(*, '(//a)') 'after sorting:'
		do i = 1,n 
			write(*, '(/a/a/a/a2,i8)') data_array(i)
		end do 
end program zippy