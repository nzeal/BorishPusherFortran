program main

  type t1_t
     real :: x
  end type t1_t

  type, extends(t1_t) :: t1e_t
     character(8) :: string
  end type t1e_t

  type(t1e_t) :: t1e

  integer :: answer
  namelist /test_NML/ t1e, answer

  open(unit=1,file='test1.inp')
  read(1,NML=test_NML)

  write(*,*) t1e%x, t1e%string, answer

end program