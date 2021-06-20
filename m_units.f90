!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     units module
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module m_units

  !use m_parameters

  implicit none

  integer, parameter :: dp = kind(4.0d0)


!==============================================================
!       constants depending on the unit system used
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  !cgs (gauss) constannts
  real(kind = dp), parameter ::  c         = 1
  real(kind = dp), parameter ::  q         = 1
  real(kind = dp), parameter ::  me         = 1


end module m_units
