program BorisPusher

use m_units

implicit none


! Position 
integer :: x = 0
integer :: y = 0 
integer :: z = 0

double precision, parameter :: vx = 0.0
double precision, parameter :: vy = 0.0 
double precision, parameter :: vz = 0.0

double precision, parameter :: Ex = 0.0
double precision, parameter :: Ey = 0.0 
double precision, parameter :: Ez = 0.0

double precision, parameter :: Bx = 0.0
double precision, parameter :: By = 0.0 
double precision, parameter :: Bz = 0.1


 real v2, g, ux, uy, uz, ux_min, uy_min, uz_min 
 integer :: tx, ty, tz, t2, sx, sy, sz
 integer :: ux_pr, uy_pr, uz_pr, ux_plus, uy_plus, uz_plus
 integer :: u2, gamma, x1, y1, z1
 integer :: i

integer :: Nt = 10000
integer :: dt = 0.1

real(kind=8), allocatable :: T(:), XX(:), YY(:), ZZ(:), UUX(:), UUY(:), UUZ(:)

! Allocate memory
allocate( T(Nt), XX(Nt), YY(Nt), ZZ(Nt), UUX(Nt), UUY(Nt), UUZ(Nt) )

! print*, 'after allocation'
v2 = vx**2 + vy**2 + vz**2
g  = 1 / (1-v2)**0.5

ux = g*vx
uy = g*vy 
uz = g*vz

do i = 1, Nt
    print*, i
    T(i)   = i*0.0
    XX(i)  = i*0.0
    YY(i)  = i*0.0
    ZZ(i)  = i*0.0
    UUX(i) = i*0.0
    UUY(i) = i*0.0
    UUZ(i) = i*0.0
 end do

T(1)   = 0
XX(1)  = x 
YY(1)  = y 
ZZ(1)  = z
UUX(1) = ux 
UUY(1) = uy 
UUZ(1) = uz


do i = 2, Nt

   print*, i

   ux_min = ux + (q/me)* Ex*dt/2
   uy_min = uy
   uz_min = uz

   tx = 0
   ty = 0
   tz = - (q/(me*c))* Bz * dt/(2*g)
   t2 = (tx**2 + ty**2 + tz**2)

   sx = 2*tx/(1+t2)
   sy = 2*ty/(1+t2)
   sz = 2*tz/(1+t2)

!Rotate the result with half magnetic impulse
   ux_pr = ux_min + (uy_min*tz - uz_min*ty)
   uy_pr = uy_min - (ux_min*tz - uz_min*tx)
   uz_pr = uz_min + (ux_min*ty - uy_min*tx)

!Rotate the result with full magnetic impulse
   ux_plus = ux_min + (uy_min*sz - uz_min*sy)
   uy_plus = uy_min - (ux_min*sz - uz_min*sx)
   uz_plus = uz_min + (ux_min*sy - uy_min*sx)

! Add remaining electric impulse

  ux = ux_plus + (q/me)* Ex*dt/2
  uy = uy_plus
  uz = uz_plus
  u2 = (ux**2 + uy**2 + uz**2)
  gamma = (1 + u2)**0.5

  x = x + dt*ux/gamma
  y = y + dt*uy/gamma
  z = z + dt*uz/gamma

	T(i) = i*dt
	XX(i) = x
	YY(i) = y
	ZZ(i) = z

	UUX(i)  = ux
	UUY(i)  = uy
	UUZ(i)  = uz

end do

! do i = 1, Nt
 !   print *, T(i)
! end do

deallocate( T, XX, YY, UUX, UUY, UUZ )
end program BorisPusher
