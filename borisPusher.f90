program BorisPusher

implicit none

! Define double precision working with all compilers
integer, parameter :: rp = selected_real_kind(15)

! Position 
real(rp) :: x 
real(rp) :: y 
real(rp) :: z

real(rp) :: vx 
real(rp) :: vy 
real(rp) :: vz

real(rp) :: Ex 
real(rp) :: Ey 
real(rp) :: Ez 

real(rp) :: Bx 
real(rp) :: By  
real(rp) :: Bz

real(rp) :: v2, g, ux, uy, uz, ux_min, uy_min, uz_min 
real(rp) :: tx, ty, tz, t2, sx, sy, sz
real(rp) :: ux_pr, uy_pr, uz_pr, ux_plus, uy_plus, uz_plus
real(rp) :: u2, gamma, x1, y1, z1

integer :: i
integer :: ierr

integer :: Nt
real(rp) :: dt

real(rp) :: qom, moq

real(rp), dimension(:), allocatable :: T, XX, YY, ZZ, UUX, UUY, UUZ

namelist /input_file/ moq, Nt, dt, x, y, z, vx, vy, vz, Ex, Ey, Ez, Bx, By, Bz  

!======================================================================
!======================================================================
!======================================================================
!======================================================================

! Allocate memory
allocate(T(Nt), stat=ierr)
if (ierr /= 0) then
   print*,  'Failing to allocate memory for array T'
   stop
end if

allocate(XX(Nt), stat=ierr)
if (ierr /= 0) then
   print*,  'Failing to allocate memory for array XX'
   stop
end if

allocate(YY(Nt), stat=ierr)
if (ierr /= 0) then
   print*,  'Failing to allocate memory for array YY'
   stop
end if

allocate(ZZ(Nt), stat=ierr)
if (ierr /= 0) then
   print*,  'Failing to allocate memory for array ZZ'
   stop
end if

allocate(UUX(Nt), stat=ierr)
if (ierr /= 0) then
   print*,  'Failing to allocate memory for array UUX'
   stop
end if

allocate(UUY(Nt), stat=ierr)
if (ierr /= 0) then
   print*,  'Failing to allocate memory for array UUY'
   stop
end if

allocate(UUZ(Nt), stat=ierr)
if (ierr /= 0) then
   print*,  'Failing to allocate memory for array UUZ'
   stop
end if

print*, 'after allocation'

v2 = vx**2 + vy**2 + vz**2
g  = 1 / (1-v2)**0.5

ux = g*vx
uy = g*vy 
uz = g*vz

T = 0.0_dp
XX = 0.0_dp
YY = 0.0_dp
UUX = 0.0_dp
UUY = 0.0_dp
UUZ = 0.0_dp

XX(1)  = x 
YY(1)  = y 
ZZ(1)  = z
UUX(1) = ux 
UUY(1) = uy 
UUZ(1) = uz


do i = 2, Nt

   print*, i

   ux_min = ux + qom * Ex * dt/2
   uy_min = uy + qom * Ey * dt/2
   uz_min = uz + qom * Ez * dt/2

   tx = - (qom*(1/c))* Bx * dt/(2*g)
   ty = - (qom*(1/c))* By * dt/(2*g)
   tz = - (qom*(1/c))* Bz * dt/(2*g)
   t2 = (tx**2 + ty**2 + tz**2)

   sx = 2*tx/(1+t2)
   sy = 2*ty/(1+t2)
   sz = 2*tz/(1+t2)

   !--Rotate the result with half magnetic impulse
   ux_pr = ux_min + (uy_min*tz - uz_min*ty)
   uy_pr = uy_min - (ux_min*tz - uz_min*tx)
   uz_pr = uz_min + (ux_min*ty - uy_min*tx)

   !--Rotate the result with full magnetic impulse
   ux_plus = ux_min + (uy_min*sz - uz_min*sy)
   uy_plus = uy_min - (ux_min*sz - uz_min*sx)
   uz_plus = uz_min + (ux_min*sy - uy_min*sx)

   !---Add remaining electric impulse
   ux = ux_plus + qom * Ex * dt/2
   uy = uy_plus + qom * Ey * dt/2
   uz = uz_plus + qom * Ez * dt/2
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

end program BorisPusher
