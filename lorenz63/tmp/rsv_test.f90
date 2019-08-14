program rnet
use common
use common_dim
use common_rsv_rnet

implicit NONE

real(r_size),parameter::dt=0.10
integer,parameter::nt=2000
integer,parameter::nsmp=1000

real(r_size),parameter::vrho_in=0.4
real(r_size),parameter::vsig_in=0.15

real(r_size)::xsmp(nx,nsmp)
real(r_size)::x(nx)
real(r_sngl)::x4(nx)

real(r_size)::time
integer::it

!initialize a and r

call rsv_rnet_init(vrho_in,vsig_in)

 time=0.0
 read(10)  x4
 write(90) x4
do it=1,nsmp
 time=time+dt

 read(10)  x4
 write(90) x4
  xsmp(1:nx,it)=dble(x4)
end do

call rsv_rnet_train_batch(nsmp,xsmp) 

x(1:nx)=xsmp(1:nx,nsmp)

do it=nsmp+1,nt
 call rsv_rnet_fcst(x,x)
 x4=real(x)
 write(90) x4(1:nx)
end do

close(10)
close(90)


stop
end program rnet
!=========================================!
