program rnet
use common
use common_rsv_rnet

implicit NONE

integer,parameter::nx=3
integer,parameter::nr=500


real(r_size),parameter::dt=0.10
integer,parameter::nt=1000
integer,parameter::nsmp=800

integer,parameter::nrdim_ave=3
real(r_size),parameter::vrho_in=0.9
real(r_size),parameter::vsig_in=0.15
real(r_size),parameter::vreg_in=0.20

real(r_size)::xsmp(nx,nsmp)
real(r_size)::x(nx)
real(r_sngl)::x4(nx)
real(r_sngl)::x4_t(nx)

real(r_size)::time
integer::it

!initialize a and r

call rsv_rnet_init(nx,nr,nr+1,nrdim_ave,vrho_in,vsig_in,sub_basis_linear,vreg_in)

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
! write(*,*) x4
 read(10)  x4_t
 write(*,*) sqrt(sum((x4_t-x4)**2)/real(nx))
 write(90) x4(1:nx)
end do

close(10)
close(90)

stop

contains

subroutine sub_basis_linear(vr,vp)
real(r_size),intent(IN)::vr(nr)
real(r_size),intent(out)::vp(nr+1)
integer::ir
do ir=1,nr-1,2
 vp(ir)=vr(ir)
end do
do ir=2,nr,2
 vp(ir)=vr(ir)**2
end do
 vp(nr+1)=1.0 !!! constant component
return
end subroutine sub_basis_linear

end program rnet
!=========================================!
