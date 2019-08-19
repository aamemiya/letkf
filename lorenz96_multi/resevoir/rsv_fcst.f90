program rnet
use common
use common_rsv_rnet

implicit NONE

integer,parameter::nx=64
integer,parameter::nr=2000

real(r_size),parameter::dt=0.25
integer,parameter::nt_train=4000
integer,parameter::nt_sync=200
integer,parameter::nt_test=200

integer,parameter::nrdim_ave=3
real(r_size),parameter::vrho_in=0.4
real(r_size),parameter::vsig_in=1.0
real(r_size),parameter::vreg_in=0.20

real(r_size)::x_train(nx,nt_train)
real(r_size)::x(nx)
real(r_sngl)::x4(nx)
real(r_sngl)::x4_t(nx)

real(r_size)::time
integer::it

write(*,*) 'initialization'
call rsv_rnet_init(nx,nr,nrdim_ave,vrho_in,vsig_in,sub_basis_linear,vreg_in)

 time=0.0
 read(10)  x4
do it=1,nt_train
 time=time+dt
 read(10)  x4
 x_train(1:nx,it)=dble(x4)
end do

write(*,*) 'training'
call rsv_rnet_train_batch(nt_train,x_train) 

write(*,*) 'sync'
do it=1,nt_sync
 read(30) x4
 write(90) x4
 x=real(x4,r_size)
 call rsv_rnet_input(x)
end do

x(1:nx)=real(x4,r_size)

write(*,*) 'fcst'
do it=1,nt_test
 call rsv_rnet_fcst(x,x)
 x4=real(x,r_sngl)
! write(*,*) x4
 read(30)  x4_t
 write(*,*) sqrt(sum((x4_t-x4)**2)/real(nx)) !!!! RMSE quicklook
 write(90) x4(1:nx)
end do

close(30)
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
