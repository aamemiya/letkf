program rnet
use common
use common_rsv_rnet

implicit NONE

integer,parameter::nx=8
integer,parameter::nr=2000

real(r_size),parameter::dt=0.10
integer,parameter::nt_train=5000
integer,parameter::nt_sync=400
integer,parameter::nt_test=200

integer,parameter::nrdim_ave=3
real(r_size),parameter::vrho_in=0.4
real(r_size),parameter::vsig_in=0.15
real(r_size),parameter::vreg_in=0.02

real(r_size)::x_train(nx,nt_train)
real(r_size)::x(nx)
real(r_sngl)::x4(nx)
real(r_sngl)::x4_t(nx)

real(r_size)::time
integer::it,i

integer::istat,idnci,idnco,idvvi,idvvo,iddx,iddt,idvx,idvt


include 'netcdf.inc'

call rsv_rnet_init(nx,nr,nr+1,nrdim_ave,vrho_in,vsig_in,sub_basis_linear,vreg_in)

  istat=NF_OPEN('train.nc',NF_NOWRITE,idnci)
  istat=NF_INQ_VARID(idnci,'v',idvvi)

  istat=NF_CREATE('fcst.nc',NF_CLOBBER,idnco)
  istat=NF_DEF_DIM(idnco,'x',nx,iddx)
  istat=NF_DEF_DIM(idnco,'t',NF_UNLIMITED,iddt)
  istat=NF_DEF_VAR(idnco,'x',NF_FLOAT,1,iddx,idvx)
  istat=NF_DEF_VAR(idnco,'t',NF_FLOAT,1,iddt,idvt)
  istat=NF_DEF_VAR(idnco,'vx',NF_FLOAT,2,(/iddx,iddt/),idvvo)
  istat=NF_ENDDEF(idnco)
  istat=NF_PUT_VARA_REAL(idnco,idvx,1,nx,(/( real(i), i=1,nx )/))



 time=0.0
! read(10)  x4
do it=1,nt_train
 time=time+dt
 istat=NF_GET_VARA_REAL(idnci,idvvi,(/1,it/),(/nx,1/),x4)
! read(10)  x4
 x_train(1:nx,it)=dble(x4)
end do

istat=NF_CLOSE(idnci)

write(*,*)'train start'
call rsv_rnet_train_batch(nt_train,x_train) 
write(*,*)'train end'


istat=NF_OPEN('test.nc',NF_NOWRITE,idnci)
istat=NF_INQ_VARID(idnci,'v',idvvi)


do it=1,nt_sync
 istat=NF_GET_VARA_REAL(idnci,idvvi,(/1,it/),(/nx,1/),x4)
! read(30) x4
 istat=NF_PUT_VARA_REAL(idnco,idvvo,(/1,it/),(/nx,1/),x4)
! write(90) x4
 x=real(x4,r_size)
 call rsv_rnet_input(x)
end do

x(1:nx)=real(x4,r_size)

do it=nt_sync+1,nt_sync+nt_test
 call rsv_rnet_fcst(x,x)
 x4=real(x,r_sngl)
! write(*,*) x4
 istat=NF_GET_VARA_REAL(idnci,idvvi,(/1,it/),(/nx,1/),x4_t)
! read(30)  x4_t
 write(*,*) sqrt(sum((x4_t-x4)**2)/real(nx))
 istat=NF_PUT_VARA_REAL(idnco,idvvo,(/1,it/),(/nx,1/),x4)
! write(90) x4(1:nx)
end do

!close(30)
!close(90)

istat=NF_CLOSE(idnci)
istat=NF_CLOSE(idnco)


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
