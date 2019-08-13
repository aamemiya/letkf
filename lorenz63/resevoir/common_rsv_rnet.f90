module common_rsv_rnet
use common
use common_mtx
implicit real(a-h,o-z)

logical,save::flag_init=.false.

integer,save::nx=10
integer,save::nr=200
integer,save::np=200
real(r_size),save::vrho=0.9
real(r_size),save::vsig=0.2

real(r_size),allocatable,save::r(:)
real(r_size),allocatable,save::amat(:,:)
real(r_size),allocatable,save::rmat_in(:,:)
real(r_size),allocatable,save::rmat_out(:,:)
real(r_size),allocatable,save::rconst_out(:) 

contains

subroutine rsv_rnet_init(nx_in,nr_in,np_in,vrho_in,vsig_in)
integer,intent(in)::nx_in,nr_in,np_in
real(r_size),intent(in)::vrho_in,vsig_in

real(r_size),allocatable::eigen_r(:),vmat_dummy(:,:)

if (flag_init) return

nx=nx_in
nr=nr_in
np=np_in
vsig=vsig_in
vrho=vrho_in

allocate(amat(nr,nr),rmat_in(nr,nx),rmat_out(nx,np),rconst_out(nx),eigen_r(nr),vmat_dummy(nr,nr))


call random_seed

do ir=1,nr
 call random_number(ran)
 iloc=int(ran*real(nx))+1
 call random_number(ran)
 rmat_in(ir,iloc)=vsig*(2.0*ran-1.0)
end do

do ir=1,nr
do jr=1,nr
 call random_number(ran)
 amat(ir,jr)=2.0*ran-1.0
end do
end do

do idim=1,ndim
 call random_number(ran)
 r(idim)=2.0*ran-1.0 
end do


call mtx_eigen(0,nr,amat,eigen_r,vmat_dummy,idummy)

amat(:,:)=amat(:,:)*vrho/eigen_r(1)

flag_init=.true.

return
end subroutine rsv_rnet_init

subroutine rsv_rnet_input(nx,x_in)
integer,intent(in)::nx
real(r_size),intent(in)::x_in(nx)
real(r_size)::dr_in,dr_self

 do ir=1,nr
  dr_in  =sum(rmat_in(ir,:)*x_in(:))
  dr_self=sum(amat(ir,:)*r(:)) 
  r(ir)  =tanh(dr_in+dr_self)
 end do

return
end subroutine rsv_rnet_input

subroutine rsv_rnet_train_batch(nx,nsmp,x_smp,f_out)
integer,intent(in)::nx,nsmp
real(r_size),intent(in)::x_smp(nx,nsmp)

real(r_size)::train_smp(nx,nsmp-1)
real(r_size)::p_smp(np,nsmp-1)

do ismp=1,nsmp-1
 call rsv_rnet_input(nx,x_smp(:,ismp)) 
 train_smp(:,ismp)=x_smp(:,ismp+1)
 p_smp(:,ismp)= f_out(r)
end do

call regression_id_batch(np,nx,nsmp-1,p_smp,train_smp,rmat_out,rconst_out,1.0)

return
end subroutine rsv_rnet_train_batch

subroutine rsv_rnet_fcst(nx,x_in,x_out,f_out)
integer,intent(in)::nx
real(r_size),intent(in)::x_in(nx)
real(r_size),intent(out)::x_out(nx)

real(r_size)::p_temp(np)

call rsv_rnet_input(nx,x_in) 
p_temp=f_out(r)
do ix=1,nx
 x_out(ix)=sum(rmat_out(ix,:)*p_temp(:))+rconst_out(ix)
end do

return
end subroutine rsv_rnet_fcst

end module common_rsv_rnet
!=========================================!
