module common_rsv_rnet
use common
use common_mtx
use common_reg

implicit NONE

logical,save::flag_init=.false.

integer,save,private::nx,nr

real(r_size),save,private::rho=0.9
real(r_size),save,private::sig=0.2

real(r_size),allocatable,private::r(:)
real(r_size),allocatable,private::amat(:,:)
real(r_size),allocatable,private::rmat_in(:,:)


contains
!--------------------------------------------------------!

subroutine rsv_rnet_init(nx_in,nr_in,ave_dim_in,rho_in,sig_in,sub_basis,reg_in)
integer,intent(in)::nx_in,nr_in
integer,intent(in)::ave_dim_in
real(r_size),intent(in)::rho_in,sig_in,reg_in
real(r_size),allocatable::eigen_r(:),mat_dummy(:,:)
real(r_size),allocatable::rans1(:),rans2(:)
integer::ir,jr,iloc,idummy

external sub_basis

if (flag_init) return

nx=nx_in
nr=nr_in
sig=sig_in
rho=rho_in

allocate(r(nr),amat(nr,nr),rmat_in(nr,nx))

call reg_init(nr,nr+1,nx,sub_basis,reg_in)

allocate(eigen_r(nr),mat_dummy(nr,nr),rans1(nr),rans2(nr))

rmat_in=0.0
call com_rand(nr,rans1)
call com_rand(nr,rans2)
do ir=1,nr
 iloc=int(rans1(ir)*real(nx))+1
 rmat_in(ir,iloc)=sig*(2.0*rans2(ir)-1.0)
end do

amat=0.0
do jr=1,nr
call com_rand(ave_dim_in,rans1(1:ave_dim_in)) !!! location
call com_rand(ave_dim_in,rans2(1:ave_dim_in)) !!! value
do ir=1,ave_dim_in
 iloc=int(real(nr)*rans1(ir))+1
 do while (amat(iloc,jr).ne.0.0)
 call com_rand(1,rans1(ir))
 iloc=int(real(nr)*rans1(ir))+1
 end do
 amat(iloc,jr)=2.0*rans2(ir)-1.0
end do
end do

call com_rand(nr,rans1)
do ir=1,nr
 r(ir)=2.0*rans1(ir)-1.0 
end do


call mtx_eigen(0,nr,amat,eigen_r,mat_dummy,idummy)

amat(:,:)=amat(:,:)*rho/eigen_r(1)

flag_init=.true.

return

end subroutine rsv_rnet_init
!--------------------------------------------------------!
subroutine rsv_rnet_input(x_in)
real(r_size),intent(in)::x_in(:)
real(r_size),allocatable::dr_in(:),dr_self(:)
integer::ir

if(.not.flag_init)then
 write(*,*) 'call init first.'
 stop
end if

allocate(dr_in(nr),dr_self(nr))

 do ir=1,nr
  dr_in(ir)  =sum(rmat_in(ir,:)*x_in(:))
  dr_self(ir)=sum(amat(ir,:)*r(:)) 
 end do

 do ir=1,nr 
  r(ir)  =tanh(dr_in(ir)+dr_self(ir))
 end do

deallocate(dr_in,dr_self)

return
end subroutine rsv_rnet_input
!--------------------------------------------------------!
subroutine rsv_rnet_train_batch(nsmp,x_smp)
integer,intent(in)::nsmp
real(r_size),intent(in)::x_smp(:,:)

real(r_size),allocatable::train_smp(:,:)
real(r_size),allocatable::r_smp(:,:)
integer::ismp

if(.not.flag_init)then
 write(*,*) 'call init first.'
 stop
end if

allocate(train_smp(nx,nsmp-1),r_smp(nr,nsmp-1))
 
do ismp=1,nsmp-1
 call rsv_rnet_input(x_smp(:,ismp)) 
 train_smp(:,ismp)=x_smp(:,ismp+1)
 r_smp(:,ismp)=r
end do


call reg_train_batch(nsmp-1,r_smp,train_smp)

deallocate(train_smp,r_smp)

return
end subroutine rsv_rnet_train_batch
!--------------------------------------------------------!
subroutine rsv_rnet_fcst(x_in,x_out)
real(r_size),intent(in)::x_in(:)
real(r_size),intent(out)::x_out(:)

call rsv_rnet_input(x_in) 
call reg_fcst(r,x_out)

return
end subroutine rsv_rnet_fcst
!--------------------------------------------------------!

end module common_rsv_rnet
!=========================================!
