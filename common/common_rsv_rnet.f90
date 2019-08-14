module common_rsv_rnet
use common
use common_mtx
use common_dim
use common_reg

implicit NONE

logical,save::flag_init=.false.

real(r_size),save,private::vrho=0.9
real(r_size),save,private::vsig=0.2

real(r_size),save::r(nr)
real(r_size),save::amat(nr,nr)
real(r_size),save::rmat_in(nr,nx)

real(r_size),parameter::reg=0.2d0

contains
!--------------------------------------------------------!

subroutine rsv_rnet_init(vrho_in,vsig_in)
real(r_size),intent(in)::vrho_in,vsig_in
real(r_size)::eigen_r(nr),vmat_dummy(nr,nr)
real(r_size)::rans1(nr)
real(r_size)::rans2(nr)
integer::ir,jr,iloc,idummy

if (flag_init) return

vsig=vsig_in
vrho=vrho_in

call com_rand(nr,rans1)
call com_rand(nr,rans2)
do ir=1,nr
 iloc=int(rans1(ir)*real(nx))+1
 rmat_in(ir,iloc)=vsig*(2.0*rans2(ir)-1.0)
end do

do ir=1,nr
call com_rand(nr,rans1)
do jr=1,nr
 amat(ir,jr)=2.0*rans1(ir)-1.0
end do
end do

call com_rand(nr,rans1)
do ir=1,nr
 r(ir)=2.0*rans1(ir)-1.0 
end do


call mtx_eigen(0,nr,amat,eigen_r,vmat_dummy,idummy)

amat(:,:)=amat(:,:)*vrho/eigen_r(1)

flag_init=.true.

return
end subroutine rsv_rnet_init
!--------------------------------------------------------!
subroutine rsv_rnet_input(x_in)
real(r_size),intent(in)::x_in(nx)
real(r_size)::dr_in,dr_self
integer::ir

 do ir=1,nr
  dr_in  =sum(rmat_in(ir,:)*x_in(:))
  dr_self=sum(amat(ir,:)*r(:)) 
  r(ir)  =tanh(dr_in+dr_self)
 end do

return
end subroutine rsv_rnet_input
!--------------------------------------------------------!
subroutine rsv_rnet_train_batch(nsmp,x_smp)
integer,intent(in)::nsmp
real(r_size),intent(in)::x_smp(nx,nsmp)

real(r_size)::train_smp(nx,nsmp-1)
real(r_size)::r_smp(nr,nsmp-1)
integer::ismp

do ismp=1,nsmp-1
 call rsv_rnet_input(x_smp(:,ismp)) 
 train_smp(:,ismp)=x_smp(:,ismp+1)
 r_smp(:,ismp)=r
end do

call reg_train_batch(nsmp-1,r_smp,train_smp,reg)

return
end subroutine rsv_rnet_train_batch
!--------------------------------------------------------!
subroutine rsv_rnet_fcst(x_in,x_out)
real(r_size),intent(in)::x_in(nx)
real(r_size),intent(out)::x_out(nx)

call rsv_rnet_input(x_in) 
call reg_fcst(r,x_out)

return
end subroutine rsv_rnet_fcst
!--------------------------------------------------------!

end module common_rsv_rnet
!=========================================!
