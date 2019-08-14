!===============================================!
module common_reg
use common
use common_mtx
use common_dim

implicit NONE

real(r_size),save,private::w(nx,np)
real(r_size),save,private::b(nx)
real(r_size),save,private::r

logical,save::flag_trained=.false.

contains 
!------------------------------------------------!
subroutine reg_train_batch(ndim,r_in,x_in,reg_in)
implicit NONE
integer,intent(IN)::ndim
real(r_size),intent(IN)::r_in(nr,ndim)
real(r_size),intent(IN)::x_in(nx,ndim)
real(r_size),intent(IN),optional::reg_in
real(r_size)::pwork(0:np,ndim)
real(r_size)::wb(0:np,nx)
real(r_size)::vmat_w1(0:np,0:np)
real(r_size)::vmat_w2(0:np,nx)
integer::ix,ip,jp,idim

if (.not.present(reg_in)) then
 r=0.0
else
 r=reg_in
end if

do idim=1,ndim
 pwork(1:np,idim)=phi(r_in(1:nr,idim))
end do
 pwork(0,:)=1.0

vmat_w1=0.0
vmat_w2=0.0

do ip=0,np
do jp=0,np
 vmat_w1(ip,jp)=sum(pwork(ip,1:ndim)*pwork(jp,1:ndim)) + r*dble(iddl(ip,jp))
end do
end do

if (np.eq.0)then
   vmat_w1 = 1.0/vmat_w1
else
   call mtx_inv(1+np,vmat_w1,vmat_w1)
end if

do ip=0,np
do ix=1,nx
  vmat_w2(ip,ix)=sum(pwork(ip,1:ndim)*x_in(ix,1:ndim))
end do
end do

wb = matmul(vmat_w1,vmat_w2)

b=wb(0,1:nx)
w=transpose(wb(1:np,1:nx))

flag_trained=.true.

return
contains !=========!
 integer function iddl(ix,iy)
 integer::ix,iy
   if (ix.eq.iy) then
      iddl=1
   else
      iddl=0
   end if
 end function iddl

end subroutine reg_train_batch 
!------------------------------------------------!
subroutine reg_fcst(r_in,x_out)
implicit NONE
real(r_size),intent(IN)::r_in(nr)
real(r_size),intent(OUT)::x_out(nx)
integer::ix

if (.not.flag_trained) then
 write(*,*) 'call reg_train first.'
 stop
end if

do ix=1,nx
 x_out(ix)=sum(w(ix,1:np)*phi(r_in))+b(ix)
end do

return
end subroutine reg_fcst
!------------------------------------------------!
end module common_reg

