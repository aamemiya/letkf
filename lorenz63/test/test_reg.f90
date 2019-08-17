!===============================================!
module common_reg
use common
use common_mtx

implicit NONE

integer,save,private::nx,np,ny

real(r_size),allocatable,private::w(:,:)
real(r_size),save,private::reg

logical,save,private::flag_trained=.false.

procedure(),pointer :: x_to_p => null()

contains 
!------------------------------------------------!
subroutine reg_init(nx_in,np_in,ny_in,sub_in,reg_in)
integer,intent(in)::nx_in,np_in,ny_in
external sub_in
real(r_size),intent(IN),optional::reg_in

x_to_p=>sub_in
nx=nx_in
ny=ny_in
np=np_in

allocate(w(1:ny,1:np))

if (.not.present(reg_in)) then
 reg=0.0
else
 reg=reg_in
end if

return
end subroutine reg_init
!------------------------------------------------!
subroutine reg_train_batch(ndim,x_in,y_in)
implicit NONE
integer,intent(IN)::ndim
real(r_size),intent(IN)::x_in(:,:)
real(r_size),intent(IN)::y_in(:,:)

real(r_size),allocatable::pwork(:,:),mat_w1(:,:),mat_w2(:,:)
integer::iy,ip,jp,idim


if(ubound(x_in,1).ne.nx.or.ubound(y_in,1).ne.ny)then
 write(*,*) 'error::dimension mismatch nx or ny'
 stop
end if
if(ubound(x_in,2).ne.ndim.or.ubound(y_in,2).ne.ndim)then
 write(*,*) 'error::dimension mismatch ndim'
 stop
end if

allocate(pwork(np,ndim),mat_w1(np,np),mat_w2(np,ny))

do idim=1,ndim
 call x_to_p(x_in(:,idim),pwork(:,idim)) 
end do

mat_w1=0.0
mat_w2=0.0

do ip=1,np
do jp=1,np
 mat_w1(ip,jp)=sum(pwork(ip,1:ndim)*pwork(jp,1:ndim)) + reg*dble(iddl(ip,jp))
end do
end do

if (np.eq.0)then
   mat_w1 = 1.0/mat_w1
else
   call mtx_inv(np,mat_w1,mat_w1)
end if

do ip=1,np
do iy=1,ny
  mat_w2(ip,iy)=sum(pwork(ip,1:ndim)*y_in(iy,1:ndim))
end do
end do

w = transpose(matmul(mat_w1,mat_w2))

flag_trained=.true.

deallocate(pwork,mat_w1,mat_w2)

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
subroutine reg_fcst(x_in,y_out)
implicit NONE
real(r_size),intent(IN)::x_in(:)
real(r_size),intent(OUT)::y_out(:)
real(r_size),allocatable::pwork(:)
integer::i

if (.not.flag_trained) then
 write(*,*) 'call reg_train first.'
 stop
end if
if(ubound(x_in,1).ne.nx.or.ubound(y_out,1).ne.ny)then
 write(*,*) 'error::dimension mismatch nx or ny'
 stop
end if

allocate(pwork(np))
call x_to_p(x_in,pwork) 
do i=1,ny
 y_out(i)=sum(w(i,1:np)*pwork(1:np))
end do
deallocate(pwork)

return
end subroutine reg_fcst
!------------------------------------------------!
end module common_reg

