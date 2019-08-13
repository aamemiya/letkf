!===============================================!
subroutine regression_id_batch(nx,ny,nsmp,x,y,w,b,r)
implicit real(a-h,o-z)

integer,intent(in)::nx,ny,nsmp
real(4),intent(in)::x(nx,nsmp)
real(4),intent(in)::y(ny,nsmp)
real(4),intent(out)::w(ny,nx)
real(4),intent(out)::b(ny)
real(4),intent(in),optional::r

real(4)::wb(0:nx,ny)

real(4)::vmat_w1(0:nx,0:nx)
real(4)::vmat_w2(0:nx,ny)

real(4)::p(0:nx,nsmp)

if (.not.present(r)) then
 reg=0.0
else
 reg=r
end if

p(1:nx,1:nsmp) = x(1:nx,1:nsmp)
p(0,1:nsmp)=1.0

vmat_w1=0.0
vmat_w2=0.0

do ij=0,nx
do ijj=0,nx
 vmat_w1(ij,ijj)=sum(p(ij,1:nsmp)*p(ijj,1:nsmp)) + reg*real(iddl(ij,ijj))
end do
end do

if (nx.eq.0)then
   vmat_w1 = 1.0/vmat_w1
else
   call mtx_inv(1+nx,vmat_w1,vmat_w1)
end if

do ij=0,nx
do iy=1,ny
  vmat_w2(ij,iy)=sum(p(ij,1:nsmp)*y(iy,1:nsmp))
end do
end do

wb = matmul(vmat_w1,vmat_w2)

b=wb(0,1:ny)
w=transpose(wb(1:nj,1:ny))

return
contains !=========!
 integer function iddl(ix,iy)
   if (ix.eq.iy) then
      iddl=1
   else
      iddl=0
   end if
 end function iddl

end subroutine regression_id_batch 
