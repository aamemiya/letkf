program rnet
use common
use common_mtx
implicit real(a-h,o-z)

integer,parameter::ndim=200
real(4)::r(ndim)

integer,parameter::nt=10000
integer,parameter::nsmp=1000
real(4),parameter::vrho=0.9
real(4)::amat(ndim,ndim)
real(4)::rmat_in(ndim,3)
real(4)::rmat_out(3,ndim)
real(4)::rmat_out_const(3)
real(4)::dr_in(ndim)
real(4)::dr_self(ndim)

real(4)::train_smp(3,nsmp)
real(4)::r_trans_smp(ndim,nsmp)

real(4),parameter::sig=0.2

real(r_size)::eigen_r(ndim),eigen_i(ndim)
real(r_size)::vmat_dummy(ndim,ndim)

character*40::cfile_train='../DATA/nature.dat'
character*40::cfile_init='../DATA/spinup/init.dat'
character*40::cfile_fcst='../DATA/fcst/fcst.dat'

real(4)::fcst_temp(3)

!initialize a and r
call random_seed

do idim=1,ndim
 call random_number(ran)
 iloc=int(ran/3.0)+1
 call random_number(ran)
 rmat_in(idim,iloc)=sig*(2.0*ran-1.0)
end do

do idim=1,ndim
do jdim=1,ndim
 call random_number(ran)
 amat(idim,jdim)=2.0*ran-1.0
end do
end do

do idim=1,ndim
 call random_number(ran)
 r(idim)=0.2*ran-0.1 
end do

!call real_eigen(ndim,amat,eigen_r,eigen_i)

call mtx_eigen(0,ndim,dble(amat),eigen_r,vmat_dummy,idummy)

amat(:,:)=amat(:,:)*vrho/eigen_r(1)

open(11,file=trim(cfile_train),form='unformatted') 
open(12,file=trim(cfile_fcst),form='unformatted') 

write(*,'(10F8.3)') r(1:3)

 time=0.0
 read(11) x,y,z
 write(12) x,y,z
do it=1,nsmp
 time=time+dt

 do idim=1,ndim
  dr_in(idim)=x*rmat_in(idim,1)+y*rmat_in(idim,2)+z*rmat_in(idim,3)
  dr_self(idim)=sum(amat(idim,:)*r(:)) 
 end do
 do idim=1,ndim
  r(idim)=tanh(dr_in(idim)+dr_self(idim))
 end do

 if (mod(it,100).eq.0) write(*,'(10F8.3)') r(1:3)
 read(11) x,y,z
 write(12)x,y,z

! if (it.gt.nt-nsmp) then
!  itd=it-(nt-nsmp)
  r_trans_smp(1:ndim:2,it) = r(1:ndim:2)
  r_trans_smp(2:ndim:2,it) = r(2:ndim:2)**2
  train_smp(1:3,it)=(/x,y,z/)
! end if
end do

!close(11)

!!!! regression

call regression_id_batch(ndim,3,nsmp,r_trans_smp,train_smp,rmat_out,rmat_out_const,1.0)

do ismp=1,nsmp,10
 do j=1,3
  fcst_temp(j)=sum(rmat_out(j,:)*r_trans_smp(:,ismp))+rmat_out_const(j)
 end do
 write(*,*) ismp, sqrt(sum((fcst_temp(:)-train_smp(:,ismp))**2))
end do

!!!! prediction

!open(11,file=trim(cfile_init),form='unformatted')
! read(11) x,y,z
!close(11) 


call fcst

close(11)
close(12)


stop

contains

subroutine fcst

real(4)::vmat(3)
real(4)::r_trans(ndim)


do it=nsmp+1,nt-1
 time=time+dt
 do idim=1,ndim
  dr_in(idim)=x*rmat_in(idim,1)+y*rmat_in(idim,2)+z*rmat_in(idim,3)
  dr_self(idim)=sum(amat(idim,:)*r(:)) 
 end do
 do idim=1,ndim
  r(idim)=tanh(dr_in(idim)+dr_self(idim))
  if (mod(idim,2).eq.1) r_trans(idim)=r(idim)
  if (mod(idim,2).eq.0) r_trans(idim)=r(idim)**2
 end do

 x=sum(rmat_out(1,:)*r_trans(:))+rmat_out_const(1)
 y=sum(rmat_out(2,:)*r_trans(:))+rmat_out_const(2)
 z=sum(rmat_out(3,:)*r_trans(:))+rmat_out_const(3)

 read(11) xn,yn,zn
 if (mod(it,100).eq.0) write(*,*) it, sqrt(sum(((/x,y,z/)-(/xn,yn,zn/))**2))

 write(12)x,y,z
end do

return
end subroutine fcst

end program rnet
!=========================================!
