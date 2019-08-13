program rnet
use common
use common_rsv_rnet

implicit real(a-h,o-z)

integer,parameter::nx=3
real(4)::x_smp(nx,nsmp)

integer,parameter::ndim=200
real(4)::r(ndim)

integer,parameter::nt=10000
integer,parameter::nsmp=1000

character*40::cfile_train='../DATA/nature.dat'
character*40::cfile_init='../DATA/spinup/init.dat'
character*40::cfile_fcst='../DATA/fcst/fcst.dat'

real(4)::fcst_temp(3)

!initialize a and r

call rsv_rnet_init(nx,ndim,ndim,0.4,0.15)


open(11,file=trim(cfile_train),form='unformatted') 
open(12,file=trim(cfile_fcst),form='unformatted') 

 time=0.0
 read(11) x,y,z
 write(12) x,y,z
do it=1,nsmp
 time=time+dt

 read(11) x,y,z
 write(12)x,y,z
  xsmp(1:3,it)=(/x,y,z/)
end do

call rsv_rnet_train_batch(nx,nsmp,xsmp,f_out) 

do it=nsmp+1,nt
 call rsv_rnet_fcst(nx,xfcst,xfcst)
 write(12) xfcst(1:nx)
end do

close(11)
close(12)


stop
end program rnet
!=========================================!
real function f_out()


end function f_out
