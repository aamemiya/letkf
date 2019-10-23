!==========================================!
module setup 
implicit real(a-h,o-z)

real(4),parameter::dt=0.005
real(4),parameter::a=10.0
real(4),parameter::b=28.0
real(4),parameter::c=8.0/3.0

character*30::cfile_nature="../DATA/test.nc"
character*30::cfile_model="../DATA/fcst/fcst.nc"

integer,parameter::nx=8

integer,parameter::nt_fcst=100
integer,parameter::nt_sync=400
integer,parameter::nt_bufr=10
integer,parameter::nt=nt_sync+nt_fcst
integer,parameter::nt_draw=nt_bufr+nt_fcst
integer,parameter::nt_gap=nt_sync-nt_bufr
real(4)::vmin,vmax
real(4)::vtic
real(4)::v(nx,nt)
real(4)::vm(nx,nt)

integer,parameter::ixref=5

end module setup
!==========================================!
program main
use setup

call load 

vmin=-10.0
vmax=10.0
vtic=5.0

call draw

stop
end program main
!==========================================!
subroutine load
use setup

include 'netcdf.inc'


istat=NF_OPEN(trim(cfile_nature),NF_NOWRITE,idnc)
istat=NF_INQ_VARID(idnc,'v',idvv)
istat=NF_GET_VARA_REAL(idnc,idvv,(/1,1/),(/nx,nt/),v)
istat=NF_CLOSE(idnc)

istat=NF_OPEN(trim(cfile_model),NF_NOWRITE,idnc)
istat=NF_INQ_VARID(idnc,'v',idvv)
istat=NF_GET_VARA_REAL(idnc,idvv,(/1,1/),(/nx,nt/),vm)
istat=NF_CLOSE(idnc)

!write(*,*) maxval(v),minval(v)
!write(*,*) maxval(vm),minval(vm)
!stop

return
end subroutine load
!==========================================!
subroutine draw
use setup

astics=real(nt_draw)*dt/5.0
amtics=astics

iout=2
! *** general settings ***
      call sgiset ('IFONT',1)
      call swcmll
      call swlset ('LSEP',.TRUE.) ! psfilename numbering
      call swcset ('FNAME','figure')
      call swiset ('IWIDTH',1000) ! output window size
      call swiset ('IHEIGHT',800)     

    call swiset('IFL',1) !!! PNG
!   call swiset('IFL',2) !!! EPS
!    call swiset('IFL',4) !!! PDF

      call gropn(iout) 
      call sglset ('LFULL',.TRUE.) ! using fullsize
      call slmgn (0.0,0.0,0.0,0.0) ! margin 

      call grfrm 
      call grswnd (real(nt_gap+1)*dt,real(nt_sync+nt_fcst)*dt ,vmin,vmax) ! set window

       vpl=0.17; vpr=0.77; vpb=0.23 ; vpt=0.73
       call grsvpt (vpl,vpr,vpb,vpt) ! set viewport

      call grstrn (1) ! linear or log
      call grstrf

! **** Lines & markers ****
      call sglset ('LCLIP',.TRUE.) ! Cliping

      call uuslnt(1)
      call uuslni(23)
      call uulin (nt_draw,(/(real(nt_gap+i)*dt, i=1,nt_draw)/),v(ixref,nt_gap+1:nt))
!      call uuslni(43)
!      call uulin (nt_draw,(/(real(nt_gap+i)*dt, i=1,nt_draw)/),3.0*(x(nt_gap+1:nt)-y(nt_gap+1:nt)))
!      call uuslni(63)
!      call uulin (nt_draw,(/(real(nt_gap+i)*dt, i=1,nt_draw)/),z(nt_gap+1:nt))

      call uuslnt(3)
      call uuslni(23)
      call uulin (nt_draw,(/(real(nt_gap+i)*dt, i=1,nt_draw)/),vm(ixref,nt_gap+1:nt))
!      call uuslni(43)
!      call uulin (nt_draw,(/(real(nt_gap+i)*dt, i=1,nt_draw)/),3.0*(xm(nt_gap+1:nt)-ym(nt_gap+1:nt)))
!      call uuslni(63)
!      call uulin (nt_draw,(/(real(nt_gap+i)*dt, i=1,nt_draw)/),zm(nt_gap+1:nt))

      call uuslni(1)
      call uulin(2,(/real(nt_sync)*dt,real(nt_sync)*dt/),(/vmin,vmax/))

! **** x ,y axis ****
      call UYSFMT('(F5.1)')

      call uziset ('INDEXT2',5)
     ! call uziset ('INDEXL1',5)
      call uziset ('INNER',-1)
     ! call uzrset ('RSIZEL1',0.020)
      call uzrset ('RSIZEC1',0.020)
      call uzrset ('RSIZET2',0.010)
      call uzrset ('RSIZET1',0.004)


!      call uzlset ('LABELXB',.FALSE.)
      call uxaxdv ('B',astics,amtics)
      call uxaxdv ('T',astics,amtics)

      call uxsttl ('B','time',0.0)

      call uzlset ('LABELYR',.FALSE.)
      call uzlset ('LABELYL',.TRUE.)
      call uyaxdv ('L',vtic,vtic)
      call uyaxdv ('R',vtic,vtic)  
      call uziset ('IROTCYL',1)

     call uysttl('L','x',0.0)

      call sglset ('LCLIP',.FALSE.) ! Cliping
      call sgtxzv (0.47,0.76,'Lorenz 96',0.025,0,0,5) !


      call grcls


return
end subroutine draw

