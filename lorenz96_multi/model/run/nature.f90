PROGRAM nature
  USE common
  USE lorenz96

  IMPLICIT NONE

  INTEGER,PARAMETER :: ndays=3600 ! 10 years
  REAL(r_size) :: xy(ntot)
  REAL(r_sngl) :: x4(nx),y4(nxx)
  INTEGER :: i,ktoneday
  INTEGER :: ktcyc

  INTEGER :: istat, idnc, iddx, idvx, iddt, idvt, idvv, idvw

  REAL(r_sngl) :: time

  include 'netcdf.inc'

  dt=0.005d0
  force=8.0d0
  oneday=0.2d0

  ktoneday = INT(oneday/dt)
  ktcyc = ktoneday/4

  time=0.0e0

  istat=NF_OPEN('init_nature.nc',NF_NOWRITE,idnc)
  istat=NF_INQ_VARID(idnc,'v',idvv)
  istat=NF_INQ_VARID(idnc,'w',idvw)
  istat=NF_GET_VAR_REAL(idnc,idvv,x4)
  istat=NF_GET_VAR_REAL(idnc,idvw,y4)
  istat=NF_CLOSE(idnc)
 
  xy(1:nx)=real(x4,r_size)
  xy(nx+1:ntot)=real(y4,r_size)


!  READ(10) xy

  istat=NF_CREATE('nature.nc',NF_CLOBBER,idnc)
  istat=NF_DEF_DIM(idnc,'x',nx,iddx)
  istat=NF_DEF_DIM(idnc,'t',NF_UNLIMITED,iddt)
  istat=NF_DEF_VAR(idnc,'x',NF_FLOAT,1,iddx,idvx)
  istat=NF_DEF_VAR(idnc,'t',NF_FLOAT,1,iddt,idvt)
  istat=NF_DEF_VAR(idnc,'v',NF_FLOAT,2,(/iddx,iddt/),idvv)
  istat=NF_ENDDEF(idnc)
  istat=NF_PUT_VARA_REAL(idnc,idvx,1,nx,(/( real(i), i=1,nx )/))



  DO i=1,ndays*4
!    xy4 = xy
!    WRITE(90) xy4(1:nx)
!    WRITE(91) xy4(nx+1:ntot)
  x4=real(xy(1:nx),r_sngl)
  istat=NF_PUT_VARA_REAL(idnc,idvt,i,1,time)
  istat=NF_PUT_VARA_REAL(idnc,idvv,(/1,i/),(/nx,1/),x4)
    CALL tinteg_rk4(ktcyc,xy,xy)
  time=time+real(dt)
    if (mod(i,1000).eq.0) WRITE(*,*) i,x4(1:3)
  END DO

  istat=NF_CLOSE(idnc)

  STOP
END PROGRAM nature
