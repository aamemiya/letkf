PROGRAM spinup
  USE common
  USE lorenz96
!  USE lorenz96_oro

  IMPLICIT NONE

  REAL(r_size) :: x(nx)
  REAL(r_sngl) :: x4(nx)
  INTEGER :: i,ktoneday
  INTEGER :: istat, idnc, iddx, idvx, idvv

  include 'netcdf.inc'

  dt=0.005d0
  force=8.0d0
  oneday=0.2d0

  ktoneday = INT(oneday/dt)

  istat=NF_CREATE('init.nc',NF_CLOBBER,idnc)
  istat=NF_DEF_DIM(idnc,'x',nx,iddx)
  istat=NF_DEF_VAR(idnc,'x',NF_FLOAT,1,iddx,idvx)
  istat=NF_DEF_VAR(idnc,'v',NF_FLOAT,1,iddx,idvv)
  istat=NF_ENDDEF(idnc)
  istat=NF_PUT_VARA_REAL(idnc,idvx,1,nx,(/( real(i), i=1,nx )/))

  CALL com_randn(nx,x)
  x = x * 5.0d0

  CALL tinteg_rk4(ktoneday*360*100,x,x) ! 100 years integration


!  WRITE(90) x
  x4=real(x,r_sngl)
  istat=NF_PUT_VARA_REAL(idnc,idvv,1,nx,x4)
  istat=NF_CLOSE(idnc)

  STOP
END PROGRAM spinup
