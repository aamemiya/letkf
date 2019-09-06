PROGRAM spinup
  USE common
  USE lorenz96

  IMPLICIT NONE

  REAL(r_size) :: xy(ntot)
  REAL(r_sngl) :: x4(nx), y4(nxx)
  INTEGER :: i,ktoneday

  INTEGER :: istat, idnc, iddx, idvx, iddy, idvy, idvv, idvw

  include 'netcdf.inc'

  dt=0.005d0
  force=14.0d0
  oneday=0.2d0

  ktoneday = INT(oneday/dt)


  istat=NF_CREATE('init_nature.nc',NF_CLOBBER,idnc)
  istat=NF_DEF_DIM(idnc,'x',nx,iddx)
  istat=NF_DEF_DIM(idnc,'y',nxx,iddy)
  istat=NF_DEF_VAR(idnc,'x',NF_FLOAT,1,iddx,idvx)
  istat=NF_DEF_VAR(idnc,'y',NF_FLOAT,1,iddy,idvy)
  istat=NF_DEF_VAR(idnc,'v',NF_FLOAT,1,iddx,idvv)
  istat=NF_DEF_VAR(idnc,'w',NF_FLOAT,1,iddy,idvw)
  istat=NF_ENDDEF(idnc)
  istat=NF_PUT_VARA_REAL(idnc,idvx,1,nx,(/( real(i), i=1,nx )/))
  istat=NF_PUT_VARA_REAL(idnc,idvy,1,nxx,(/( 0.5+(real(i)-0.5)*real(nx)/real(nxx), i=1,nxx )/))



  CALL com_randn(ntot,xy)
  xy(1:nx) = xy(1:nx) * 5.0d0
  xy(nx+1:ntot) = xy(nx+1:ntot) * 0.0d0

  CALL tinteg_rk4(ktoneday*360*100,xy,xy) ! 100 years integration


  x4=real(xy(1:nx),r_sngl)
  y4=real(xy(nx+1:ntot),r_sngl)

!  WRITE(90) xy

  istat=NF_PUT_VARA_REAL(idnc,idvv,1,nx,x4)
  istat=NF_PUT_VARA_REAL(idnc,idvw,1,nxx,y4)

  istat=NF_CLOSE(idnc)

  STOP
END PROGRAM spinup
