PROGRAM spinup
  USE common
  USE lorenz96

  IMPLICIT NONE

  REAL(r_size) :: xy(ntot)
  INTEGER :: i,ktoneday

  dt=0.005d0
  force=8.0d0
  oneday=0.2d0

  ktoneday = INT(oneday/dt)

  CALL com_randn(ntot,xy)
  xy(1:nx) = xy(1:nx) * 5.0d0
  xy(nx+1:ntot) = xy(nx+1:ntot) * 0.0d0

  CALL tinteg_rk4(ktoneday*360*100,xy,xy) ! 100 years integration

  WRITE(90) xy

  STOP
END PROGRAM spinup
