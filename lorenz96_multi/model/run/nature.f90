PROGRAM nature
  USE common
  USE lorenz96
!  USE lorenz96_oro

  IMPLICIT NONE

  INTEGER,PARAMETER :: ndays=3600 ! 10 years
  REAL(r_size) :: xy(nxy)
  REAL(r_sngl) :: xy4(nxy)
  INTEGER :: i,ktoneday
  INTEGER :: ktcyc

  dt=0.005d0
  force=8.0d0
  oneday=0.2d0

  ktoneday = INT(oneday/dt)
  ktcyc = ktoneday/4

  READ(10) xy

  DO i=1,ndays*4
    xy4 = xy
    WRITE(90) xy4(1:nx)
    WRITE(91) xy4(nx+1:nxy)
    CALL tinteg_rk4(ktcyc,xy,xy)
    if (mod(i,100).eq.0) WRITE(*,*) i,xy4(1:3)
  END DO

  STOP
END PROGRAM nature
