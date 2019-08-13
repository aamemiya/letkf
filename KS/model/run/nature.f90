PROGRAM nature
  USE common
  USE KSmodel

  IMPLICIT NONE

  INTEGER,PARAMETER :: nt=100000
  REAL(r_size) :: x(nx)
  REAL(r_sngl) :: x4(nx)
  INTEGER :: i,ktcyc
  REAL(r_size) ::dtsmp   

  dt=0.005d0
  dtsmp=0.25d0
  ktcyc=int(dtsmp/dt)

  READ(10) x
  DO i=1,nt/ktcyc
    x4 = x
    WRITE(90) x4
    CALL tinteg_rk4(ktcyc,x,x)
  END DO


  STOP
END PROGRAM nature
