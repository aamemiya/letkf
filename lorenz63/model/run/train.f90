PROGRAM train_data
  USE common
  USE lorenz63

  IMPLICIT NONE

  INTEGER,PARAMETER :: nt_train=100000
  REAL(r_size) :: x(nx)
  REAL(r_sngl) :: x4(nx)
  INTEGER :: i,ktcyc
 
  ktcyc=1

  READ(10) x

  DO i=1,nt_train
    x4 = x
    WRITE(90) x4(1:nx)
    CALL tinteg_rk4(ktcyc,x,x)
    if (mod(i,100).eq.0) WRITE(*,*) i,x4
  END DO

  STOP
END PROGRAM train_data
