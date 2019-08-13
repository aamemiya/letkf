PROGRAM spinup
  USE common
  USE lorenz63

  IMPLICIT NONE

  REAL(r_size) :: x(nx)
  INTEGER :: i

  CALL com_randn(nx,x)
  x(1) = x(1) * 0.2d0
  x(2) = x(2) * 0.2d0
  x(3) = b + x(3) * 0.2d0


!  do i=1,100
!   CALL tinteg_rk4(1,x,x)
!   write(*,*) x
!  end do
!stop
 CALL tinteg_rk4(10000,x,x)

  WRITE(90) x

  STOP
END PROGRAM spinup
