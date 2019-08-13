PROGRAM spinup
  USE common
  USE KSmodel

  IMPLICIT NONE

  REAL(r_size) :: x(nx)
  INTEGER,PARAMETER ::nt=10000 
  integer::it
  dt=0.005d0

  CALL com_randn(nx,x)
  x = 0.2*(2.0 * x - 1.0d0)

do it=1,nt
  CALL tinteg_rk4(1,x,x) 
!  if(mod(it,100).eq.0) write(*,*) x(1:3)
end do
  WRITE(90) x

  STOP
END PROGRAM spinup
