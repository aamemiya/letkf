MODULE h_ope
!=======================================================================
! observation operator
!=======================================================================
  USE common
  USE lorenz96

  IMPLICIT NONE

!  INTEGER,PARAMETER :: ny=13
  INTEGER,PARAMETER :: ny=nx
  REAL(r_size),SAVE :: h(ny,nx)

CONTAINS
SUBROUTINE set_h(x)
  IMPLICIT NONE
  REAL(r_size),INTENT(IN) :: x(nx)
  INTEGER :: i
  INTEGER :: idx(nx)

 !!! identity matrix
  do i=1,ny
    idx(i)=i
  end do

  h = 0.0d0
  DO i=1,ny
    h(i,idx(i)) = 1.0d0
  END DO

  RETURN
END SUBROUTINE set_h

END MODULE h_ope
