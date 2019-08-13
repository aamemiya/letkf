MODULE lorenz63
!=======================================================================
!
! [PURPOSE:] Lorenz 1633 model
!
! [PUBLIC:]
!   SUBROUTINE tinteg_rk4(kt,xin,xout)
!
! [FIRST CREATED:] 08/13/2019 Arata Amemiy
!
! [HISTORY:]
!   08/13/2019 Arata Amemiya  Initial Creation
!
!=======================================================================
  USE common

  PRIVATE

  PUBLIC :: tinteg_rk4

  INTEGER,PARAMETER,PUBLIC :: nx=3                ! model dimension
  REAL(r_size),SAVE,PUBLIC :: dt=0.1d0       ! time of one time step
  REAL(r_size),SAVE,PUBLIC :: a=10.0d0       !
  REAL(r_size),SAVE,PUBLIC :: b=28.0d0       ! 
  REAL(r_size),SAVE,PUBLIC :: c=8.0d0/3.0d0  ! 
CONTAINS
!=======================================================================
! [1] Methods of Lorenz63
!=======================================================================
!-----------------------------------------------------------------------
! [1.1] Time integration of Lorenz63
!-----------------------------------------------------------------------
SUBROUTINE tinteg_rk4(kt,xin,xout)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: kt
  REAL(r_size),INTENT(IN)  :: xin(1:nx)
  REAL(r_size),INTENT(OUT) :: xout(1:nx)
  REAL(r_size),ALLOCATABLE :: x(:),xtmp(:),q1(:),q2(:),q3(:),q4(:)
  INTEGER :: k
!--[1.1.1] allocation --------------------------------------------------
  ALLOCATE( x(1:nx) )
  ALLOCATE( xtmp(1:nx) )
  ALLOCATE( q1(1:nx) )
  ALLOCATE( q2(1:nx) )
  ALLOCATE( q3(1:nx) )
  ALLOCATE( q4(1:nx) )
!--[1.1.2] time integration --------------------------------------------
  x(:) = xin(:)
!>>>>> TIME INTEGRATION START
  DO k=1,kt
    xtmp(:) = x(:)
    CALL lorenz63_core(xtmp,q1)
    xtmp(:) = x(:) + 0.5d0 * q1(:)
    CALL lorenz63_core(xtmp,q2)
    xtmp(:) = x(:) + 0.5d0 * q2(:)
    CALL lorenz63_core(xtmp,q3)
    xtmp(:) = x(:) + q3(:)
    CALL lorenz63_core(xtmp,q4)
    x(:) = x(:) + ( q1(:) + 2.0d0 * q2(:) + 2.0d0 * q3(:) + q4(:) ) / 6.0d0
  END DO
!<<<<< TIME INTEGRATION END
  xout(:) = x(:)
!--[1.1.3] tidy up -----------------------------------------------------
  DEALLOCATE( xtmp,q1,q2,q3,q4 )

  RETURN
END SUBROUTINE tinteg_rk4
!-----------------------------------------------------------------------
!=======================================================================
! [2] core part of Lorenz63
!=======================================================================
!--[2.1] NL ------------------------------------------------------------
SUBROUTINE lorenz63_core(xin,xout)
  IMPLICIT NONE

  REAL(r_size),INTENT(IN) :: xin(1:nx)
  REAL(r_size),INTENT(OUT) :: xout(1:nx)
  INTEGER :: i

  xout(1) = a * (xin(2)-xin(1))
  xout(2) = xin(1) * (b - xin(3)) - xin(2)
  xout(3) = xin(1) * xin(2) - c * xin(3)

  xout(:) = dt * xout(:)

  RETURN
END SUBROUTINE lorenz63_core

END MODULE lorenz63
