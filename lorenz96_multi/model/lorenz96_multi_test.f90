MODULE lorenz96
!=======================================================================
!
! [PURPOSE:] Lorenz 1996 model for large-scale variables
!            with steady inhomogeneous bias (Danforth and Kalnay, 2008)
!
! [PUBLIC:]
!   SUBROUTINE tinteg_rk4(kt,xin,xout)
!   SUBROUTINE TL_tinteg_rk4(kt,x9,xin,xout)
!   SUBROUTINE TL_tinteg_rk4_x9out(kt,x9,xin,xout)
!   SUBROUTINE AD_tinteg_rk4(kt,x9,xin,xout)
!   SUBROUTINE tinteg_rk4_ptbmtx(alpha,kt,x9,pa,pf)
!   SUBROUTINE TL_tinteg_rk4_ptbmtx(kt,x9,pa,pf)
!
! [FIRST CREATED:] 08/23/2003 Takemasa MIYOSHI
!
! [HISTORY:]
!   08/23/2003 Takemasa Miyoshi  Initial Creation
!   10/17/2003 Takemasa Miyoshi  Tangent Linear Model is added
!   03/20/2004 Takemasa Miyoshi  Covariance matrix forecast is added
!   03/30/2004 Takemasa Miyoshi  Adjoint model is added
!   03/31/2004 Takemasa Miyoshi  Cleaned up
!   06/08/2009 Takemasa Miyoshi  Orography model is separated
!
!   08/02/2019 Arata Amemiya     modification
!
!=======================================================================
!=======================================================================
!
! [PURPOSE:] Multi-scale Lorenz 1996 model (Wilks et al. 2005)
!
! [PUBLIC:]
!   SUBROUTINE tinteg_rk4(kt,xin,xout)
!   SUBROUTINE TL_tinteg_rk4(kt,x9,xin,xout)
!   SUBROUTINE TL_tinteg_rk4_x9out(kt,x9,xin,xout)
!   SUBROUTINE AD_tinteg_rk4(kt,x9,xin,xout)
!   SUBROUTINE tinteg_rk4_ptbmtx(alpha,kt,x9,pa,pf)
!   SUBROUTINE TL_tinteg_rk4_ptbmtx(kt,x9,pa,pf)
!
! [FIRST CREATED:] 08/23/2003 Takemasa MIYOSHI
! [EXTENDED TO MULTI-SCALE:] 08/02/2019 Arata AMEMIYA
!
! [HISTORY:]
!   08/23/2003 Takemasa Miyoshi  Initial Creation
!   10/17/2003 Takemasa Miyoshi  Tangent Linear Model is added
!   03/20/2004 Takemasa Miyoshi  Covariance matrix forecast is added
!   03/30/2004 Takemasa Miyoshi  Adjoint model is added
!   03/31/2004 Takemasa Miyoshi  Cleaned up
!   06/08/2009 Takemasa Miyoshi  Orography model is separated
!
!   08/02/2019 Arata Amemiya     Multi-scale
!
!=======================================================================
  USE common

  PRIVATE

  PUBLIC :: tinteg_rk4, TL_tinteg_rk4, TL_tinteg_rk4_x9out, AD_tinteg_rk4,&
          & tinteg_rk4_ptbmtx, TL_tinteg_rk4_ptbmtx

  INTEGER,PARAMETER,PUBLIC :: nx=8          ! number of grid points of large scale variable
  INTEGER,PARAMETER,PUBLIC :: nxx=256        ! number of grid points of small scale variable
  INTEGER,PARAMETER,PUBLIC :: ntot=nx+nxx   ! number of total grid points
  REAL(r_size),SAVE,PUBLIC :: dt=0.005d0    ! time of one time step
  REAL(r_size),SAVE,PUBLIC :: force=8.0d0   ! F term
  REAL(r_size),SAVE,PUBLIC :: oneday=0.2d0  ! time for one day

  REAL(r_size),SAVE,PUBLIC :: vh=0.0d0      ! coupling intensity
  REAL(r_size),SAVE,PUBLIC :: vc=10.0d0      ! scale separation
  REAL(r_size),SAVE,PUBLIC :: vb=10.0d0      ! scale separation

  LOGICAL,SAVE,PUBLIC :: flag_mat=.false.
  REAL(r_size),SAVE,PUBLIC :: cpl_mat(nx,nxx) ! Coupling matrix
CONTAINS
!=======================================================================
! [0] Time integration of Perturbation Matrix
!=======================================================================
!-----------------------------------------------------------------------
! [0.1] M P M^T
!-----------------------------------------------------------------------
SUBROUTINE tinteg_rk4_ptbmtx(alpha,kt,xy9,pa,pf)
  IMPLICIT NONE

  REAL(r_size),INTENT(IN)  :: alpha ! NL(x+alpha*dx) = NL(x)+alpha*dxf
  INTEGER,INTENT(IN) :: kt
  REAL(r_size),INTENT(IN)  :: xy9(1:ntot)  ! background state
  REAL(r_size),INTENT(IN)  :: pa(1:ntot,1:ntot)
  REAL(r_size),INTENT(OUT) :: pf(1:ntot,1:ntot)

  REAL(r_size),ALLOCATABLE :: work1(:),work2(:)
  INTEGER :: i

  ALLOCATE( work1(1:ntot) )
  ALLOCATE( work2(1:ntot) )

  CALL tinteg_rk4(kt,xy9,work1)
  DO i=1,ntot
    work2(:) = pa(:,i) * alpha + xy9(:)
    CALL tinteg_rk4(kt,work2,work2)
    pf(:,i) = ( work2 - work1 ) / alpha
  END DO

  DEALLOCATE( work1,work2 )

  RETURN
END SUBROUTINE tinteg_rk4_ptbmtx
!-----------------------------------------------------------------------
! [0.2] M P M^T using TL
!-----------------------------------------------------------------------
SUBROUTINE TL_tinteg_rk4_ptbmtx(kt,xy9,pa,pf)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: kt
  REAL(r_size),INTENT(IN)  :: xy9(1:ntot)  ! background state
  REAL(r_size),INTENT(IN)  :: pa(1:ntot,1:ntot)
  REAL(r_size),INTENT(OUT) :: pf(1:ntot,1:ntot)
  INTEGER :: i

  DO i=1,ntot
    CALL TL_tinteg_rk4(kt,xy9,pa(:,i),pf(:,i))
  END DO

  RETURN
END SUBROUTINE TL_tinteg_rk4_ptbmtx
!=======================================================================
! [1] Methods of Lorenz96
!=======================================================================
!-----------------------------------------------------------------------
! [1.1] Time integration of Lorenz96
!-----------------------------------------------------------------------
SUBROUTINE tinteg_rk4(kt,xyin,xyout)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: kt
  REAL(r_size),INTENT(IN)  :: xyin(1:ntot)
  REAL(r_size),INTENT(OUT) :: xyout(1:ntot)
  REAL(r_size),ALLOCATABLE :: xy(:),xytmp(:),q1(:),q2(:),q3(:),q4(:)
  INTEGER :: k
!--[1.1.1] allocation --------------------------------------------------
  ALLOCATE( xy(1:ntot) )
  ALLOCATE( xytmp(1:ntot) )
  ALLOCATE( q1(1:ntot) )
  ALLOCATE( q2(1:ntot) )
  ALLOCATE( q3(1:ntot) )
  ALLOCATE( q4(1:ntot) )
!--[1.1.2] time integration --------------------------------------------
  xy(:) = xyin(:)
!>>>>> TIME INTEGRATION START
  DO k=1,kt
    xytmp(:) = xy(:)
    CALL lorenz96_core(xytmp,q1)
    xytmp(:) = xy(:) + 0.5d0 * q1(:)
    CALL lorenz96_core(xytmp,q2)
    xytmp(:) = xy(:) + 0.5d0 * q2(:)
    CALL lorenz96_core(xytmp,q3)
    xytmp(:) = xy(:) + q3(:)
    CALL lorenz96_core(xytmp,q4)
    xy(:) = xy(:) + ( q1(:) + 2.0d0 * q2(:) + 2.0d0 * q3(:) + q4(:) ) / 6.0d0
  END DO
!<<<<< TIME INTEGRATION END
  xyout(:) = xy(:)
!--[1.1.3] tidy up -----------------------------------------------------
  DEALLOCATE( xytmp,q1,q2,q3,q4 )

  RETURN
END SUBROUTINE tinteg_rk4
!-----------------------------------------------------------------------
! [1.2] TL: time integration of Lorenz96
!-----------------------------------------------------------------------
SUBROUTINE TL_tinteg_rk4(kt,xy9,xyin,xyout)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: kt
  REAL(r_size),INTENT(IN)  :: xy9(1:ntot)
  REAL(r_size),INTENT(IN)  :: xyin(1:ntot)
  REAL(r_size),INTENT(OUT) :: xyout(1:ntot)
  REAL(r_size),ALLOCATABLE :: xy(:),xy9tmp(:,:)
  INTEGER :: k
!--[1.2.1] allocation --------------------------------------------------
  ALLOCATE( xy(1:ntot) )
  ALLOCATE( xy9tmp(1:ntot,1:5) )
!--[1.2.2] time integration --------------------------------------------
  xy9tmp(:,1) = xy9
  xy = xyin
  DO k=1,kt
    CALL TL_tinteg_rk4_one(xy9tmp,xy,xy)
    xy9tmp(:,1) = xy9tmp(:,5)
  END DO
  xyout = xy
!--[1.2.3] tidy up -----------------------------------------------------
  DEALLOCATE( xy,xy9tmp )
 
  RETURN
END SUBROUTINE TL_tinteg_rk4
!-----------------------------------------------------------------------
! [1.3] TL (detail x9 out): time integration of Lorenz96
!-----------------------------------------------------------------------
SUBROUTINE TL_tinteg_rk4_x9out(kt,xy9,xyin,xyout)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: kt
  REAL(r_size),INTENT(INOUT)  :: xy9(1:ntot,1:4,1:kt)
  REAL(r_size),INTENT(IN)  :: xyin(1:ntot)
  REAL(r_size),INTENT(OUT) :: xyout(1:ntot)
  REAL(r_size),ALLOCATABLE :: xy(:),xy9tmp(:,:)
  INTEGER :: k
!--[1.3.1] allocation --------------------------------------------------
  ALLOCATE( xy(1:ntot) )
  ALLOCATE( xy9tmp(1:ntot,1:5) )
!--[1.3.2] time integration --------------------------------------------
  xy9tmp(:,1) = xy9(:,1,1)
  xy = xyin
  DO k=1,kt
    CALL TL_tinteg_rk4_one(xy9tmp,xy,xy)
    xy9(:,:,k) = xy9tmp(:,1:4)
    xy9tmp(:,1) = xy9tmp(:,5)
  END DO
  xyout = xy
!--[1.3.3] tidy up -----------------------------------------------------
  DEALLOCATE( xy,xy9tmp )

  RETURN
END SUBROUTINE TL_tinteg_rk4_x9out
!-----------------------------------------------------------------------
! [1.4] TL one step: time integration of Lorenz96
!-----------------------------------------------------------------------
SUBROUTINE TL_tinteg_rk4_one(xy9,xyin,xyout)
  IMPLICIT NONE

  REAL(r_size),INTENT(INOUT)  :: xy9(1:ntot,1:5)
  REAL(r_size),INTENT(IN)  :: xyin(1:ntot)
  REAL(r_size),INTENT(OUT) :: xyout(1:ntot)
  REAL(r_size),ALLOCATABLE :: xy(:),xytmp(:),q1(:),q2(:),q3(:),q4(:)
  REAL(r_size),ALLOCATABLE :: q19(:),q29(:),q39(:),q49(:)
!--[1.4.1] allocation --------------------------------------------------
  ALLOCATE( xy(1:ntot) )
  ALLOCATE( xytmp(1:ntot) )
  ALLOCATE( q1(1:ntot) )
  ALLOCATE( q2(1:ntot) )
  ALLOCATE( q3(1:ntot) )
  ALLOCATE( q4(1:ntot) )
  ALLOCATE( q19(1:ntot) )
  ALLOCATE( q29(1:ntot) )
  ALLOCATE( q39(1:ntot) )
  ALLOCATE( q49(1:ntot) )
!--[1.4.2] time integration --------------------------------------------
  xy(:) = xyin(:)
  xytmp(:) = xy(:)
  CALL TL_lorenz96_core(xy9(:,1),xytmp,q1)
  xytmp = xy9(:,1) + xy
  CALL lorenz96_core(xytmp,q19)
  xy9(:,2) = xy9(:,1) + 0.5d0 * q19
  xytmp(:) = xy(:) + 0.5d0 * q1(:)
  CALL TL_lorenz96_core(xy9(:,2),xytmp,q2)
  xytmp = xy9(:,2) + xy
  CALL lorenz96_core(xytmp,q29)
  xy9(:,3) = xy9(:,1) + 0.5d0 * q29
  xytmp(:) = xy(:) + 0.5d0 * q2(:)
  CALL TL_lorenz96_core(xy9(:,3),xytmp,q3)
  xytmp = xy9(:,3) + xy
  CALL lorenz96_core(xytmp,q39)
  xy9(:,4) = xy9(:,1) + q39
  xytmp(:) = xy(:) + q3(:)
  CALL TL_lorenz96_core(xy9(:,4),xytmp,q4)
  xytmp = xy9(:,4) + xy
  CALL lorenz96_core(xytmp,q49)
  xyout(:) = xy(:) + ( q1(:) + 2.0d0 * q2(:) + 2.0d0 * q3(:) + q4(:) ) / 6.0d0
  xy9(:,5) = xy9(:,1) + ( q19 + 2.0d0 * q29 + 2.0d0 * q39 + q49 ) / 6.0d0
!--[1.4.3] tidy up -----------------------------------------------------
  DEALLOCATE( xytmp,q1,q2,q3,q4 )
  DEALLOCATE( q19,q29,q39,q49 )

  RETURN
END SUBROUTINE TL_tinteg_rk4_one
!-----------------------------------------------------------------------
! [1.5] AD: time integration of Lorenz96 without orography
!-----------------------------------------------------------------------
SUBROUTINE AD_tinteg_rk4(kt,xy9,xyin,xyout)
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: kt
  REAL(r_size),INTENT(IN)  :: xy9(1:ntot,1:4,1:kt)  ! background state
  REAL(r_size),INTENT(IN)  :: xyin(1:ntot)
  REAL(r_size),INTENT(OUT) :: xyout(1:ntot)
  REAL(r_size),ALLOCATABLE :: xy(:),xytmp(:),q1(:),q2(:),q3(:),q4(:)
  REAL(r_size),ALLOCATABLE :: q19(:),q29(:),q39(:),q49(:)
  INTEGER :: k
!--[1.5.1] allocation --------------------------------------------------
  ALLOCATE( xy(1:ntot) )
  ALLOCATE( xytmp(1:ntot) )
  ALLOCATE( q1(1:ntot) )
  ALLOCATE( q2(1:ntot) )
  ALLOCATE( q3(1:ntot) )
  ALLOCATE( q4(1:ntot) )
  ALLOCATE( q19(1:ntot) )
  ALLOCATE( q29(1:ntot) )
  ALLOCATE( q39(1:ntot) )
  ALLOCATE( q49(1:ntot) )
!--[1.5.2] time integration --------------------------------------------
  xy = xyin
  DO k=kt,1,-1
    q1 = xy / 6.0d0
    q2 = xy / 3.0d0
    q3 = xy / 3.0d0
    q4 = xy / 6.0d0

    CALL AD_lorenz96_core(xy9(:,4,k),xytmp,q4)
    xy = xy + xytmp
    q3 = q3 + xytmp
    CALL AD_lorenz96_core(xy9(:,3,k),xytmp,q3)
    xy = xy + xytmp
    q2 = q2 + xytmp * 0.5d0
    CALL AD_lorenz96_core(xy9(:,2,k),xytmp,q2)
    xy = xy + xytmp
    q1 = q1 + xytmp * 0.5d0
    CALL AD_lorenz96_core(xy9(:,1,k),xytmp,q1)
    xy = xy + xytmp
  END DO
  xyout = xy
!--[1.5.3] tidy up -----------------------------------------------------
  DEALLOCATE( xytmp,q1,q2,q3,q4 )
  DEALLOCATE( q19,q29,q39,q49 )

  RETURN
END SUBROUTINE AD_tinteg_rk4
!=======================================================================
! [2] core part of Lorenz96
!=======================================================================
!--[2.0] Coupling matrix -----------------------------------------------
SUBROUTINE set_cpl
  IMPLICIT NONE

  INTEGER::NRATIO,i ,ista,iend 

     nratio = nxx/nx
     cpl_mat=0.0
     do i=1,nx
        ista=nratio*(i-1)+1
        iend=nratio*(i-1)+nratio
        cpl_mat(i,ista:iend) = 1.0 
     end do

     flag_mat=.true.
     
  RETURN
END SUBROUTINE set_cpl
!--[2.1] NL ------------------------------------------------------------
SUBROUTINE lorenz96_core(xyin,xyout)
  IMPLICIT NONE

  REAL(r_size),INTENT(IN) :: xyin(1:ntot)
  REAL(r_size),INTENT(OUT) :: xyout(1:ntot)
  INTEGER :: i

  if (.not.flag_mat) call set_cpl  

  xyout(1) = xyin(nx) * ( xyin(2) - xyin(nx-1) ) - xyin(1) + force - vh*vc/vb * sum(cpl_mat(1,1:nxx)*xyin(nx+1:ntot))
  xyout(2) = xyin(1) * ( xyin(3) - xyin(nx) ) - xyin(2) + force - vh*vc/vb * sum(cpl_mat(2,1:nxx)*xyin(nx+1:ntot))
  DO i=3,nx-1
    xyout(i) = xyin(i-1) * ( xyin(i+1) - xyin(i-2) ) - xyin(i) + force - vh*vc/vb * sum(cpl_mat(i,1:nxx)*xyin(nx+1:ntot))
  END DO
  xyout(nx) = xyin(nx-1) * ( xyin(1) - xyin(nx-2) ) - xyin(nx) + force - vh*vc/vb * sum(cpl_mat(nx,1:nxx)*xyin(nx+1:ntot))


    xyout(nx+1) = -vc*vb*xyin(ntot) * ( xyin(nx+2) - xyin(ntot-1) ) - vc*xyin(nx+1) + vh*vc/vb * sum(cpl_mat(1:nx,1)*xyin(1:nx))
    xyout(nx+2) = -vc*vb*xyin(nx+1) * ( xyin(nx+3) - xyin(ntot) ) - vc*xyin(nx+2) + vh*vc/vb * sum(cpl_mat(1:nx,2)*xyin(1:nx))
  DO i=3,nxx-1
    xyout(nx+i) = -vc*vb*xyin(nx+i-1) * ( xyin(nx+i+1) - xyin(nx+i-2) ) - vc*xyin(nx+i) + vh*vc/vb * sum(cpl_mat(1:nx,i)*xyin(1:nx))
  END DO
    xyout(ntot) = -vc*vb*xyin(ntot-1) * ( xyin(nx+1) - xyin(ntot-2) ) - vc*xyin(ntot) + vh*vc/vb * sum(cpl_mat(1:nx,nxx)*xyin(1:nx))



  xyout(:) = dt * xyout(:)

  RETURN
END SUBROUTINE lorenz96_core
!--[2.2] TL ------------------------------------------------------------
SUBROUTINE TL_lorenz96_core(xy9,xyin,xyout)
  IMPLICIT NONE

  REAL(r_size),INTENT(IN) :: xy9(1:ntot)
  REAL(r_size),INTENT(IN) :: xyin(1:ntot)
  REAL(r_size),INTENT(OUT) :: xyout(1:ntot)
  INTEGER :: i

  if (.not.flag_mat) call set_cpl  

  xyout(1) = xy9(nx) * ( xyin(2) - xyin(nx-1) ) + xyin(nx) * ( xy9(2) - xy9(nx-1) ) - xyin(1) - vh*vc/vb * sum(cpl_mat(1,1:nxx)*xyin(nx+1:ntot))
  xyout(2) = xy9(1) * ( xyin(3) - xyin(nx) ) + xyin(1) * ( xy9(3) - xy9(nx) ) - xyin(2) - vh*vc/vb * sum(cpl_mat(2,1:nxx)*xyin(nx+1:ntot))

  DO i=3,nx-1
    xyout(i) = xy9(i-1) * ( xyin(i+1) - xyin(i-2) ) + xyin(i-1) * ( xy9(i+1) - xy9(i-2) ) - xyin(i) - vh*vc/vb * sum(cpl_mat(i,1:nxx)*xyin(nx+1:ntot))
  END DO
  xyout(nx) = xy9(nx-1) * ( xyin(1) - xyin(nx-2) ) + xyin(nx-1) * ( xy9(1) - xy9(nx-2) ) - xyin(nx) - vh*vc/vb * sum(cpl_mat(nx,1:nxx)*xyin(nx+1:ntot))


    xyout(nx+1) = -vc*vb*xy9(ntot) * ( xyin(nx+2) - xyin(ntot-1) ) -vc*vb*xyin(ntot) * ( xy9(nx+2) - xy9(ntot-1) ) - vc*xyin(nx+1) + vh*vc/vb * sum(cpl_mat(1:nx,1)*xyin(1:nx))
    xyout(nx+2) = -vc*vb*xy9(nx+1) * ( xyin(nx+3) - xyin(ntot) ) -vc*vb*xyin(nx+1) * ( xy9(nx+3) - xy9(ntot) ) - vc*xyin(nx+2) + vh*vc/vb * sum(cpl_mat(1:nx,2)*xyin(1:nx))

  DO i=3,nxx-1
    xyout(nx+i) = -vc*vb*xy9(nx+i-1) * ( xyin(nx+i+1) - xyin(nx+i-2) ) -vc*vb*xyin(nx+i-1) * ( xy9(nx+i+1) - xy9(nx+i-2) ) - vc*xyin(nx+i) + vh*vc/vb * sum(cpl_mat(1:nx,i)*xyin(1:nx))
  END DO
    xyout(ntot) = -vc*vb*xy9(ntot-1) * ( xyin(nx+1) - xyin(ntot-2) )  -vc*vb*xyin(ntot-1) * ( xy9(nx+1) - xy9(ntot-2) ) - vc*xyin(ntot) + vh*vc/vb * sum(cpl_mat(1:nx,nxx)*xyin(1:nx))

  xyout(:) = dt * xyout(:)

  RETURN
END SUBROUTINE TL_lorenz96_core
!--[2.3] AD ------------------------------------------------------------
SUBROUTINE AD_lorenz96_core(xy9,xyin,xyout)
  IMPLICIT NONE

  REAL(r_size),INTENT(IN) :: xy9(1:ntot)
  REAL(r_size),INTENT(OUT) :: xyin(1:ntot)
  REAL(r_size),INTENT(INOUT) :: xyout(1:ntot)
  INTEGER :: i

  if (.not.flag_mat) call set_cpl  

  xyin = 0.0d0

  xyout(:) = dt * xyout(:)

  xyin(1) = xyin(1) + xy9(nx-1) * xyout(nx)
  xyin(nx-2) = xyin(nx-2) - xy9(nx-1) * xyout(nx)
  xyin(nx-1) = xyin(nx-1) + ( xy9(1) - xy9(nx-2) ) * xyout(nx)
  xyin(nx) = xyin(nx) - xyout(nx)

  DO i=nx-1,3,-1
    xyin(i+1) = xyin(i+1) + xy9(i-1) * xyout(i)
    xyin(i-2) = xyin(i-2) - xy9(i-1) * xyout(i)
    xyin(i-1) = xyin(i-1) + ( xy9(i+1) - xy9(i-2) ) * xyout(i)
    xyin(i) = xyin(i) - xyout(i)
  END DO

  xyin(3) = xyin(3) + xy9(1) * xyout(2)
  xyin(nx) = xyin(nx) - xy9(1) * xyout(2)
  xyin(1) = xyin(1) + ( xy9(3) - xy9(nx) ) * xyout(2)
  xyin(2) = xyin(2) - xyout(2)

  xyin(2) = xyin(2) + xy9(nx) * xyout(1)
  xyin(nx-1) = xyin(nx-1) - xy9(nx) * xyout(1)
  xyin(nx) = xyin(nx) + ( xy9(2) - xy9(nx-1) ) * xyout(1)
  xyin(1) = xyin(1) - xyout(1)


  xyin(nx+1) = xyin(nx+1) + (-vc*vb)* xy9(nx+nxx-1) * xyout(nx+nxx)
  xyin(nx+nxx-2) = xyin(nx+nxx-2) - (-vc*vb)* xy9(nx+nxx-1) * xyout(nx+nxx)
  xyin(nx+nxx-1) = xyin(nx+nxx-1) + (-vc*vb)* ( xy9(nx+1) - xy9(nx+nxx-2) ) * xyout(nx+nxx)
  xyin(nx+nxx) = xyin(nx+nxx) - vc* xyout(nx+nxx)

  DO i=nxx-1,3,-1
    xyin(nx+i+1) = xyin(nx+i+1) + (-vc*vb)* xy9(nx+i-1) * xyout(nx+i)
    xyin(nx+i-2) = xyin(nx+i-2) - (-vc*vb)* xy9(nx+i-1) * xyout(nx+i)
    xyin(nx+i-1) = xyin(nx+i-1) + (-vc*vb)* ( xy9(nx+i+1) - xy9(nx+i-2) ) * xyout(nx+i)
    xyin(nx+i) = xyin(nx+i) - vc*xyout(nx+i)
  END DO

  xyin(nx+3) = xyin(nx+3) + (-vc*vb)* xy9(nx+1) * xyout(nx+2)
  xyin(nx+nxx) = xyin(nx+nxx) - (-vc*vb)* xy9(nx+1) * xyout(nx+2)
  xyin(nx+1) = xyin(nx+1) + (-vc*vb)*( xy9(nx+3) - xy9(nx+nxx) ) * xyout(nx+2)
  xyin(nx+2) = xyin(nx+2) - vc*xyout(nx+2)

  xyin(nx+2) = xyin(nx+2) + (-vc*vb)* xy9(nx+nxx) * xyout(nx+1)
  xyin(nx+nxx-1) = xyin(nx+nxx-1) - (-vc*vb)* xy9(nx+nxx) * xyout(nx+1)
  xyin(nx+nxx) = xyin(nx+nxx) + (-vc*vb)* ( xy9(nx+2) - xy9(nx+nxx-1) ) * xyout(nx+1)
  xyin(nx+1) = xyin(nx+1) - vc*xyout(nx+1)

DO i=1,nx
 xyin(i) = xyin(i) + vh*vc/vb * sum(cpl_mat(i,1:nxx) * xyout(nx+1:nx+nxx))
END DO

DO i=1,nxx
 xyin(nx+i) = xyin(nx+i) - vh*vc/vb * sum(cpl_mat(1:nx,i) * xyout(1:nx))
END DO



  RETURN
END SUBROUTINE AD_lorenz96_core

END MODULE lorenz96
