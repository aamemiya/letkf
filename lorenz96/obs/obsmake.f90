PROGRAM obsmake
!=======================================================================
! simulate observation data
!=======================================================================
  USE common
  USE lorenz96
  USE h_ope

  IMPLICIT NONE

  INTEGER,PARAMETER :: ndays=3600
  INTEGER,PARAMETER :: nt=ndays*4
  REAL(r_size),PARAMETER :: obserr=1.0d0
  REAL(r_sngl) :: x4(nx)
  REAL(r_size) :: x(nx)
  REAL(r_size) :: ober(ny*nt)
  REAL(r_size) :: y(ny)
  REAL(r_sngl) :: y4(ny)
  INTEGER :: it
  INTEGER :: i,j,k

  INTEGER :: istat, idnci, idnco, iddy, idvy, iddt, idvt, idvvi, idvvo

  REAL(r_sngl) :: time

  include 'netcdf.inc'

  istat=NF_OPEN('nature.nc',NF_NOWRITE,idnci)
  istat=NF_INQ_VARID(idnci,'v',idvvi)

  istat=NF_CREATE('obs.nc',NF_CLOBBER,idnco)
  istat=NF_DEF_DIM(idnco,'y',ny,iddy)
  istat=NF_DEF_DIM(idnco,'t',NF_UNLIMITED,iddt)
  istat=NF_DEF_VAR(idnco,'y',NF_FLOAT,1,iddy,idvy)
  istat=NF_DEF_VAR(idnco,'t',NF_FLOAT,1,iddt,idvt)
  istat=NF_DEF_VAR(idnco,'vy',NF_FLOAT,2,(/iddy,iddt/),idvvo)
  istat=NF_ENDDEF(idnco)
  istat=NF_PUT_VARA_REAL(idnco,idvy,1,ny,(/( real(i), i=1,ny )/))

  time=0.0

  CALL com_randn(ny*nt,ober)
  ober = ober * obserr
  k=0
  DO it=1,nt
    !
    ! nature run <- fort.10
    !
    istat=NF_GET_VARA_REAL(idnci,idvvi,(/1,it/),(/nx,1/),x4)
!    READ(10) x4
    x = REAL(x4,r_size)
    CALL set_h(x)
    !
    ! Hx + ober
    !
    DO j=1,ny
      k = k+1
      y(j) = ober(k)
      DO i=1,nx
        y(j) = y(j) + h(j,i) * x(i)
      END DO
    END DO
    !
    ! obs data -> fort.91
    !
    y4 = y
    istat=NF_PUT_VARA_REAL(idnco,idvvo,(/1,it/),(/ny,1/),y4)
    istat=NF_PUT_VARA_REAL(idnco,idvt,it,1,time)
!    WRITE(91) y4
   time=time+dt
  END DO
  istat=NF_CLOSE(idnci)
  istat=NF_CLOSE(idnco)

  STOP
END PROGRAM obsmake
