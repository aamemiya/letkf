!==================================================!
subroutine monitor_yspace(hxfcst,hxanal,y,istep)
use common
USE common_letkf
USE h_ope

real(r_size)::hxfcst(ny,nbv)
real(r_size)::hxanal(ny,nbv)
real(r_size)::y(ny)
integer::istep

real(4),parameter:: obserr=1.0
real(4)::yerror(ny)
real(4)::r_hxfcst(ny,nbv)
real(4)::r_hxanal(ny,nbv)
real(4)::r_y(ny)

real(4)::vy1smp_f(nbv)
real(4)::vy2smp_f(nbv)
real(4)::vy1smp_a(nbv)
real(4)::vy2smp_a(nbv)
real(4)::vy1obs
real(4)::vy2obs

real(4),parameter::vy1min=-5.0
real(4),parameter::vy1max=10.0

real(4),parameter::vy2min=-5.0
real(4),parameter::vy2max=10.0

character*20 ::title1,title2(2),title3

integer,parameter::npts=1000
real(4)::vpts_y1_f(npts)
real(4)::vpts_y2_f(npts)
real(4)::vpts_y1_a(npts)
real(4)::vpts_y2_a(npts)
real(4)::vpts_y1_obs(npts)
real(4)::vpts_y2_obs(npts)
real(4),parameter::twpi=6.283

character*40 ::psfile='figure'


title1='obs monitor'
title2=''
write(title3,'(A,I5)')'day',istep/4


r_hxfcst=real(hxfcst)
r_hxanal=real(hxanal)
r_y=real(y)
yerror=obserr*2

vy1smp_f(:)=r_hxfcst(1,:)
vy2smp_f(:)=r_hxfcst(4,:)
vy1smp_a(:)=r_hxanal(1,:)
vy2smp_a(:)=r_hxanal(4,:)
vy1obs=r_y(1)
vy2obs=r_y(4)
vy1obs_sdv=sqrt(yerror(1))
vy2obs_sdv=sqrt(yerror(4))
!write(*,*) vy1obs_sdv
!write(*,*) yerror(1)
!write(*,*) obserr*2
!stop


vy1_c = sum(vy1smp_f)/real(nbv)
vy2_c = sum(vy2smp_f)/real(nbv)
vy1_a_c = sum(vy1smp_a)/real(nbv)
vy2_a_c = sum(vy2smp_a)/real(nbv)


vy1_w = sqrt(sum((vy1smp_f-vy1_c)**2)/real(nbv))
vy2_w = sqrt(sum((vy2smp_f-vy2_c)**2)/real(nbv))
vy1_a_w = sqrt(sum((vy1smp_a-vy1_a_c)**2)/real(nbv))
vy2_a_w = sqrt(sum((vy2smp_a-vy2_a_c)**2)/real(nbv))


do ipt=1,npts-1
 vpts_y1_f(ipt) = vy1_c + vy1_w * cos(twpi* real(ipt-1)/real(npts-1))
 vpts_y2_f(ipt) = vy2_c + vy2_w * sin(twpi* real(ipt-1)/real(npts-1))
 vpts_y1_a(ipt) = vy1_a_c + vy1_a_w * cos(twpi* real(ipt-1)/real(npts-1))
 vpts_y2_a(ipt) = vy2_a_c + vy2_a_w * sin(twpi* real(ipt-1)/real(npts-1))
 vpts_y1_obs(ipt) = vy1obs + vy1obs_sdv * cos(twpi* real(ipt-1)/real(npts-1))
 vpts_y2_obs(ipt) = vy2obs + vy2obs_sdv * sin(twpi* real(ipt-1)/real(npts-1))
end do
 ipt=npts
 vpts_y1_f(ipt) = vy1_c + vy1_w * cos(twpi* real(ipt-1)/real(npts-1))
 vpts_y2_f(ipt) = vy2_c + vy2_w * sin(twpi* real(ipt-1)/real(npts-1))
 vpts_y1_a(ipt) = vy1_a_c + vy1_a_w * cos(twpi* real(ipt-1)/real(npts-1))
 vpts_y2_a(ipt) = vy2_a_c + vy2_a_w * sin(twpi* real(ipt-1)/real(npts-1))
 vpts_y1_obs(ipt) = vy1obs + vy1obs_sdv * cos(twpi* real(ipt-1)/real(npts-1))
 vpts_y2_obs(ipt) = vy2obs + vy2obs_sdv * sin(twpi* real(ipt-1)/real(npts-1))

write(psfile,'(A,I6.6)')'monitor_obs_',istep
call draw

return


contains
!==================================================!
subroutine draw 


  iout=2


  vpl=0.15
  vpr=0.85
  vpb=0.20
  vpt=0.70

  call gliset('MSGLEV',1)
  call sgiset('IFONT',1)
  call swiset('ICLRMAP',6)
  call swcmll
  call swcset('FNAME',trim(psfile))
  call swlset('LSEP',.false.)
  call swiset('IFL',1) !!! PNG
  call swiset('IWIDTH',1000)
  call swiset('IHEIGHT',800)
  call gropn(iout)
  call sglset('LFULL',.true.)
  call sglset('LCLIP',.true.)
  call slmgn(0.0,0.0,0.0,0.0)
  call grfrm

  call grswnd(vy1min,vy1max,vy2min,vy2max)

  call grsvpt(vpl,vpr,vpb,vpt)
  call grstrn(1)
  call grstrf


! call sgtnzu(npts,vpts_y1_f,vpts_y2_f,40999)
 call sgtnzu(npts,vpts_y1_a,vpts_y2_a,40999)
! call sgtnzu(npts,vpts_y1_obs,vpts_y2_obs,10999)

 call uulinz(npts,vpts_y1_f,vpts_y2_f,3,31)
! call uulinz(npts,vpts_y1_a,vpts_y2_a,3,31)
 call uulinz(npts,vpts_y1_obs,vpts_y2_obs,3,21)

 call uumrkz(nbv,vy1smp_f,vy2smp_f,4,31,0.005)
 call uumrkz(nbv,vy1smp_a,vy2smp_a,4,41,0.005)
 call uumrkz(1,vy1obs,vy2obs,4,21,0.010)

  amtics=5.0
  astics=5.0

  bmtics=5.0
  bstics=5.0

  call sglset('LCLIP',.false.)
  call uzinit
  call uzlset('LOFFSET',.false.)
  call uziset('INDEXT2',3)
  call uziset('INDEXT1',3)
  call uziset('INNER',-1)
  call uzrset('RSIZEL1',0.016)
  call uzrset('RSIZEC1',0.016)
  call uzrset('RSIZET1',0.006)
  call uzrset('RSIZET2',0.003)
  call uxsfmt ('B')
  call uysfmt ('B')

    
  call uxaxdv('B',astics,amtics)
  call uxaxdv('T',astics,amtics)
  call uxsttl('B','y1',0.0)
  call uyaxdv('L',bstics,bmtics)
  call uyaxdv('R',bstics,bmtics)
  call uysttl('L','y2',0.0)

  call uzlset('LABELYR',.false.)

  call sgtxzv (0.5*(vpr+vpl),vpt+0.03,trim(title1),0.025,0,0,5) !
  call sgtxzv (vpr-0.01,vpt+0.045,trim(title2(1)),0.016,0,1,3) !
  call sgtxzv (vpr-0.01,vpt+0.020,trim(title2(2)),0.016,0,1,3) !
  call sgtxzv (vpl+0.01,vpt+0.025,trim(title3),0.018,0,-1,3) !
  call grcls 


return
end subroutine draw

!==================================================!

end subroutine monitor_yspace
