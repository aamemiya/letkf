program main

integer,parameter::nx=8,nt=7200
real(4)::xfm(nx,nt)
real(4)::xmean(nx),xsdv(nx)

do it=1,nt
read(11) xfm(:,it) 
!write(*,'(8F8.2)') xfm(:,it)
end do

 xmean=sum(xfm,2)/real(nt)
do ix=1,nx
 xsdv(ix)=sqrt(sum((xfm(ix,:)-xmean(ix))**2)/real(nt))
end do


do it=1,10
 write(13) xfm(:,it)
 write(14) xfm(:,it+10)
end do

write(12) xmean
write(12) xsdv

stop
end program
