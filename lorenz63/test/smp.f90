program main
use mod_test

integer,parameter::nx=2,ny=4
real(4)::vx(nx),vy(ny)

call sub_init(nx,ny,f_input)

vx(1)=2.0
vx(2)=3.0
call sub_calc(vx,vy)

write(*,*) vx
write(*,*) vy

contains 
subroutine f_input(vx,vy)
real(4)::vx(nx)
real(4)::vy(ny)

vy(1)=vx(1)
vy(2)=vx(1)**2
vy(3)=vx(2)
vy(4)=vx(2)**2

end subroutine f_input
end program main

