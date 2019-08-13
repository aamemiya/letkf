program test

integer,parameter::nx=7
real(4)::x(nx)

x(1:3)=2.0
open(20,file='temp',form='unformatted')
read(20) x
close(20)
write(*,*) x
stop
end program test
