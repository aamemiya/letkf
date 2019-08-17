module python_tf
use common 

character*100,private::command

contains 
!------------------------------------------------!
subroutine init_tf(nx)
integer,intent(in)::nx
write(command,'(A,I5)') 'python3 init.py',nx
call system(trim(command))
return
end subroutine init_tf
!------------------------------------------------!
subroutine train_tf(nrec)
integer,intent(in)::nrec
write(command,'(A,I5)') 'python3 train.py', nrec
call system(trim(command))
return
end subroutine train_tf
!------------------------------------------------!
subroutine calc_single_tf(nx,x_in,x_out)
integer,intent(In)::nx
real(r_size),intent(In)::x_in(nx)
real(r_size),intent(out)::x_out(nx)
real(r_sngl)::x4(nx)


 x4=real(x_in,r_sngl)
 open (81,file='xfm_temp.dat',form='unformatted',status='replace')
 write(81) x4
 close(81)
 call system('python3 calc_single.py')

 open (82,file='xam_temp.dat',form='unformatted',status='old')
 read(82) x4
 close(82)
 x_out=real(x4,r_size)
 call system('rm xfm_temp.dat xam_temp.dat')

 

return
end subroutine calc_single_tf
!------------------------------------------------!
end module python_tf
