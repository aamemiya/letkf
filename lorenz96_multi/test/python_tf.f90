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
 write(21) x_in
 call system('python3 calc_single.py')
 read(22) x_out
 call system('rm fort.21 fort.22')
return
end subroutine calc_single_tf
!------------------------------------------------!
end module python_tf
