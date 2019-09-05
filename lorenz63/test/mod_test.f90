module mod_test

procedure(), pointer:: sub_test => null()

contains

subroutine sub_init(nx,ny,sub_input)
 integer,intent(in)::nx,ny
 external sub_input
! interface 
!  subroutine sub_input(vx,vy)
!   real(4),intent(in)::vx(:)
!   real(4),intent(out)::vy(:)
!  end subroutine
! end interface
 sub_test=>sub_input
return
end subroutine sub_init

subroutine sub_calc(vx,vy)
 real(4),intent(in)::vx(:)
 real(4),intent(out)::vy(:)

write(*,*) 'sub_calc'
call sub_test(vx,vy)

return
end subroutine sub_calc
end module mod_test
