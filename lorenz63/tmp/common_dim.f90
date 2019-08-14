module common_dim
use common
integer,parameter::nx=3
integer,parameter::nr=500
integer,parameter::np=500

public phi
 
contains
function phi(r_in)
 real(r_size)::r_in(nr)
 real(r_size),dimension(np)::phi
 phi(1:np:2)=r_in(1:np:2)
 phi(2:np:2)=r_in(2:np:2)**2
end function phi

end module common_dim
