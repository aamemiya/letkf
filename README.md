# README

This repository is forked from [takemasa-miyoshi/letkf](https://github.com/takemasa-miyoshi/letkf).

The original contents :
- Resevoir computing
- LETKF with simple on-line bias correction schemes
- Additional low-dimensional models  
 [Lorenz96_multi](https://github.com/aamemiya/letkf/tree/master/lorenz96_multi) : multi-scale-coupled Lorenz96 model  
 [Lorenz63](https://github.com/aamemiya/letkf/tree/master/lorenz63)       : classic 3-variable chaotic dynamical system model  
 [KS](https://github.com/aamemiya/letkf/tree/master/KS)             : Kuramoto-Sivasinski equation - another well-known simple model for spatiotemporal chaos  
 
 See READMEs in each directory for details.
 
 ## Reservoir computing fortran code
  
 A module for reservoir computing is `common/common_rsv_rnet.f90`.  
 It uses a simple linear-regression module `common/common_reg.f90`.  
 
* Usage
### Use module
Add a following line to the header to import the reservoir module 
    
     use common_rsv_rnet

### Initialize
Declear and set parameters in the header   

     integer,parameter::nx=3     !!! system dimension  
     integer,parameter::nr=500   !!! reservoir dimension  
     integer,parameter::nrdim_ave=3       !!! average network dimension   
     real(r_size),parameter::vrho_in=0.4  !!! largest eigenvalue of the adjcency matrix     
     real(r_size),parameter::vsig_in=0.15 !!! input scaling   
     real(r_size),parameter::vreg_in=0.20 !!! reguralization of linear regression  

Define basis functions used in linear regression (the simplest case is shown below - it can be polynomial, Fourier series, or anything)

    subroutine sub_basis_linear(vr,vp)  
    real(r_size),intent(IN)::vr(nr)  
    real(r_size),intent(out)::vp(nr+1)  
    integer::ir  
    do ir=1,nr  
      vp(ir)=vr(ir)  
    end do  
     vp(nr+1)=1.0 !!! constant component  
    return  
    end subroutine sub_basis_linear  `   

Call initialization before sync/train the reservoir  
This allocates reservoir array and input/output function matrice  

    rsv_rnet_init(nx,nr,nrdim_ave,vrho_in,vsig_in,sub_basis_linear,vreg_in)

### Sync reservoir (without training)
Just input the state variable (dimension nx)

     call rsv_rnet_input(x)

### Train reservoir readout layer
Store the time series (length nt_train) of the state variable in x_train(nx,nt_train) and call the subroutine
(this calls rsv_rnet_input inside: do not call it explicitly in advance)

     call rsv_rnet_train_batch(nt_train,x_train) 

### Calculate forecast 
Input x_in(nx) and get x_out(nx) 

     call rsv_rnet_fcst(x_in,x_out)
