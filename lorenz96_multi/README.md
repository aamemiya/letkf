# Lorenz96-multi

Lorenz96 model coupled with smaller-scale Lorenz96 model("unresolved processes")

## Getting started
The main codes are written in Fortran90.  
To exec a job, use a shell script in which all the necessary processes (compilation, link, execution, copying input and output data) are wrapped.  
### Spinup and nature run
First, 'true' time evolution of the system needs to be prepared with the unbiased coupled Lorenz96 model.  
Spinup -- create an initial condition at an arbitrary point on the attractor of the system  
`cd model/run`  
`sh spinup_nature.sh`  
This creates the initial condition `DATA/spinup/init_nature.dat`  
 *this contains both large-scale and small-scale variables
 
Nature run -- create a sample time evolution  
`sh nature.sh` 
This creates `DATA/nature.dat`
*this contains only large-scale variables

### Create initial conditions
LETKF needs a set of initial conditions for ensemble forecasts.  
`cd model/run` 
`sh spinup.sh`
This creates `DATA/spinup/init.dat` and `DATA/spinup/initXX.dat` 
*these initial conditions contain only large-scale variables because these are for the (biased) imperfect model 

### Create observation
Next, create observation time series.     
`cd obs`
`sh obsmake.sh`
This reads the 'true' data from nature.dat and applies observational filters; random additive noise and variable transformation(optional).  
Default setting is 'all_02', which means all varaibles are observed with random error of 0.2 standard deviation.
The resultant file is `DATA/all_02/obs.dat`. 

### Data Assimilation with LETKF
Now you are ready to start a data assimilation experiment with LETKF.  
`cd letkf`  
`sh letkf.sh`  
An observation type and a letkf experiment name are specified in `letkf.sh`. By default they are `all_02` and `test`.  
The following resultaint files in a directory `DATA/all_02/test/` are  
`analmean.dat`  
`guesmean.dat`  
`anal.dat`  
`gues.dat`  
`rmse_x.dat`  
`rmse_t.dat`  

### Data Assimilation with LETKF + bias correction
modify `letkf.sh`  
- letkf_DdSM.f90  
- letkf_reg.f90  

### Reservoir computing

`cd reservoir`  
`sh rsv_test.sh`   


`sh rsv_fcst.sh`

Reservoir computing 'training reusability'

### Visualization

For quicklook, the easiest way is to use [GrADS](http://cola.gmu.edu/grads/) (The Grid Analysis and Display System). Some ctl files are prepared for the data. Below are examples. 

## Changing configurations

## References
About the model:  
[Lorenz, 1996](http://eaps4.mit.edu/research/Lorenz/Predicability_a_Problem_2006.pdf)  
[Lorenz and Emanuel, 2003](https://journals.ametsoc.org/doi/full/10.1175/1520-0469%281998%29055%3C0399%3AOSFSWO%3E2.0.CO%3B2)  
Correction of subgrid-scale effects:  
[Wilks, 2005](https://doi.org/10.1256/qj.04.03)  
[Danforth and Kalnay, 2008](https://journals.ametsoc.org/doi/full/10.1175/2007JAS2419.1)  
[Pulido et al. 2016](https://rmets.onlinelibrary.wiley.com/doi/abs/10.1002/qj.2879)  
