# Lorenz63

The origin of "butterfly" -- a simple 3-dimensional model of deterministic chaos

## Getting started
The main codes are written in Fortran90.  
To exec a job, use a shell script in which all the necessary processes (compilation, link, execution, copying input and output data) are wrapped.  
### Spinup and nature run
First, 'true' time evolution of the system needs to be prepared with the unbiased model.  
Spinup -- create an initial condition at an arbitrary point on the attractor of the system  
`cd model/run`  
`sh spinup_nature.sh`  
This creates the initial condition `DATA/spinup/init_nature.dat`  
 
Nature run -- create a sample time evolution  
`sh nature.sh` 
This creates `DATA/nature.dat`

### Create initial conditions
LETKF needs a set of initial conditions for ensemble forecasts.  
`cd model/run` 
`sh spinup.sh`
This creates `DATA/spinup/init.dat` and `DATA/spinup/initXX.dat` 
*be sure that these may be on a different attractor from that of nature run if the model has a bias

### Create observation
Next, create observation time series.     
`cd obs`
`sh obsmake.sh`
This reads the 'true' data from nature.dat and applies observational filters; random additive noise and variable transformation(optional).  
Default setting is 'all', which means all varaibles are observed with random error of 1.0 standard deviation.
The resultant file is `DATA/all/obs.dat`. 

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
