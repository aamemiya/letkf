# Lorenz96-multi

Lorenz96 model coupled with smaller-scale Lorenz96 model
("unresolved") processes 

## Getting started
The main codes are written in plain Fortran90.
To exec a job, all the necessary processes (compilation, link, execution, copying input and output data) is wrapped in one shell script.  
### Spinup and nature run
### Create initial conditions
### Create observation
### Data Assimilation with LETKF
### Data Assimilation with LETKF bias correction
### Reservoir computing
### Visualization

## Changing configurations

## References
About the model:  
[Lorenz, 1996](http://eaps4.mit.edu/research/Lorenz/Predicability_a_Problem_2006.pdf)  
[Lorenz and Emanuel, 2003](https://journals.ametsoc.org/doi/full/10.1175/1520-0469%281998%29055%3C0399%3AOSFSWO%3E2.0.CO%3B2)  
Correction of subgrid-scale effects:  
[Wilks, 2005](https://doi.org/10.1256/qj.04.03)  
[Danforth and Kalnay, 2008](https://journals.ametsoc.org/doi/full/10.1175/2007JAS2419.1)  
[Pulido et al. 2016](https://rmets.onlinelibrary.wiley.com/doi/abs/10.1002/qj.2879)  
