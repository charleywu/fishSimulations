The environments' data used in the simulation is at the format .json and can be found at the subdirectory "environments".

Steps:

1) Run environmentGenerator.py to generate enironments, which are then saved as json files in the subdirectory "environments".

2) run "gpEnvironmentDataAndPlotsGeneration.R" script in order to interpolate the existing environments to the desired spatial and temporal dimensions, and to convert and save them on a .Rdata format.

3) run "fish.R" script in order to create the simualtion data

4) run "plotting.R" in order to plot the animation corresponding to the simulation data


 Note, that while similar to the environments used in Berdahl et al (2010), we had difficulty in exactly replicating the environments used in the paper based on the information provided in the supplementary materials. (Specifically, it seems like there is a vector missing from the first term of Eq. 7 and couldn't consistently arrive at initial state for Eq. 14). If anyone has code for these environments, we would be very happy if you contacted us. These environments represent spatially and temporally correlated environments sampled from a Gaussian Process prior, using an RBF kernel for the light prfile composed with a white noise kernel for the noise component. Due to scalability, we generated a smaller environment, which was then interpolated in step 2 to construct the 100 x 100 x 100 environment