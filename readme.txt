The fish model was programmed by Charley Wu and Imen Bouhlel based on Couzin et al. 2002. The light field environments are from Berdahl et al (2010), and we're grateful to Peter Krafft for providing us his Python implementation. 

Steps:

1) Run `light_fields/main.py` generate enironments, which are then saved as csv files in the subdirectory `light_fields/envs`

2) Run `fish.R` in order to load an environment and run the simulation. The output is saved under `fishSim.RDS`

3) Run `plotting.R` in order to plot the animation corresponding to the simulation data (saved in `gifs/`)
