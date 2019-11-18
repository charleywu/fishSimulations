#code for plots
#Imen Bouhlel and Charley Wu, July 2018

#load packages
packages <- c('ggplot2', 'gganimate', 'ggimage', 'ggthemes', 'rapport', 'RColorBrewer')
lapply(packages, require, character.only=TRUE)


#############################################################################################
#Load data
#############################################################################################
Data <-  readRDS('fishSim.RDS')


####################################################################################
#Plot fish movement with environment
####################################################################################

zmin <- min(resourcesEnvironment$z, na.rm=TRUE)
zmax <- max(resourcesEnvironment$z, na.rm=TRUE)
breaks <- pretty(c(zmin,zmax),101)
colours <- heat.colors(length(breaks)-1)
p <- ggplot(data=resourcesEnvironment,  aes(x = x, y = y, fill = z)) +
  geom_raster()+
  transition_time(t) +
  theme_classic()+
  #scale_fill_distiller(palette = "Spectral", breaks = breaks, labels = NULL, name = "Reward")+
  scale_fill_gradient(low = "white", high = "black")+
  geom_image( data=Data,  inherit.aes = FALSE,  aes( x = X, y = Y, frame = t, image="fish_icon.png"), size=.05 , by="height")+
  #labs(subtitle = paste(" Environment=",environment_number," lamba=",environment_lambda," sigma=",environment_sigma," exp=",environment_exp,"\n PersonalSpace=",personalSpace,"  OrientationSpace=",delta_o,"  AttractionSpace=", delta_a,"\n Theta=", theta,"  MinSpeed=", min_speed,"  MaxSpeed=", max_speed, sep=""))+
  theme(plot.subtitle=element_text(size=13, color="black"), legend.position='none')

animate(p)
anim_save('gifs/FishSim.gif')

#ani.options(interval = 0.1)
#gganimate(p_combined)
#gganimate(p_combined, title_frame = TRUE, file=paste("simulations/Environment",environment_number,"lamba",environment_lambda,"sigma",environment_sigma,"exp",environment_exp,"PersonalSpace",personalSpace,"OrientationSpace",delta_o,"AttractionSpace",delta_a,"Theta",theta,"MinSpeed",min_speed,"MaxSpeed",max_speed,".gif"), ani.width = 750, ani.height = 700, fps=10) # Save
