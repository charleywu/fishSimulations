#code for plots
#Imen Bouhlel and Charley Wu, July 2018

#load packages
packages <- c('ggplot2', 'gganimate', 'ggimage', 'ggthemes', 'rapport', 'RColorBrewer',  'animation')
lapply(packages, require, character.only=TRUE)

### Data formatting ###
Data <- data.frame(t = numeric(), fish = numeric(), X = numeric(), Y = numeric(), orientation = numeric())

#reformat data computed in fish.R 
for (time in (1:(timesteps))){
  singleData <- as.data.frame(position[,,time])
  colnames(singleData)<- c('X', 'Y')
  for (r in (1:agents)){
    singleData$orientation[r] <- atan2(velocity[r,2,time], velocity[r,1,time]) #calculate orientation in degrees
    singleData$changeDirection[r] <- atan2(direction[r,2,time], direction[r,1,time]) #calculate changeDirection in degrees
    singleData$thetaAngle[r] <- thetas[r,1,time] #theta angle
  }
  singleData$X = as.numeric(singleData$X)
  singleData$Y = as.numeric(singleData$Y)
  singleData$orientation <- as.numeric(singleData$orientation)
  singleData$changeDirection <- as.numeric(singleData$changeDirection)
  singleData$thetaAngle <- as.numeric(singleData$thetaAngle)
  singleData$fish= sequence(agents)
  singleData$t = time
  
  Data <- rbind(Data, singleData)
}#summary(Data)


####################################################################################
#Plot fish movement (without environment) but orientation and change direction vectors
####################################################################################

p <- ggplot(data=Data,  aes( x = X, y = Y, frame = t, image="fish_icon.png")) +
  geom_image( size=.05 , by="height")+  #, orientation=45 
  #geom_point(size=3,alpha = .5,show.legend=F)+
  scale_size_identity()+
  xlim(-1, 1)+
  ylim(-1, 1)+
  theme_classic() +
  geom_spoke(aes(angle=orientation), radius = 0.1, arrow=arrow(length = unit(0.1,"cm")))+
  geom_spoke(aes(angle=changeDirection), color='red', radius = 0.1, arrow=arrow(length = unit(0.1,"cm")))
  geom_label_repel(aes(label = fish),box.padding   = 0.1, point.padding = 0.05, size = 3, segment.color = 'grey50', show.legend=F)

ani.options(interval = 0.2)
gganimate(p)
gganimate(p, file="fish_animation_with_speed.gif", fps=10) # Save

####################################################################################
#Plot fish movement with environment
####################################################################################

zmin <- min(resourcesEnvironment$z, na.rm=TRUE)
zmax <- max(resourcesEnvironment$z, na.rm=TRUE)
breaks <- pretty(c(zmin,zmax),101)
colours <- heat.colors(length(breaks)-1)
p_combined <- ggplot(data=resourcesEnvironment,  aes(x = x, y = y, frame = t, fill = z)) +
  geom_raster()+
  theme_classic()+
  xlim(-1, 1)+
  ylim(-1, 1)+
  scale_fill_distiller(palette = "Spectral", breaks = breaks, labels = NULL, name = "Reward")+
  geom_image( data=Data,  inherit.aes = FALSE,  aes( x = X, y = Y, frame = t, image="fish_icon.png"), size=.05 , by="height")+
  labs(subtitle = paste(" Environment=",environment_number," lamba=",environment_lambda," sigma=",environment_sigma," exp=",environment_exp,"\n PersonalSpace=",personalSpace,"  OrientationSpace=",delta_o,"  AttractionSpace=", delta_a,"\n Theta=", theta,"  MinSpeed=", min_speed,"  MaxSpeed=", max_speed, sep=""))+
  theme(plot.subtitle=element_text(size=13, color="black"))
  

ani.options(interval = 0.1)
gganimate(p_combined)
#gganimate(p_combined, title_frame = TRUE, file=paste("simulations/Environment",environment_number,"lamba",environment_lambda,"sigma",environment_sigma,"exp",environment_exp,"PersonalSpace",personalSpace,"OrientationSpace",delta_o,"AttractionSpace",delta_a,"Theta",theta,"MinSpeed",min_speed,"MaxSpeed",max_speed,".gif"), ani.width = 750, ani.height = 700, fps=10) # Save
