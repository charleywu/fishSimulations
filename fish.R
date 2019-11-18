#implementation of Fish Schooling behavior from Couzin et al. 2002
#Charley Wu and Imen Bouhlel, July 2018
#house cleaning
rm(list=ls())


#load packages
packages <- c('ggplot2', 'gganimate', 'rapport', 'ppls', 'bio3d')
lapply(packages, require, character.only=TRUE)


###simulation parameters
#Environment parameters
agents <- 20
#timesteps <- 200
timesteps <- 199
minVal <- 1 #initial range of environment
maxVal <- 100
dimensions <- 2
environment_rangeX <- 100
environment_rangeY <- 100
environmentCoordinates <- seq(minVal,maxVal, length=environment_rangeX)
environment_dir <- 'light_fields/envs/1-1en01/'

#load environment
readEnv <- function(targetDir, maxTsteps){
  envDF <- data.frame()
  gridCoords <- expand.grid(x=seq(minVal,maxVal), y = seq(minVal,maxVal))
  for (t in seq(0,maxTsteps)){
    file <- paste0(targetDir,'t', t,'.csv')
    reward <- as.vector(as.matrix((read.csv(file, header=F,  row.names=NULL))))
    tStep <- data.frame(x = gridCoords$x, y = gridCoords$y, z = reward, t = t )
    envDF <- rbind(envDF,tStep )
  }
  return(envDF)
}
resourcesEnvironment<-readEnv(environment_dir, timesteps)
#normalization
normalize <- function(x){ return((x- min(x)) /(max(x)-min(x)))}

resourcesEnvironment$z <- normalize(1 - resourcesEnvironment$z) #invert then normalize

#Fish parameters 
personalSpace <- 0.1 #this is what defines the zone of repulsion, which is a sphere (or circle) with this as the radius
delta_o <- 0.2 #distance between zone of repulsion and end of zone of orientation
delta_a <- 0.2 #distance between zone of orientation and zone of attraction
alpha <- 0.9*pi #field of perception in rads (2*pi~=6.28 rads = 360 deg)
theta <- 1.5 #turning rate (in rads)
min_speed <- 1 #minimum movement speed
max_speed <- 10 #maximum movement speed
sigma <- 0.02 #random angle change (in rads) 

#agents are each defined by a position and velocity at each time point
position <- array(NA, dim=c(agents, dimensions, timesteps+1)) # 3-dimensional matrix defining the location of each agent at each timestep
discrete_position <- array(NA, dim=c(agents, 1, timesteps+1)) # 3-dimensional matrix defining the discretized location of each agent at each timestep
velocity <- array(NA, dim=c(agents, dimensions, timesteps+1)) #3-dimensional matrix defining the velocity of each agent at each timestep
direction <- array(NA, dim=c(agents, dimensions, timesteps+1)) #3-dimensional matrix defining a unit vector each agent desires to change towards
rewards <- array(NA, dim=c(agents, 1, timesteps+1)) #3-dimensional matrix defining rewards (taking into account equal splitting)
thetas <- array(theta, dim=c(agents, 1, timesteps+1)) #3-dimensional matrix defining theta angles each agent uses to change towards the desired direction
speeds <- array(min_speed, dim=c(agents, 1, timesteps+1)) #3-dimensional matrix defining speeds agents use to move
zor <- array(0, dim=c(agents, agents, timesteps+1)) #zone of repulsion
zoa <- array(0, dim=c(agents, agents, timesteps+1)) #zone of attraction
zoo <- array(0, dim=c(agents, agents, timesteps+1)) #zone of orientation


################################################################################################################################################################################################
# Useful Functions
################################################################################################################################################################################################

#basic function for computing angles between two vectors (in rads)
angle <- function(x,y){ 
  if (all.equal(x,y)==TRUE){
    theta <- 0
  }
  else{
    dot.prod <- x%*%y 
    norm.x <- norm(x, type='2')
    norm.y <- norm(y, type='2')
    theta <- acos(dot.prod / (norm.x * norm.y))
    as.numeric(theta)
  }
  return(theta)
}

#discretize position. Returns a vector (x discrete coordinate, y discrete coordinate)
#avoid using equality tests/conditions with this function's output because of "floating point problem"
discrete_coordinates <- function(fish, timestep){
  coordinates <- c()
  for (d in 1:dimensions){
    dimPosition=position[fish,d,timestep]
    dimIndex <- which(abs(environmentCoordinates-dimPosition)==min(abs(environmentCoordinates-dimPosition)))
    dimCoordinate <- environmentCoordinates[dimIndex]
    coordinates[d] <- dimCoordinate
  }
  return (coordinates)
}

#returns index of discretized position in environment dataframe
discrete_coordinates_index <- function(fish, timestep){
  xPosition=position[fish,1,timestep]
  xIndex <- which(abs(environmentCoordinates-xPosition)==min(abs(environmentCoordinates-xPosition)))
  
  yPosition=position[fish,2,timestep]
  yIndex <- which(abs(environmentCoordinates-yPosition)==min(abs(environmentCoordinates-yPosition)))

  index_in_environment_df <- ((timestep-1)*environment_rangeX*environment_rangeY) +  ((yIndex-1)*environment_rangeY) + xIndex
  return (index_in_environment_df)
  
}

#returns reward corresponding to position
compute_reward <- function(fish, timestep){
  index_in_environment_df <- discrete_position[fish,,timestep]
  reward <- resourcesEnvironment[index_in_environment_df,]$z 
  return (reward)
}

#equally splits the rewards among agents at the same position
split_rewards <- function (timestep){
  #check for duplicate positions. Returns a vector corresponding to the total number of agents with the same position
  discrete_position_matrix <- as.matrix(discrete_position[,,timestep])
  duplicates <- sapply(1:agents, FUN=function(i) sum(apply(discrete_position_matrix, 1, function(j) identical(j, discrete_position_matrix[i,]))))
  rewards[,,timestep] <- rewards[,,timestep]/duplicates
  
}


#randomly initialize starting position and velocity
for (i in 1:agents){
  position[i,,1] <- runif(2, min = minVal, max=maxVal) #randomly generate position
  discrete_position[i,,1] <- discrete_coordinates_index(i,1)
  v <- runif(2, min = minVal, max=maxVal) #randomly generate velocity, and then scale it to unit length
  velocity[i,,1] <- normalize.vector(v)
  rewards[i,,1] <- compute_reward(i,1)
}#plot(velocity[,,1])
#split_rewards(1)



################################################################################################################################################################################################
# Simulation Loop
################################################################################################################################################################################################

for (t in 1:timesteps){ #plot(position[,,t])
  #put current velocity vector into change direction matrix as the default (if no social interactions result in any changes) 
  direction[,,t]<- velocity[,,t]
  #Calculate pairwise distances
  pairwiseDist <- as.matrix(dist(position[,,t], diag=FALSE)) #compute pairwise distance matrix
  diag(pairwiseDist) <- NA #replace diagonals with NA
  #Find pairwise relationships between agents for each of the 3 zones (before checking for blind spot)
  repulsionCandidates <- which(pairwiseDist<=personalSpace, arr.ind = TRUE) #which agents are potentially in each other's zone of repulsion?
  orientationCandidates <- which(pairwiseDist>personalSpace & pairwiseDist<=(personalSpace+delta_o), arr.ind = TRUE)
  attractionCandidates <- which(pairwiseDist>(personalSpace+delta_o) & pairwiseDist<=(personalSpace+delta_o+delta_a), arr.ind = TRUE)
  #FIRST RULE OF FISH CLUB: Repulsion comes first
  if (length(repulsionCandidates)>0){ #if not null, add to zor #Note: this is assuming zor is not affected by alpha (as it is stated in the paper)
    for (pair in 1:nrow(repulsionCandidates)){
      i <- repulsionCandidates[pair,1]
      j <- repulsionCandidates[pair,2]
      zor[i,j,t]<- 1 #add agent j to agent i's zone of repulsion
    }
  } #zor[,,t]
  #Implement Repulsion and add change direction to direction matrix
  for (i in 1:agents){
    if (sum(zor[i,,t])!=0){ #if agent has someone in zor
      self<- position[i,,t]
      others<-position[,,t][as.logical(zor[i,,t]),]
      directionVecs <- others-self #calculate difference between position of self and other
      if (is.null(nrow(directionVecs))){#if one other in zor
        normalize.vector(directionVecs)
        normalize.vector(directionVecs)
      }
      else if (!is.null(nrow(directionVecs))){ #if multiple others in zor, sum
        #normalize each of the direction vectors first
        for (z in 1:nrow(directionVecs)){
          directionVecs[z,] <- normalize.vector(directionVecs[z,])
          directionVecs[z,] <- normalize.vector(directionVecs[z,])
        }
        directionVecs <- colSums(directionVecs) #sum over columns
      }
      direction[i,,t] <- -(directionVecs) #sum direction vectors, normalized to unit length, and convert to negative (i.e., repulsion)
    }
  } # repulsions <- sapply(1:agents, FUN=function(j) angle(direction[j,,t], velocity[j,,t]))
  # #SECOND RULE OF FISH CLUB: orientation and attraction zones  #only if no repulsion candidates
  for (i in 1:agents){
    if (sum(zor[i,,t])==0){ #if not repulsed by any other fish
      #check if it has orientation or repulsion candidates (within radIus)
      zoo_candidates <- position[orientationCandidates[orientationCandidates[,1]==i,2],,t]
      zoa_candidates<- position[attractionCandidates[attractionCandidates[,1]==i,2],,t]
      #check if not in blind spot
      if (length(zoo_candidates)>0){ #first for zone of orientation
        #check if angle is within field of view (ifelse function is for separate cases of a single candidate or multiple candidates)
        #calculate each angle between agent i's velocity and candidate zoo agent's position)
        if(!is.null(nrow(zoo_candidates))){
          zoo_angles <- sapply(1:nrow(zoo_candidates), FUN=function(r) angle(zoo_candidates[r,], velocity[i,,t]))
        } else if (is.null(nrow(zoo_candidates))){
          zoo_angles <- angle(zoo_candidates, velocity[i,,t])
        }
        #which j agents are visible?
        visibleZooAgents <- orientationCandidates[orientationCandidates[,1]==i,2][zoo_angles<(alpha/2)]
        zoo[i,visibleZooAgents,t]<- 1 #add to zone of orientation matrix
      }#zoo[i,,t]
      if (length(zoa_candidates)>0){ #repeat for zone of attraction
        #calculate each angle between agent i's velocity and candidate zoo agent's position
        if(!is.null(nrow(zoa_candidates))){
          zoa_angles <- sapply(1:nrow(zoa_candidates), FUN=function(r) angle(zoa_candidates[r,], velocity[i,,t]))
        } else if (is.null(nrow(zoa_candidates))){
          zoa_angles <- angle(zoa_candidates, velocity[i,,t])
        }
        #which j agents are visible?
        visibleZoaAgents <- attractionCandidates[attractionCandidates[,1]==i,2][zoa_angles<(alpha/2)]
        zoa[i,visibleZoaAgents,t]<- 1 #add to zone of attraction matrix
      }#zoa[i,,t]
      #Now let's calculate movement, where d_o is based on orientation (velocity matrix) of zoo members and d_a is based on position of zoa members
      if (sum(zoo[i,,t])>1){
        d_o<- colSums(normalize.vector(velocity[as.logical(zoo[i,,t]),,t])) # sum them up if multiple agents in zoo
      }else if (sum(zoo[i,,t])==1){
        d_o <- normalize.vector(velocity[as.logical(zoo[i,,t]),,t])
      }else if (sum(zoo[i,,t])==0){
        d_o <- NULL
      }
      
      self<- position[i,,t]
      others<-position[,,t][as.logical(zoa[i,,t]),]
      directionVecs <- others-self #calculate difference between position of self and other
      if (sum(zoa[i,,t])>1){
        for (z in 1:nrow(directionVecs)){
          directionVecs[z,] <- normalize.vector(directionVecs[z,])
          directionVecs[z,] <- normalize.vector(directionVecs[z,])
        }
        directionVecs <- colSums(directionVecs) #sum over columns
        d_a <- directionVecs
      }else if (sum(zoa[i,,t])==1){
        normalize.vector(directionVecs)
        normalize.vector(directionVecs)
        d_a <- directionVecs
      }else if (sum(zoa[i,,t])==0){
        d_a <- NULL
      }
      #change vector
      d_i <- rbind(d_o, d_a) #put the two together
      if (!is.null(d_i)){ #(nrow(d_i)>0){ #if at least one vector present
        d_i <- colMeans(replace(d_i, d_i==0, NA), na.rm = TRUE)  #if only one vector is present, then set d_i to that. If both d_o and d_a are defined, then it is a mean of the two
        direction[i,,t] <- d_i
        if (all(d_i == 0)){
          direction[i,,t]<- velocity[i,,t]
        }
      }
    }
  }#plot(direction[,,t])
  
  #Stochastic effects
  direction[,,t] <- direction[,,t] + array(rnorm(n=agents*dimensions,mean=0,sd=sigma), dim=c(agents,dimensions))
  
  #MOVE EACH FISH
  thetas[,,t] <- thetas[,,t] + rnorm(n=agents,mean=0,sd=sigma) #add noise to theta
  for (i in 1:agents){
    thetas[i,,t] <- min(sign(pi-angle(velocity[i,,t], direction[i,,t]))*thetas[i,,t], angle(velocity[i,,t], direction[i,,t])) #choose the minimum of either the max direction change + noise (signed positively or negatively according to the desired direction), or the total angular diference between the current direction and the direction we want to turn towards
    #update velocity (i.e., orientation)
    velocity[i,1,(t+1)] <-  cos(thetas[i,,t])*velocity[i,1,t] - sin(thetas[i,,t])*velocity[i,2,t]
    velocity[i,2,(t+1)] <-  sin(thetas[i,,t])*velocity[i,1,t] + cos(thetas[i,,t])*velocity[i,2,t]
    
    #calculate speed (based on the obtained reward on the current tstep)
    speeds[i,,t] <- min_speed + ((1-rewards[i,,t])*(max_speed - min_speed))

    #update position
    position[i,,(t+1)] <- position[i,,t] +  velocity[i,,(t+1)]*speeds[i,,t]
    discrete_position[i,,(t+1)] <- discrete_coordinates_index(i,(t+1))
    rewards[i,,(t+1)] <- compute_reward(i,t+1)
    #check whether outside of environment bounds
    while (any(position[i,,t+1]<=minVal) | any(position[i,,t+1]>=maxVal) | all(position[i,,t+1]<=minVal) | all(position[i,,t+1]>=maxVal)){
      #if yes, choose a random angle and recalculate new position
      thetas[i,,t] <- runif(1, min = 0, max = (2*pi))
      velocity[i,1,(t+1)] <-  cos(thetas[i,,t])*velocity[i,1,t] - sin(thetas[i,,t])*velocity[i,2,t]
      velocity[i,2,(t+1)] <-  sin(thetas[i,,t])*velocity[i,1,t] + cos(thetas[i,,t])*velocity[i,2,t]
      position[i,,(t+1)] <- position[i,,t] +  velocity[i,,(t+1)]*speeds[i,,t]
      discrete_position[i,,(t+1)] <- discrete_coordinates_index(i,(t+1))
      rewards[i,,(t+1)] <- compute_reward(i,t+1)
    }
    
  } #plot(velocity[,,(t+1)])
  
  #split_rewards(t+1)
  
  #debug
  print(t)
}


### Data formatting ###
Data <- data.frame(t = numeric(), fish = numeric(), X = numeric(), Y = numeric(), orientation = numeric())

#Reformat into dataframe
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

saveRDS(Data, 'fishSim.RDS')
