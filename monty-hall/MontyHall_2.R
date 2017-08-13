rm(list=ls())
montyHallAlgorithm<-function(strategy, number_of_doors, number_of_runs){
  doors<-1:number_of_doors
  winnings<-0 
  
  for(i in 1:number_of_runs)
  {
    prize_door<-floor(runif(1,1,number_of_doors+1)) 
    guessed_door<-floor(runif(1,1,number_of_doors+1)) 
    
    if(length(doors[-c(prize_door,guessed_door)]) != 1)
      door_to_reveal<-sample(doors[-c(prize_door,guessed_door)],1)
    else
      door_to_reveal<-doors[-c(prize_door,guessed_door)]
      
    if(strategy == 'cambiar')
      if (length(doors[-c(door_to_reveal,guessed_door)]) != 1)
        select <- sample(doors[-c(door_to_reveal,guessed_door)],1)
      else 
        select <- doors[-c(door_to_reveal, guessed_door)]
    
    if(strategy == 'no_cambiar')
      select <- guessed_door
    if(strategy == 'random')
      select <- sample(doors[-door_to_reveal],1)
    
    
    if(select == prize_door)
      winnings<-winnings+1
    
  }
  
  return(winnings/number_of_runs*100)
}
runsimulation<-function(doors, times){
  stay <- montyHallAlgorithm(strategy='no_cambiar', number_of_doors = doors, number_of_runs = times)
  switch <- montyHallAlgorithm(strategy='cambiar', number_of_doors = doors, number_of_runs = times)  
  random <- montyHallAlgorithm(strategy='random', number_of_doors = doors, number_of_runs = times)
  
  percentages <- as.numeric(c(stay, switch, random))
  
  ylim <- c(0, 1.1*max(percentages))
  
  graph <- barplot(percentages, main = 'Problema Monty Hall', xlab = "Estrategia", ylab = "Porcentage",
          names.arg = c("No Cambiar", "Cambiar", "Random"), ylim = ylim)
  
  text(x = graph, y = percentages, label = paste0(percentages,'%'), pos = 3, cex = 0.8)
  
}

runsimulation(3, 1000)
runsimulation(3, 5000)
runsimulation(3, 10000)

runsimulation(4, 1000)
runsimulation(4, 5000)
runsimulation(4, 10000)

runsimulation(5, 1000)
runsimulation(5, 5000)
runsimulation(5, 10000)

runsimulation(100, 10000)

