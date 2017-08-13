rm(list=ls())
monty<-function(strategy, number_of_runs){
  doors <- 1:3
  winnings <- 0
 
   for (i in 1:number_of_runs)
  {
    prize_door <- floor(runif(1,1,4))
    guessed_door <- floor(runif(1,1,4))
    win_this_game <- FALSE
    
    if (prize_door != guessed_door)
      door_to_reveal <- doors[-c(prize_door,guessed_door)]
    else
      door_to_reveal <- sample(doors[-c(prize_door,guessed_door)],1)
    
    if (strategy == 'cambiar')
      selected_door <- doors[-c(door_to_reveal,guessed_door)]
    if (strategy == 'no_cambiar')
      selected_door <- guessed_door
    if (strategy == 'random')
      selected_door <- sample(doors[-door_to_reveal],1)
    
    if (selected_door == prize_door){
      winnings <- winnings + 1
      win_this_game <- TRUE
    }
      
}
 return(winnings/number_of_runs*100)
}
runsimulation<-function( number_of_runs){
  stay <- monty(strategy ='no_cambiar', number_of_runs)
  switch <- monty(strategy='cambiar', number_of_runs)  
  random <- monty(strategy='random', number_of_runs)
  
  percentages <- as.numeric(c(stay, switch, random))
  
  ylim <- c(0, 1.1*max(percentages))
  
  graph <- barplot(percentages, main = 'Problema Monty Hall', xlab = "Estrategia", ylab = "Porcentage",
          names.arg = c("No Cambiar", "Cambiar", "Random"), ylim = ylim)
  
  text(x = graph, y = percentages, label = paste0(percentages,'%'), pos = 3, cex = 0.8)
  
}

runsimulation(10)
runsimulation(30)
runsimulation(1000)
runsimulation(5000)
runsimulation(10000)

runif(1,1,4)
floor(runif(1,1,4))
