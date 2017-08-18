prensentar<-function(number_of_runs = 1000){
  results <- c()
  presenters <- c('Byron', 'Obed', 'Gilberto')
  presenters_prob <- c(0.4,0.4,0.2)
  for (i in 1:number_of_runs){
    presenter <- sample(presenters,1 , prob = presenters_prob)
    
    results <- rbind(results, presenter)
  }
  factors <- factor(results)
  table <- table(factors)
  percentages <- table/number_of_runs*100
  
  print (table)
  
  ylim <- c(0, 1.1*max(percentages))
  
  graph <-barplot(percentages, main = 'Quien Presenta', xlab = "Presentadores", ylab = "Porcentage",
          names.arg = c("Byron", "Gilberto", "Obed"), ylim = ylim)
  
  text(x = graph, y = percentages, label = paste0(percentages,'%'), pos = 3, cex = 0.8)
  
}


prensentar(1000)

