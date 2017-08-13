random<-function(number_of_times){
  vec <- c()
  for (i in 1:number_of_times){
    number <- sample(1:12,1)
    vec<- rbind(vec, number)
  }
  return(vec)
}

vec <- random(1000)
mean(vec)
fdata = factor(vec)
fdata
table <- table(fdata)
table
table_p <- table/10000 
table_p

plot(table,'l')
plot(table_p,'l')

sum(table_p)
sd(vec)
