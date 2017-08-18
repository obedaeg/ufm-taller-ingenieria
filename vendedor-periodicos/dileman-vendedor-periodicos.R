library(ggplot2)
library(dplyr)

venta_periodicos <- function(compra_del_dia, dias_a_simular) {

  costo_periodico <- 0.30
  venta_periodico <- 0.5
  reciclaje_periodico <- 0.05
  
  tipo_dias <- c("excelente", "bueno", "malo")
  probabilidad_tipo_dia <- c(0.35,0.45,0.20)
  
  lista_tipo_demanda <- seq(from = 40, to = 100, by = 10 )
  probabilidad_venta_excelente <- c(0.03, 0.05, 0.15, 0.20, 0.35, 0.15, 0.07)
  probabilidad_venta_bueno <- c(0.10, 0.18, 0.40, 0.20, 0.08, 0.10, 0)
  probabilidad_venta_malo <- c(0.44, 0.22, 0.16, 0.12, 0.06, 0, 0)
  
  simulacion <- data.frame()
  
  for (i in 1:dias_a_simular){
    seleccion_tipo_dia <- sample(tipo_dias, 1, prob = probabilidad_tipo_dia)
    demanda_del_dia <- 0
    ganancia_del_dia <- 0
    if (seleccion_tipo_dia == 'malo')
      demanda_del_dia <- sample(lista_tipo_demanda, 1, prob = probabilidad_venta_malo)  
    if (seleccion_tipo_dia == 'bueno')
      demanda_del_dia <- sample(lista_tipo_demanda, 1, prob = probabilidad_venta_bueno)  
    if (seleccion_tipo_dia == 'excelente')
      demanda_del_dia <- sample(lista_tipo_demanda, 1, prob = probabilidad_venta_excelente)  
    
    if (demanda_del_dia >= compra_del_dia)
       ganancia_del_dia <- compra_del_dia*venta_periodico - compra_del_dia*costo_periodico - (demanda_del_dia - compra_del_dia)*(venta_periodico-costo_periodico)
    else
      ganancia_del_dia <- demanda_del_dia*venta_periodico - compra_del_dia*costo_periodico + (compra_del_dia - demanda_del_dia)*reciclaje_periodico
    
    row_simulacion <- data.frame(compra = compra_del_dia, TipoDia=seleccion_tipo_dia, Demanda=demanda_del_dia, Ganancia=ganancia_del_dia)
    simulacion <- rbind(simulacion, row_simulacion)
  }
  return(simulacion)
}

simular <- function(dias_a_simular = 20, resumen = TRUE){
  out <- data.frame()
  lista_tipo_compra <- seq(from = 40, to = 100, by = 10 ) 
  
  for (compra in lista_tipo_compra){
    
    resultado_simulacion <- venta_periodicos(compra, dias_a_simular)
    if (resumen)
      step_sim <- data.frame(promedio_ganancia_diaria = mean(resultado_simulacion$Ganancia),
                             desviacion_ganancia_diaria = sd(resultado_simulacion$Ganancia),
                             oferta = compra)
    else
      step_sim <- data.frame(ganancia = resultado_simulacion$Ganancia,
                             oferta = compra)
    out <- rbind(out,step_sim)
    
  }
  return(out)
}

result <- simular(resumen = FALSE)

View(result)


plot(result$oferta,result$promedio_ganancia_diaria,type = 'l')

simular(dias_a_simular = 2000, resumen = FALSE) %>% ggplot(aes(as.factor(oferta),ganancia))+
   geom_boxplot()






  