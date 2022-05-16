  library("rjags")
  library(tidyverse)
  library(ltm)
  
  
  #Respostas
  data <- read_csv2("respostas.csv")
  perguntas <- colnames(data)
  colnames(data) <- c(paste0("i",1:13), paste0("f",1:3))
  
  #Gabarito
  gab <- read_csv2("gabarito.csv", col_names = FALSE)
  colnames(gab) <- c(paste0("i",1:13))
  
  #Desconsidera as linhas incompletas
  data.NA <- data[complete.cases(data), ]
  
  #Correção
  data.f <- mult.choice(data.NA[,1:13],as.numeric(gab))
  
  data.list <- data.f %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "id") %>%
    pivot_longer(i1:i13,
                 names_to = "item",
                 values_to = "resp") %>% 
    mutate(item = substr(item, 2,3)) %>% 
    as.list()
  
  
  data.list$N <- length(data.list$resp)
  data.list$Nid <- length(unique(data.list$id))
  data.list$Nitem <- length(unique(data.list$item))
  
  
  cat("model{
      
      ## cada resposta
      for(n in 1:N){
        resp[n] ~ dbern(z[n])
        logit(z[n]) <- -alpha[item[n]] + beta[item[n]]*theta[id[n]]
      }
      
      
      ## Para o individuo ind, i = 1, ..., Nid
      for(ind in 1:Nid){
        theta[ind] ~ dnorm(0, 1)
      }
      
      ## Para o item it, i = 1, ..., Nitem
      for(it in 1:Nitem){
        alpha[it] ~ dnorm(0, 1)
        beta[it] ~ dbeta(1, 1)
      }
  }", file="sim.jags")
  
  
  
  
  sim.jags <- jags.model(file = "sim.jags",
                         data = data.list, n.chains = 3, n.adapt = 1000)
  
  fit.jags <- coda.samples(model=sim.jags,
                           n.iter=10000, thin=10,
                           variable.names=c("alpha","beta", "theta"))
  
  save(fit.jags, file= "fit-jags.Rdata")

fitjags <- load("fit-jags.Rdata")

