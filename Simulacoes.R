require(doParallel)
require(foreach)
require(flexsurvcure)

source("D:\\Estudos\\TCC\\simFunctions.R")

n.cores <- 5
cl <- makeCluster(n.cores)
registerDoParallel(cl)

prop_cured = c(0.0, 0.1, 0.3, 0.6, 0.8)
sample_size = c(100, 250, 500, 1000)
uncured_rem = c(0.25, 0.1, 0.05, 0.01, 0.001)
administrativeExp = c(1.5, 2.25, 3.0, 4.25, 6.50)

simExp = curedExp = list()

cat('Distribuição Exponencial \n')
init = Sys.time()
cat(paste0("Tempo de inicio: ",init,"\n"))

for (cure in prop_cured) {
  sampleExp = list()
  for (sampled in sample_size) {
    temp = Sys.time()
    cat(paste0(cure, " - ", sampled, " - ", temp,"\n"))
    resultados = foreach (adm = administrativeExp) %dopar% {
      require(flexsurvcure)
      simExp[paste0(adm)] = list(simRun(sampled, 1000, 11320724, c(cure, 1), 'exp', adm, NULL))
      simExp
    }
    sampleExp[paste0(sampled)] = list(resultados)
    rm(resultados)
  }
  curedExp[paste0(cure)] = list(sampleExp)
}

end = Sys.time()
cat(paste0("Tempo final: ",end,"\n"))
cat(paste0("Tempo de execução: ",end-init,"\n"))

administrativeWei = c(1.25, 1.50, 1.75, 2.25, 2.75)

simWei = sampleWei = curedWei = list()

cat('Distribuição Weibull \n')
init = Sys.time()
cat(paste0("Tempo de inicio: ",init,"\n"))

for (cure in prop_cured) {
  sampleWei = list()
  for (sampled in sample_size) {
    temp = Sys.time()
    cat(paste0(cure, " - ", sampled, " - ", temp,"\n"))
    resultados = foreach (adm = administrativeWei) %dopar% {
      require(flexsurvcure)
      simWei[paste0(adm)] = list(simRun(sampled, 1000, 11320724, c(cure, 2, 1), 'wei', adm, NULL))
      simWei
    }
    sampleWei[paste0(sampled)] = list(resultados)
    rm(resultados)
  }
  curedWei[paste0(cure)] = list(sampleWei)
}

end = Sys.time()
cat(paste0("Tempo final: ",end,"\n"))
cat(paste0("Tempo de execução: ",end-init,"\n"))

for (cure in prop_cured) {
  for (sampled in sample_size) {
    for (adm in administrativeExp) {
      curedExp[[paste0(cure)]][[paste0(sampled)]][[paste0(adm)]] = curedExp[[paste0(cure)]][[paste0(sampled)]][[which(adm == administrativeExp)]][[paste0(adm)]]
      # curedExp[[paste0(cure)]][[paste0(sampled)]][[1]] = NULL
    }
  }
}

for (cure in prop_cured) {
  for (sampled in sample_size) {
    for (adm in administrativeWei) {
      curedWei[[paste0(cure)]][[paste0(sampled)]][[paste0(adm)]] = curedWei[[paste0(cure)]][[paste0(sampled)]][[which(adm == administrativeWei)]][[paste0(adm)]]
      # curedWei[[paste0(cure)]][[paste0(sampled)]][[1]] = NULL
    }
  }
}
