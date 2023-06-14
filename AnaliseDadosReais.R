require(flexsurvcure)
require(ggfortify)

options(scipen = 99)
source("D:\\Estudos\\TCC\\simFunctions.R")

analiseReal = function (df, simTau) {
  simOut = matrix(nrow=1,ncol=2*9+2*12+11)
  colnames(simOut) <- c("M-Z","Shen","qn",
                        paste("exp",c("pi","rate","piLB","piUB","rateLB","rateUB","AIC",'loglik',"ratio"),sep="."),
                        paste("expUnc",c("pi","rate","piLB","piUB","rateLB","rateUB","AIC",'loglik',"ratio"),sep="."),
                        paste("wei",c("pi","shape","scale","piLB","piUB","shapeLB","shapeUB","scaleLB","scaleUB","AIC",'loglik',"ratio"),sep="."),
                        paste("weiUnc",c("pi","shape","scale","piLB","piUB","shapeLB","shapeUB","scaleLB","scaleUB","AIC",'loglik',"ratio"),sep="."),
                        "RVExp","RVWei","EventRate","LastObs",
                        "pHat","pCens","lastObs","isCens")
  row.names(simOut) = 'Resultado'
  simOut[1,1] <- mzTest(df)
  simOut[1,2] <- shenTest(df)
  simOut[1,3] <- qnTest(df)
  
  simOut[1,4:12] <- ratioTest(df,simTau,"exp")
  simOut[1,13:21] <- ratioTest(df,simTau,"expUnc")
  
  simOut[1,22:33] <- ratioTest(df,simTau,"wei")
  simOut[1,34:45] <- ratioTest(df,simTau,"weiUnc")
  
  simOut[1,46] <- rvTest(simOut[1,20], simOut[1,11])
  simOut[1,47] <- rvTest(simOut[1,44], simOut[1,32])
  
  simOut[1,48:49] <- c(mean(df$D==1),df$D[which.max(df$Y)]==1)
  
  simOut[1,50:53] <- immuneTest(df)
  
  return(t(simOut))
}


# Cantor e Shuster (1992) ----
# Tempo de remissão de cancer em 78 pacientes pediátricos dividos em 2 grupos
# G1 com 42 e G2 com 36 (10 anos de acompanhamento)

# G1 todas as 5 recorrências ocorreram nos primeiros 4 anos de estudo
# G1 pode conter uma proporção de Imunes

# G2 teve recorrencia da doença ao longo de todo o estudo
# G2 pode não conter proporção de Imunes

tempo = c(0.9, 1.06, 1.12, 1.98, 2.05, 2.41, 2.44, 2.58, 3.18, 3.33, 3.4,
          3.48, 3.89, 4.39, 4.41, 4.55, 4.63, 5.06, 5.32, 5.52, 5.60, 5.69,
          6.11, 6.27, 6.31, 6.63, 6.66, 6.88, 7.97, 8.03, 8.16, 8.20, 8.25,
          8.27, 8.36, 8.45, 8.47, 8.82, 8.85, 9.1, 9.31, 9.36, 0.1, 2.15, 
          2.31, 4.89, 5.42, 5.50, 5.53, 5.58, 5.61, 5.87, 5.96, 5.96, 6.01, 
          6.06, 6.14, 6.14, 6.29, 6.34, 6.37, 6.61, 6.71, 6.76, 6.90, 7.42, 
          7.92, 8.05, 8.10, 8.12, 8.23, 9.32, 9.38, 9.46, 9.79, 9.85, 10.33, 10.65)

cens = c(1, 1, 1, 0, 1, 0, 0, 0, 0, 1, rep(0, 32), 
         1, 1, rep(0,24), 1, 0, 0, 1, 0, 0, 0, 1, 0, 0)

grupo = c(rep(1, 42), rep(2, 36))

dt1 = data.frame('Y' = tempo, 'D' = cens, grupo)

km1 = survfit(Surv(Y, D) ~ grupo, data=dt1)

png('dados_pediatricos.png', height = 500, width = 800)
autoplot(km1, conf.int = FALSE,
         censor.shape = '+', censor.size = 5, facets = TRUE, nrow = 2, lwt = 1) +
  scale_color_discrete(name = 'Grupos') +
  ylim(0, 1) +
  theme_bw() +
  theme(text = element_text(size=20))
dev.off()

set.seed(54)
### Seleção de modelos
# Supondo que existe imunes, para o grupo 1, o melhor modelo é o Weibull
# curereg1Exp <- flexsurvcure(Surv(Y, D) ~ 1, data=dt1[dt1$grupo == 1,],dist = 'exp')
curereg1Wei <- flexsurvcure(Surv(Y, D) ~ 1, data=dt1[dt1$grupo == 1,],dist = 'weibull')
administrativeWei = plyr::round_any(quantile(rweibull(10000, plyr::round_any(curereg1Wei$res, 0.01)[2,1], 
                                                      plyr::round_any(curereg1Wei$res, 0.01)[3,1]), 
                                             1 - c(0.25,0.1,0.05,0.01,0.001)), 0.25)
# Supondo que existe imunes, para o grupo 2, o melhor modelo é o Exponencial
curereg2Exp <- flexsurvcure(Surv(Y, D) ~ 1, data=dt1[dt1$grupo == 2,],dist = 'exp', link='probit')
# curereg2Wei <- flexsurvcure(Surv(Y, D) ~ 1, data=dt1[dt1$grupo == 2,],dist = 'weibull')
administrativeExp = plyr::round_any(quantile(rexp(10000, plyr::round_any(curereg2Exp$res, 0.01)[2,1]), 
                                             1 - c(0.25,0.1,0.05,0.01,0.001)), 0.25)

# Modelos selecionados
# WEIBULL para o grupo 1
reg1Wei <- flexsurvreg(Surv(Y, D) ~ 1, data=dt1[dt1$grupo == 1,],dist = 'weibull')

# Exponencial para o grupo 1
reg2Exp <- flexsurvreg(Surv(Y, D) ~ 1, data=dt1[dt1$grupo == 2,],dist = 'exp')


tabResultados1 = list('wei' = list(),'exp' = list())

tabelaResultados = analiseReal(dt1[dt1$grupo == 1,], 10)
tabResultados1[[1]] = list(round(tabelaResultados, 4))
tabelaResultados = analiseReal(dt1[dt1$grupo == 2,], 10)
tabResultados1[[2]] = list(round(tabelaResultados, 4))


### Testes para a imunidade
# Teste RV conclui que há imunes 1 e não no grupo 2
(rvTest(reg1Wei$loglik, curereg1Wei$loglik) < 0.05)
(rvTest(reg2Exp$loglik, curereg2Exp$loglik) < 0.05)

# Teste PN
(immuneTest(dt1[dt1$grupo == 1,]))
(immuneTest(dt1[dt1$grupo == 2,]))

### Teste tempo de observação suficiente
# Teste de Shen
(shenTest(dt1[dt1$grupo == 1,]) < 0.05)
(shenTest(dt1[dt1$grupo == 2,]) < 0.05)

# Teste QN
(qnTest(dt1[dt1$grupo == 1,]))
(qnTest(dt1[dt1$grupo == 2,]))

# Teste RECeUS-AIC
# ## Para G1 conclui que há imunes
# Supondo uma taxa de não curados e censurados <= 5% 
# temos que o tempo de observação também é suficiente neste caso
# Para este grupo o modelo selecionado pelo AIC foi o  Weibull
for (adm in administrativeExp) {
  print(ratioTest(dt1[dt1$grupo == 1,], adm, dist ='exp'))
  cat('\n')
}

for (adm in administrativeWei) {
  print(ratioTest(dt1[dt1$grupo == 1,], adm, dist ='wei'))
  cat('\n')
}

# ## Para G2 conclui-se que não há imunes
# Para este grupo o modelo selecionado pelo AIC foi o Exponencial
for (adm in administrativeExp) {
  print(ratioTest(dt1[dt1$grupo == 2,], adm, dist ='exp'))
  cat('\n')
}

for (adm in administrativeWei) {
  print(ratioTest(dt1[dt1$grupo == 2,], adm, dist ='wei'))
  cat('\n')
}

# Gehan 1965; Freireich et al. 1963) ----
# Tempo (em semanas) de remissão de 42 pacientes com leucemia divido em 2 grupos
# O conjunto de dados famoso geralmente usado para
# ilustrar exposição sobre analise de sobrevivencia
Y = c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23,
          6,6,6,6,7,9,10,10,11,13,16,17,19,20,22,23,25,32,32,34,35)

D = c(rep(1, 21),
         0,1,1,1,1,0,0,1,0,1,1,0,0,0,1,1,0,0,0,0,0)

grupo = c(rep(1,21), rep(2,21))

dt2 = data.frame(Y, D, grupo)

km2 = survfit(Surv(Y, D) ~ grupo, data=dt2)
png('dados_leucemia.png', height = 500, width = 800)
autoplot(km2, conf.int = FALSE,
         censor.shape = '+', censor.size = 5, facets = TRUE, nrow = 2, lwt = 1) +
  scale_color_discrete(name = 'Grupos') +
  ylim(0, 1) +
  theme_bw() +
  theme(text = element_text(size=20))
dev.off()

set.seed(54)
### Seleção de modelos
# Supondo que existe imunes, para o grupo 1, o melhor modelo é o Weibull
# curereg1Exp <- flexsurvcure(Surv(Y, D) ~ 1, data=dt2[dt2$grupo == 1,],dist = 'exp')
curereg1Wei <- flexsurvcure(Surv(Y, D) ~ 1, data=dt2[dt2$grupo == 1,],dist = 'weibull')
administrativeWei1 = plyr::round_any(quantile(rweibull(10000, plyr::round_any(curereg1Wei$res, 0.01)[2,1], 
                                                      plyr::round_any(curereg1Wei$res, 0.01)[3,1]), 
                                             1 - c(0.25,0.1,0.05,0.01,0.001)), 0.25)

# Supondo que existe imunes, para o grupo 2, o melhor modelo é o Exponencial
# curereg2Exp <- flexsurvcure(Surv(Y, D) ~ 1, data=dt2[dt2$grupo == 2,],dist = 'exp')
curereg2Wei <- flexsurvcure(Surv(Y, D) ~ 1, data=dt2[dt2$grupo == 2,],dist = 'weibull')
administrativeWei2 = plyr::round_any(quantile(rweibull(10000, plyr::round_any(curereg2Wei$res, 0.01)[2,1], 
                                                      plyr::round_any(curereg2Wei$res, 0.01)[3,1]), 
                                             1 - c(0.25,0.1,0.05,0.01,0.001)), 0.25)

# Modelos selecionados
# WEIBULL para o grupo 1
reg1Wei <- flexsurvreg(Surv(Y, D) ~ 1, data=dt2[dt2$grupo == 1,],dist = 'weibull')

# Exponencial para o grupo 1
reg2Wei <- flexsurvreg(Surv(Y, D) ~ 1, data=dt2[dt2$grupo == 2,],dist = 'weibull')

tabResultados2 = list('G1' = list(),'G2' = list())

for (adm in administrativeWei1) {
  tabelaResultados = analiseReal(dt2[dt2$grupo == 1,], adm)
  tabResultados2[[1]][paste0(adm)] = list(round(tabelaResultados, 4))
}

for (adm in administrativeWei2) {
  tabelaResultados = analiseReal(dt2[dt2$grupo == 2,], adm)
  tabResultados2[[2]][paste0(adm)] = list(round(tabelaResultados, 4))
}

### Testes para a imunidade
# Teste RV conclui que não há imunes nos dois grupos
(rvTest(reg1Wei$loglik, curereg1Wei$loglik) < 0.05)
(rvTest(reg2Wei$loglik, curereg2Wei$loglik) < 0.05)

# Teste PN
(immuneTest(dt2[dt2$grupo == 1,]))
(immuneTest(dt2[dt2$grupo == 2,]))

### Teste tempo de observação suficiente
# Teste de Shen
(shenTest(dt2[dt2$grupo == 1,]) < 0.05)
(shenTest(dt2[dt2$grupo == 2,]) < 0.05)

# Teste QN
(qnTest(dt2[dt2$grupo == 1,]))
(qnTest(dt2[dt2$grupo == 2,]))

# Teste RECeUS-AIC
# ## Para G1 não há imunes
# Para este grupo o modelo selecionado pelo AIC foi o  Weibull
for (adm in administrativeExp) {
  print(ratioTest(dt2[dt2$grupo == 1,], adm, dist ='exp'))
  cat('\n')
}

for (adm in administrativeWei) {
  print(ratioTest(dt2[dt2$grupo == 1,], adm, dist ='wei'))
  cat('\n')
}

# ## Para G2, há imunes
# Supondo uma taxa de não curados e censurados <= 1% 
# temos que o tempo de observação também é suficiente nestes casos
# Para este grupo o modelo selecionado pelo AIC foi o Exponencial
for (adm in administrativeExp) {
  print(ratioTest(dt2[dt2$grupo == 2,], adm, dist ='exp'))
  cat('\n')
}

for (adm in administrativeWei) {
  print(ratioTest(dt2[dt2$grupo == 2,], adm, dist ='wei'))
  cat('\n')
}
