require(tidyverse)
require(plyr)

load("D:/Estudos/TCC/dados_limpos.RData")

administrativeWei = c(1.25, 1.50, 1.75, 2.25, 2.75)
administrativeExp = c(1.5, 2.25, 3.0, 4.25, 6.50)
sample_size = c(100, 250, 500, 1000)

percent = function(x) {
  return(sum(x, na.rm = T) / 1000)
}

# Testes para a proporção de Imunes ----
## Verifica o teste da razão de verossimilhança ----
testeRV = function(x){
  return(ifelse(x < 0.05, 1, 0))
}

### Weibull
RVWei = data.frame("Cure" = c(rep(0, 20), rep(0.1, 20), rep(0.3, 20), rep(0.6, 20), rep(0.8, 20)),
                   "Amostra" = c(rep(100, 5), rep(250, 5), rep(500, 5), rep(1000, 5)),
                   "Cens" = c(rep(c(25, 10, 5, 1, 0.1), 5)),
                   "Value" = rep(NA, 100))
rv = c()

for (i in 1:5) {
  for (j in 1:4) {
    for (k in 1:5) {
      
      value = data.frame(curedWei[[i]][[j]][[k]]) |> select(RVWei) |> 
        apply(MARGIN=2, testeRV) |> apply(MARGIN=2, percent)
      
      rv = c(rv, value)
    }
  }
}

RVWei$Value = rv
RVWei = RVWei |> pivot_wider(names_from='Cens', values_from = "Value")
RVWei

### Exponencial
RVExp = data.frame("Cure" = c(rep(0, 20), rep(0.1, 20), rep(0.3, 20), rep(0.6, 20), rep(0.8, 20)),
                   "Amostra" = c(rep(100, 5), rep(250, 5), rep(500, 5), rep(1000, 5)),
                   "Cens" = c(rep(c(25, 10, 5, 1, 0.1), 5)),
                   "Value" = rep(NA, 100))
rv = c()

for (i in 1:5) {
  for (j in 1:4) {
    for (k in 1:5) {
      
      value = data.frame(curedExp[[i]][[j]][[k]]) |> select(RVExp) |> 
        apply(MARGIN=2, testeRV) |> apply(MARGIN=2, percent)
      
      rv = c(rv, value)
    }
  }
}

RVExp$Value = rv
RVExp = RVExp |> pivot_wider(names_from='Cens', values_from = "Value")
RVExp

## Verifica o teste p_n ----
#### tabela A.2 pagina 138 (254) livro Maller e Zhou

teste_pn = function(x, b, n) {
  
  if (b == 2) {
    corte = ifelse(n == 100, 0.9074,
                   ifelse(n ==250, 0.9355,
                          ifelse(n == 500, 0.9490, 0.9580)))
  } else if (b == 4) {
    corte = ifelse(n == 100, 0.9405,
                   ifelse(n ==250, 0.9609,
                          ifelse(n == 500, 0.9707, 0.9777)))
  } else if (b == 6) {
    corte = ifelse(n == 100, 0.9563,
                   ifelse(n ==250, 0.9741,
                          ifelse(n == 500, 0.9817, 0.9869)))
  } else if (b == 10) {
    corte = ifelse(n == 100, 0.9736,
                   ifelse(n ==250, 0.9861,
                          ifelse(n == 500, 0.9910, 0.9941)))
  }
  
  return(ifelse(x < corte, 1, 0))
}

### Weibul
PNWei = data.frame("Cure" = c(rep(0, 20), rep(0.1, 20), rep(0.3, 20), rep(0.6, 20), rep(0.8, 20)),
                   "Amostra" = c(rep(100, 5), rep(250, 5), rep(500, 5), rep(1000, 5)),
                   "Cens" = c(rep(c(25, 10, 5, 1, 0.1), 5)),
                   "Value" = rep(NA, 100))

pn = c()

for (i in 1:5) {
  for (j in 1:4) {
    for (k in 1:5) {
      if (k %in% c(1,2,3)) {
        value = data.frame(curedWei[[i]][[j]][[k]]) |> 
          select('pHat') |> 
          apply(MARGIN=2, teste_pn, b=2, n=sample_size[j]) |> 
          data.frame()
        
        colnames(value) <- 'teste_pn'
        curedWei[[i]][[j]][[k]] = cbind(curedWei[[i]][[j]][[k]],value)
        value = value |> apply(MARGIN=2, percent)
      } else {
        value = data.frame(curedWei[[i]][[j]][[k]]) |> 
          select('pHat') |> 
          apply(MARGIN=2, teste_pn, b=4, n=sample_size[j]) |> 
          data.frame()
        
        colnames(value) <- 'teste_pn'
        curedWei[[i]][[j]][[k]] = cbind(curedWei[[i]][[j]][[k]],value)
        value = value |> apply(MARGIN=2, percent)
      }
      
      pn = c(pn, value)
    }
  }
}

PNWei$Value = pn
PNWei = PNWei |> pivot_wider(names_from='Cens', values_from = "Value")
PNWei

### Exponencial
PNExp = data.frame("Cure" = c(rep(0, 20), rep(0.1, 20), rep(0.3, 20), rep(0.6, 20), rep(0.8, 20)),
                   "Amostra" = c(rep(100, 5), rep(250, 5), rep(500, 5), rep(1000, 5)),
                   "Cens" = c(rep(c(25, 10, 5, 1, 0.1), 5)),
                   "Value" = rep(NA, 100))

pn = c()

for (i in 1:5) {
  for (j in 1:4) {
    for (k in 1:5) {
      if (k == 1) {
        value = data.frame(curedExp[[i]][[j]][[k]]) |> 
          select('pHat') |> 
          apply(MARGIN=2, teste_pn, b=2, n=sample_size[j])
        data.frame()
        
        colnames(value) <- 'teste_pn'
        curedExp[[i]][[j]][[k]] = cbind(curedExp[[i]][[j]][[k]],value)
        value = value |> apply(MARGIN=2, percent)
      } else if (k %in% c(2, 3)) {
        value = data.frame(curedExp[[i]][[j]][[k]]) |> 
          select('pHat') |> 
          apply(MARGIN=2, teste_pn, b=4, n=sample_size[j])
        data.frame()
        
        colnames(value) <- 'teste_pn'
        curedExp[[i]][[j]][[k]] = cbind(curedExp[[i]][[j]][[k]],value)
        value = value |> apply(MARGIN=2, percent)
      } else if (k == 4) {
        value = data.frame(curedExp[[i]][[j]][[k]]) |> 
          select('pHat') |> 
          apply(MARGIN=2, teste_pn, b=6, n=sample_size[j])
        data.frame()
        
        colnames(value) <- 'teste_pn'
        curedExp[[i]][[j]][[k]] = cbind(curedExp[[i]][[j]][[k]],value)
        value = value |> apply(MARGIN=2, percent)
      } else if (k == 5) {
        value = data.frame(curedExp[[i]][[j]][[k]]) |> 
          select('pHat') |> 
          apply(MARGIN=2, teste_pn, b=10, n=sample_size[j]) |> 
          data.frame()
      
        colnames(value) <- 'teste_pn'
        curedExp[[i]][[j]][[k]] = cbind(curedExp[[i]][[j]][[k]],value)
        value = value |> apply(MARGIN=2, percent)
      }
      
      pn = c(pn, value)
    }
  }
}

PNExp$Value = pn
PNExp = PNExp |> pivot_wider(names_from='Cens', values_from = "Value")
PNExp

## Verifica o teste RECeUS ----
teste_rnUnc = function(x) {
  return(ifelse(x > 0.025, 1, 0))
}

### Weibull
rnUncWei = data.frame("Cure" = c(rep(0, 20), rep(0.1, 20), rep(0.3, 20), rep(0.6, 20), rep(0.8, 20)),
                      "Amostra" = c(rep(100, 5), rep(250, 5), rep(500, 5), rep(1000, 5)),
                      "Cens" = c(rep(c(25, 10, 5, 1, 0.1), 5)),
                      "Value" = rep(NA, 100))
rn = c()

for (i in 1:5) {
  for (j in 1:4) {
    for (k in 1:5) {
      
      value = data.frame(curedWei[[i]][[j]][[k]]) |> select('wei.pi') |> 
        apply(MARGIN=2, teste_rnUnc) |> apply(MARGIN=2, percent)
      
      rn = c(rn, value)
    }
  }
}

rnUncWei$Value = rn
rnUncWei = rnUncWei |> pivot_wider(names_from='Cens', values_from = "Value")
rnUncWei

### Exponencial
rnUncExp = data.frame("Cure" = c(rep(0, 20), rep(0.1, 20), rep(0.3, 20), rep(0.6, 20), rep(0.8, 20)),
                      "Amostra" = c(rep(100, 5), rep(250, 5), rep(500, 5), rep(1000, 5)),
                      "Cens" = c(rep(c(25, 10, 5, 1, 0.1), 5)),
                      "Value" = rep(NA, 100))
rn = c()

for (i in 1:5) {
  for (j in 1:4) {
    for (k in 1:5) {
      
      value = data.frame(curedExp[[i]][[j]][[k]]) |> select('exp.pi') |> 
        apply(MARGIN=2, teste_rnUnc) |> apply(MARGIN=2, percent)
      
      rn = c(rn, value)
    }
  }
}

rnUncExp$Value = rn
rnUncExp = rnUncExp |> pivot_wider(names_from='Cens', values_from = "Value")
rnUncExp

# Testes para a suficiência do tempo de observação ----
## Verifica o teste alpha til ----
testeShen = function (x) {
  return(ifelse(x < 0.05, 1, 0))
}

### Weibull
SHENWei = data.frame("Cure" = c(rep(0, 20), rep(0.1, 20), rep(0.3, 20), rep(0.6, 20), rep(0.8, 20)),
                     "Amostra" = c(rep(100, 5), rep(250, 5), rep(500, 5), rep(1000, 5)),
                     "Cens" = c(rep(c(25, 10, 5, 1, 0.1), 5)),
                     "Value" = rep(NA, 100))
shen = c()

for (i in 1:5) {
  for (j in 1:4) {
    for (k in 1:5) {
      
      value = data.frame(curedWei[[i]][[j]][[k]]) |> select(Shen) |> 
        apply(MARGIN=2, testeShen) |> apply(MARGIN=2, percent)

      shen = c(shen, value)
    }
  }
}

SHENWei$Value = shen
SHENWei = SHENWei |> pivot_wider(names_from='Cens', values_from = "Value")
SHENWei

### Exponencial
SHENExp = data.frame("Cure" = c(rep(0, 20), rep(0.1, 20), rep(0.3, 20), rep(0.6, 20), rep(0.8, 20)),
                     "Amostra" = c(rep(100, 5), rep(250, 5), rep(500, 5), rep(1000, 5)),
                     "Cens" = c(rep(c(25, 10, 5, 1, 0.1), 5)),
                     "Value" = rep(NA, 100))
shen = c()

for (i in 1:5) {
  for (j in 1:4) {
    for (k in 1:5) {
      
      value = data.frame(curedExp[[i]][[j]][[k]]) |> select(Shen) |> 
        apply(MARGIN=2, testeShen) |> apply(MARGIN=2, percent)
      
      shen = c(shen, value)
    }
  }
}

SHENExp$Value = shen
SHENExp = SHENExp |> pivot_wider(names_from='Cens', values_from = "Value")
SHENExp


## Verifica o teste qn ----
#### tabelas B paginas 138-142 (255-262) livro Maller e Zhou

# É necessário olhar a coluna pHat do conjunto de dados
# para selecionar a tabela correta

testeQn = function (x, p, b, n) {
  if (p == 1) {
    if (b == 2) {
      corte = ifelse(n == 100,0.13,
                     ifelse(n == 250,0.1,
                            ifelse(n == 500,0.038,0.017)))
    } else if (b == 4) {
      corte = ifelse(n == 100,0.19,
                     ifelse(n == 250,0.165,
                            ifelse(n == 500,0.1,0.037)))
    } else if (b == 6) {
      corte = ifelse(n == 100,0.22,
                     ifelse(n == 250,0.2,
                            ifelse(n == 500,0.174,0.134)))
    } else if (b == 10) {
      corte = ifelse(n == 100,0.24,
                     ifelse(n == 250,0.225,
                            ifelse(n == 500,0.206,0.196)))
    }
  } else if (p == 2) {
    if (b == 2) {
      corte = ifelse(n == 100,0.17,
                     ifelse(n == 250,0.105,
                            ifelse(n == 500,0.034,0.015)))
    } else if (b == 4) {
      corte = ifelse(n == 100,0.26,
                     ifelse(n == 250,0.22,
                            ifelse(n == 500,0.082,0.031)))
    } else if (b == 6) {
      corte = ifelse(n == 100,0.31,
                     ifelse(n == 250,0.28,
                            ifelse(n == 500,0.242,0.104)))
    } else if (b == 10) {
      corte = ifelse(n == 100,0.34,
                     ifelse(n == 250,0.32,
                            ifelse(n == 500,0.298,0.287)))
    }
  } else if (p == 3) {
    if (b == 2) {
      corte = ifelse(n == 100,0.2,
                     ifelse(n == 250,0.1,
                            ifelse(n == 500,0.032,0.015)))
    } else if (b == 4) {
      corte = ifelse(n == 100,0.32,
                     ifelse(n == 250,0.255,
                            ifelse(n == 500,0.072,0.028)))
    } else if (b == 6) {
      corte = ifelse(n == 100,0.39,
                     ifelse(n == 250,0.355,
                            ifelse(n == 500,0.258,0.087)))
    } else if (b == 10) {
      corte = ifelse(n == 100,0.44,
                     ifelse(n == 250,0.41,
                            ifelse(n == 500,0.388,0.375)))
    }
  } else if (p == 4) {
    if (b == 2) {
      corte = ifelse(n == 100,0.21,
                     ifelse(n == 250,0.09,
                            ifelse(n == 500,0.032,0.014)))
    } else if (b == 4) {
      corte = ifelse(n == 100,0.38,
                     ifelse(n == 250,0.24,
                            ifelse(n == 500,0.066,0.026)))
    } else if (b == 6) {
      corte = ifelse(n == 100,0.47,
                     ifelse(n == 250,0.43,
                            ifelse(n == 500,0.226,0.075)))
    } else if (b == 10) {
      corte = ifelse(n == 100,0.53,
                     ifelse(n == 250,0.5,
                            ifelse(n == 500,0.478,0.463)))
    }
  } else if (p == 5) {
    if (b == 2) {
      corte = ifelse(n == 100,0.2,
                     ifelse(n == 250,0.09,
                            ifelse(n == 500,0.03,0.014)))
    } else if (b == 4) {
      corte = ifelse(n == 100,0.44,
                     ifelse(n == 250,0.22,
                            ifelse(n == 500,0.06,0.024)))
    } else if (b == 6) {
      corte = ifelse(n == 100,0.55,
                     ifelse(n == 250,0.505,
                            ifelse(n == 500,0.204,0.069)))
    } else if (b == 10) {
      corte = ifelse(n == 100,0.62,
                     ifelse(n == 250,0.59,
                            ifelse(n == 500,0.566,0.55)))
    }
  } else if (p == 6) {
    if (b == 2) {
      corte = ifelse(n == 100,0.19 ,
                     ifelse(n == 250,0.085,
                            ifelse(n == 500,0.03,0.014)))
    } else if (b == 4) {
      corte = ifelse(n == 100,0.48,
                     ifelse(n == 250,0.2,
                            ifelse(n == 500,0.056,0.023)))
    } else if (b == 6) {
      corte = ifelse(n == 100,0.62,
                     ifelse(n == 250,0.57,
                            ifelse(n == 500,0.178,0.063)))
    } else if (b == 10) {
      corte = ifelse(n == 100,0.7,
                     ifelse(n == 250,0.68,
                            ifelse(n == 500,0.654,0.636)))
    }
  } else if (p == 7) {
    if (b == 2) {
      corte = ifelse(n == 100,0.18,
                     ifelse(n == 250,0.08,
                            ifelse(n == 500,0.028,0.013)))
    } else if (b == 4) {
      corte = ifelse(n == 100,0.45,
                     ifelse(n == 250,0.18,
                            ifelse(n == 500,0.052,0.022)))
    } else if (b == 6) {
      corte = ifelse(n == 100,0.69,
                     ifelse(n == 250,0.62,
                            ifelse(n == 500,0.162,0.057)))
    } else if (b == 10) {
      corte = ifelse(n == 100,0.78,
                     ifelse(n == 250,0.765,
                            ifelse(n == 500,0.74,0.721)))
    }
  } else {
    if (b == 2) {
      corte = ifelse(n == 100,0.16,
                     ifelse(n == 250,0.075,
                            ifelse(n == 500,0.028,0.013)))
    } else if (b == 4) {
      corte = ifelse(n == 100,0.38,
                     ifelse(n == 250,0.155,
                            ifelse(n == 500,0.05,0.02)))
    } else if (b == 6) {
      corte = ifelse(n == 100,0.75,
                     ifelse(n == 250,0.54,
                            ifelse(n == 500,0.144,0.052)))
    } else if (b == 10) {
      corte = ifelse(n == 100,0.86,
                     ifelse(n == 250,0.845,
                            ifelse(n == 500,0.824,0.804)))
    }
  }
  
  return(ifelse(x > corte, 1, 0))
}

### Weibull
QNWei = data.frame("Cure" = c(rep(0, 20), rep(0.1, 20), rep(0.3, 20), rep(0.6, 20), rep(0.8, 20)),
                   "Amostra" = c(rep(100, 5), rep(250, 5), rep(500, 5), rep(1000, 5)),
                   "Cens" = c(rep(c(25, 10, 5, 1, 0.1), 5)),
                   "Value" = rep(NA, 100))

qn = c()

for (i in 1:5) {
  for (j in 1:4) {
    for (k in 1:5) {
      
      p = data.frame(curedWei[[i]][[j]][[k]]) |> 
        filter(teste_pn == 1) |> select('pHat') |> 
        apply(MARGIN = 2, mean, na.rm=T) |> round_any(0.1)
      
      p = ifelse(p %in% c(0.1, 0.2), 1,
                 ifelse(p == 0.3, 2,
                        ifelse(p == 0.4, 3,
                               ifelse(p == 0.5, 4,
                                      ifelse(p == 0.6, 5,
                                             ifelse(p == 0.7, 6,
                                                    ifelse(p == 0.8, 7, 8)))))))
      if (!is.na(p)) {
        if (k %in% c(1,2,3)) {
          value = data.frame(curedWei[[i]][[j]][[k]]) |> #filter(teste_pn == 1) |> 
            select(c('qn','teste_pn')) |> 
            apply(MARGIN=1, testeQn, p=p, b=2, n=sample_size[j]) |> t() |> 
            data.frame() |> select('qn') |> apply(MARGIN=2, percent)
          
        } else {
          value = data.frame(curedWei[[i]][[j]][[k]]) |> #filter(teste_pn == 1) |> 
            select(c('qn','teste_pn')) |> 
            apply(MARGIN=1, testeQn, p=p, b=4, n=sample_size[j]) |> t() |> 
            data.frame() |> select('qn') |> apply(MARGIN=2, percent)
          
        }
      } else {
        value = 0
      }

      qn = c(qn, value)
    }
  }
}

QNWei$Value = qn
QNWei = QNWei |> pivot_wider(names_from='Cens', values_from = "Value")
QNWei

### Exponencial
QNExp = data.frame("Cure" = c(rep(0, 20), rep(0.1, 20), rep(0.3, 20), rep(0.6, 20), rep(0.8, 20)),
                   "Amostra" = c(rep(100, 5), rep(250, 5), rep(500, 5), rep(1000, 5)),
                   "Cens" = c(rep(c(25, 10, 5, 1, 0.1), 5)),
                   "Value" = rep(NA, 100))

qn = c()

for (i in 1:5) {
  for (j in 1:4) {
    for (k in 1:5) {

      p = data.frame(curedExp[[i]][[j]][[k]]) |> 
        filter(teste_pn == 1) |> select('pHat') |> 
        apply(MARGIN = 2, mean, na.rm=T) |> round_any(0.1)
      
      p = ifelse(p %in% c(0.1, 0.2), 1,
                 ifelse(p == 0.3, 2,
                        ifelse(p == 0.4, 3,
                               ifelse(p == 0.5, 4,
                                      ifelse(p == 0.6, 5,
                                             ifelse(p == 0.7, 6,
                                                    ifelse(p == 0.8, 7, 8)))))))
      if (!is.na(p)) {
        if (k == 1) {
          value = data.frame(curedExp[[i]][[j]][[k]]) |> #filter(teste_pn == 1) |> 
            select(c('qn','teste_pn')) |> 
            apply(MARGIN=1, testeQn, p=p, b=2, n=sample_size[j]) |> t() |> 
            data.frame() |> select('qn') |> apply(MARGIN=2, percent)
 
        } else if (k %in% c(2, 3)) {
          value = data.frame(curedExp[[i]][[j]][[k]]) |> #filter(teste_pn == 1) |> 
            select(c('qn','teste_pn')) |> 
            apply(MARGIN=1, testeQn, p=p, b=4, n=sample_size[j]) |> t() |> 
            data.frame() |> select('qn') |> apply(MARGIN=2, percent)
          
        } else if (k == 4) {
          value = data.frame(curedExp[[i]][[j]][[k]]) |> #filter(teste_pn == 1) |> 
            select(c('qn','teste_pn')) |> 
            apply(MARGIN=1, testeQn, p=p, b=6, n=sample_size[j]) |> t() |> 
            data.frame() |> select('qn') |> apply(MARGIN=2, percent)
          
        } else if (k == 5) {
          value = data.frame(curedExp[[i]][[j]][[k]]) |> #filter(teste_pn == 1) |> 
            select(c('qn','teste_pn')) |> 
            apply(MARGIN=1, testeQn, p=p, b=10, n=sample_size[j]) |> t() |> 
            data.frame() |> select('qn') |> apply(MARGIN=2, percent)
        }
      } else {
        value = 0
      }
      
      qn = c(qn, value)
    }
  }
}

QNExp$Value = qn
QNExp = QNExp |> pivot_wider(names_from='Cens', values_from = "Value")
QNExp

## Verifica o teste RECeUS para follow-up ----
teste_rnFU = function(x, y) {
  return(ifelse((x > 0.025) & (y < 0.05), 1, 0))
}

### Weibull
RECeUSWei = data.frame("Cure" = c(rep(0, 20), rep(0.1, 20), rep(0.3, 20), rep(0.6, 20), rep(0.8, 20)),
                       "Amostra" = c(rep(100, 5), rep(250, 5), rep(500, 5), rep(1000, 5)),
                       "Cens" = c(rep(c(25, 10, 5, 1, 0.1), 5)),
                       "Value" = rep(NA, 100))

values = c()

for (i in 1:5) {
  for (j in 1:4) {
    for (k in 1:5) {
      
      tmp = data.frame(curedWei[[i]][[j]][[k]])
     
      value = teste_rnFU(tmp$wei.pi, tmp$wei.ratio)
      value = sum(value) / 1000
      values = c(values, value)
    }
  }
}

RECeUSWei$Value = values
RECeUSWei = RECeUSWei |> pivot_wider(names_from='Cens', values_from = "Value")
RECeUSWei

### Exponencial
RECeUSExp = data.frame("Cure" = c(rep(0, 20), rep(0.1, 20), rep(0.3, 20), rep(0.6, 20), rep(0.8, 20)),
                       "Amostra" = c(rep(100, 5), rep(250, 5), rep(500, 5), rep(1000, 5)),
                       "Cens" = c(rep(c(25, 10, 5, 1, 0.1), 5)),
                       "Value" = rep(NA, 100))

values = c()

for (i in 1:5) {
  for (j in 1:4) {
    for (k in 1:5) {
      
      tmp = data.frame(curedExp[[i]][[j]][[k]])
      
      value = teste_rnFU(tmp$exp.pi, tmp$exp.ratio)
      value = sum(value) / 1000
      values = c(values, value)
    }
  }
}

RECeUSExp$Value = values
RECeUSExp = RECeUSExp |> pivot_wider(names_from='Cens', values_from = "Value")
RECeUSExp

# Continua ----
source("D:/Estudos/TCC/Geração de gráficos.R")
source("D:/Estudos/TCC/AnaliseDadosReais.R")
