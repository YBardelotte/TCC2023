require(tidyverse)
require(gridExtra)
require(egg)
require(grid)
require(ggpubr)
require(cowplot)
require(latex2exp)

options(scipen = 99)

grid_arrange_shared_legend <- function(..., xaxis.title, ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(
    position,
    "bottom" = arrangeGrob(
      do.call(arrangeGrob, gl),
      legend,
      ncol = 1,
      left = xaxis.title,
      heights = unit.c(unit(1, "npc") - lheight, lheight)),
    
    "right" = arrangeGrob(
      do.call(arrangeGrob, gl),
      legend,
      ncol = 2,
      left = xaxis.title,
      widths = unit.c(unit(1, "npc") - lwidth, lwidth))
  )
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}

# Proporção de Imunes RV ----
### Graficos de conclusão incorreta Exponencial
p1 <- RVExp |> filter(Cure == 0, Amostra == 100) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "RV") |> 
  ggplot() +
  geom_col(aes(x = factor(Data, levels = c(25, 10, 5, 1, 0.1)), y = as.numeric(RV), 
               fill=factor(Data, levels = c(25, 10, 5, 1, 0.1))),
           position='dodge') +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 100", y = NULL, fill = "% Censurados") +
  ggtitle("Razão de Verossimilhanças") +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

p2 <- RVExp |> filter(Cure == 0, Amostra == 1000) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "RV") |> 
  ggplot() +
  geom_col(aes(x = factor(Data, levels = c(25, 10, 5, 1, 0.1)), y = as.numeric(RV), 
               fill=factor(Data, levels = c(25, 10, 5, 1, 0.1))),
           position='dodge') +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 1000", y = NULL, fill = "% Censurados") +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

### Graficos de conclusão incorreta Weibull
p3 <- RVWei |> filter(Cure == 0, Amostra == 100) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "RV") |> 
  ggplot() +
  geom_col(aes(x = factor(Data, levels = c(25, 10, 5, 1, 0.1)), y = as.numeric(RV), 
               fill=factor(Data, levels = c(25, 10, 5, 1, 0.1))),
           position='dodge') +
  ylim(0, 0.07) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 100", y = NULL, fill = "% Censurados") +
  ggtitle("Razão de Verossimilhanças") +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

p4 <- RVWei |> filter(Cure == 0, Amostra == 1000) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "RV") |> 
  ggplot() +
  geom_col(aes(x = factor(Data, levels = c(25, 10, 5, 1, 0.1)), y = as.numeric(RV), 
               fill=factor(Data, levels = c(25, 10, 5, 1, 0.1))),
           position='dodge') +
  ylim(0, 0.07) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 1000", y = NULL, fill = "% Censurados") +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

# Proporção de Imunes Pn ----
### Graficos de conclusão incorreta Exponencial
p5 <- PNExp |> filter(Cure == 0, Amostra == 100) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "PN") |> 
  ggplot() +
  geom_col(aes(x = factor(Data, levels = c(25, 10, 5, 1, 0.1)), y = as.numeric(PN), 
               fill=factor(Data, levels = c(25, 10, 5, 1, 0.1))),
           position='dodge') +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 100", y = NULL, fill = "% Censurados") +
  ggtitle(TeX("Teste $\\hat{p}_n$")) +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

p6 <- PNExp |> filter(Cure == 0, Amostra == 1000) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "PN") |> 
  ggplot() +
  geom_col(aes(x = factor(Data, levels = c(25, 10, 5, 1, 0.1)), y = as.numeric(PN), 
               fill=factor(Data, levels = c(25, 10, 5, 1, 0.1))),
           position='dodge') +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 1000", y = NULL, fill = "% Censurados") +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

### Graficos de conclusão incorreta Weibull
p7 <- PNWei |> filter(Cure == 0, Amostra == 100) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "PN") |> 
  ggplot() +
  geom_col(aes(x = factor(Data, levels = c(25, 10, 5, 1, 0.1)), y = as.numeric(PN), 
               fill=factor(Data, levels = c(25, 10, 5, 1, 0.1))),
           position='dodge') +
  ylim(0, 1) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 100", y = NULL, fill = "% Censurados") +
  ggtitle(TeX("Teste $\\hat{p}_n$")) +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

p8 <- PNWei |> filter(Cure == 0, Amostra == 1000) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "PN") |> 
  ggplot() +
  geom_col(aes(x = factor(Data, levels = c(25, 10, 5, 1, 0.1)), y = as.numeric(PN), 
               fill=factor(Data, levels = c(25, 10, 5, 1, 0.1))),
           position='dodge') +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 1000", y = NULL, fill = "% Censurados") +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

# Proporção de Imunes RECeUS ----
### Graficos de conclusão incorreta Exponencial
p9 <- rnUncExp |> filter(Cure == 0, Amostra == 100) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "RECeUS−AIC") |> 
  ggplot() +
  geom_col(aes(x = factor(Data, levels = c(25, 10, 5, 1, 0.1)), y = as.numeric(`RECeUS−AIC`), 
               fill=factor(Data, levels = c(25, 10, 5, 1, 0.1))),
           position='dodge') +
  ylim(0, 0.6) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 100", y = NULL, fill = "% Censurados") +
  ggtitle(TeX("Teste RECeUS")) +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

p10 <- rnUncExp |> filter(Cure == 0, Amostra == 1000) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "RECeUS−AIC") |> 
  ggplot() +
  geom_col(aes(x = factor(Data, levels = c(25, 10, 5, 1, 0.1)), y = as.numeric(`RECeUS−AIC`), 
               fill=factor(Data, levels = c(25, 10, 5, 1, 0.1))),
           position='dodge') +
  ylim(0, 0.6) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 1000", y = NULL, fill = "% Censurados") +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

### Graficos de conclusão incorreta Weibull
p11 <- rnUncWei |> filter(Cure == 0, Amostra == 100) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "RECeUS−AIC") |> 
  ggplot() +
  geom_col(aes(x = factor(Data, levels = c(25, 10, 5, 1, 0.1)), y = as.numeric(`RECeUS−AIC`), 
               fill=factor(Data, levels = c(25, 10, 5, 1, 0.1))),
           position='dodge') +
  ylim(0, 0.6) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 100", y = NULL, fill = "% Censurados") +
  ggtitle(TeX("Teste RECeUS")) +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

p12 <- rnUncWei |> filter(Cure == 0, Amostra == 1000) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "RECeUS−AIC") |> 
  ggplot() +
  geom_col(aes(x = factor(Data, levels = c(25, 10, 5, 1, 0.1)), y = as.numeric(`RECeUS−AIC`), 
               fill=factor(Data, levels = c(25, 10, 5, 1, 0.1))),
           position='dodge') +
  ylim(0, 0.6) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 1000", y = NULL, fill = "% Censurados") +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

png('Imunes_Exp.png', height = 500, width = 800)
grid_arrange_shared_legend(p1, p2, p5, p6, p9, p10, ncol=2, nrow = 3, 
                           xaxis.title=textGrob('Proporção de modelos (incorretamente) apropriados', 
                                                rot = 90, gp = gpar(fontsize = 15)))
dev.off()

png('Imunes_Wei.png', height = 500, width = 800)
grid_arrange_shared_legend(p3, p4, p7, p8, p11, p12, ncol=2, nrow = 3,
                           xaxis.title=textGrob('Proporção de modelos (incorretamente) apropriados', 
                                                rot = 90, gp = gpar(fontsize = 15)))

dev.off()

# Tempo de observação Qn ----
### Graficos de conclusão correta Exponencial
p1 <- QNExp |> filter(Cure != 0, Amostra == 100) |> select(Cure, Amostra, `0.1`) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "QN") |> 
  ggplot() +
  geom_col(aes(x = factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8)), y = as.numeric(QN), 
               fill=factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8))),
           position='dodge') +
  ylim(0, 1) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 100", y = NULL, fill = "% Curados") +
  ggtitle(TeX("Teste $q_n$")) +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

p2 <- QNExp |> filter(Cure != 0, Amostra == 1000) |> select(Cure, Amostra, `0.1`) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "QN") |> 
  ggplot() +
  geom_col(aes(x = factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8)), y = as.numeric(QN), 
               fill=factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8))),
           position='dodge') +
  ylim(0, 1) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 1000", y = NULL, fill = "% Curados") +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

### Graficos de conclusão correta Weibull
p3 <- QNWei |> filter(Cure != 0, Amostra == 100) |> select(Cure, Amostra, `0.1`) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "QN") |> 
  ggplot() +
  geom_col(aes(x = factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8)), y = as.numeric(QN), 
               fill=factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8))),
           position='dodge') +
  ylim(0, 1) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 100", y = NULL, fill = "% Curados") +
  ggtitle(TeX("Teste $q_n$")) +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

p4 <- QNWei |> filter(Cure != 0, Amostra == 1000) |> select(Cure, Amostra, `0.1`) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "QN") |> 
  ggplot() +
  geom_col(aes(x = factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8)), y = as.numeric(QN), 
               fill=factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8))),
           position='dodge') +
  ylim(0, 1) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 1000", y = NULL, fill = "% Curados") +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

# Tempo de observação ã ----
### Graficos de conclusão correta Exponencial
p5 <- SHENExp |> filter(Cure != 0, Amostra == 100) |> select(Cure, Amostra, `0.1`) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "SHEN") |> 
  ggplot() +
  geom_col(aes(x = factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8)), y = as.numeric(SHEN), 
               fill=factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8))),
           position='dodge') +
  ylim(0, 0.5) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 100", y = NULL, fill = "% Curados") +
  ggtitle(TeX("Teste $\\tilde{\\alpha}$")) +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

p6 <- SHENExp |> filter(Cure != 0, Amostra == 1000) |> select(Cure, Amostra, `0.1`) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "SHEN") |> 
  ggplot() +
  geom_col(aes(x = factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8)), y = as.numeric(SHEN), 
               fill=factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8))),
           position='dodge') +
  ylim(0, 0.5) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 1000", y = NULL, fill = "% Curados") +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

### Graficos de conclusão correta Weibull
p7 <- SHENWei |> filter(Cure != 0, Amostra == 100) |> select(Cure, Amostra, `0.1`) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "SHEN") |> 
  ggplot() +
  geom_col(aes(x = factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8)), y = as.numeric(SHEN), 
               fill=factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8))),
           position='dodge') +
  ylim(0, 0.5) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 100", y = NULL, fill = "% Curados") +
  ggtitle(TeX("Teste $\\tilde{\\alpha}$")) +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

p8 <- SHENWei |> filter(Cure != 0, Amostra == 1000) |> select(Cure, Amostra, `0.1`) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "SHEN") |> 
  ggplot() +
  geom_col(aes(x = factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8)), y = as.numeric(SHEN), 
               fill=factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8))),
           position='dodge') +
  ylim(0, 0.5) +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 1000", y = NULL, fill = "% Curados") +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

# Tempo de observação RECeUS ----
### Graficos de conclusão correta Exponencial
p9 <- RECeUSExp |> filter(Cure != 0, Amostra == 100) |> select(Cure, Amostra, `0.1`) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "RECeUS−AIC") |> 
  ggplot() +
  geom_col(aes(x = factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8)), y = as.numeric(`RECeUS−AIC`), 
               fill=factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8))),
           position='dodge') +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 100", y = NULL, fill = "% Curados") +
  ggtitle(TeX("Teste RECeUS")) +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

p10 <- RECeUSExp |> filter(Cure != 0, Amostra == 1000) |> select(Cure, Amostra, `0.1`) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "RECeUS−AIC") |> 
  ggplot() +
  geom_col(aes(x = factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8)), y = as.numeric(`RECeUS−AIC`), 
               fill=factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8))),
           position='dodge') +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 1000", y = NULL, fill = "% Curados") +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

### Graficos de conclusão correta Weibull
p11 <- RECeUSWei |> filter(Cure != 0, Amostra == 100) |> select(Cure, Amostra, `0.1`) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "RECeUS−AIC") |> 
  ggplot() +
  geom_col(aes(x = factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8)), y = as.numeric(`RECeUS−AIC`), 
               fill=factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8))),
           position='dodge') +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 100", y = NULL, fill = "% Curados") +
  ggtitle(TeX("Teste RECeUS")) +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

p12 <- RECeUSWei |> filter(Cure != 0, Amostra == 1000) |> select(Cure, Amostra, `0.1`) |> 
  pivot_longer(-c('Cure','Amostra'), names_to = "Data", values_to = "RECeUS−AIC") |> 
  ggplot() +
  geom_col(aes(x = factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8)), y = as.numeric(`RECeUS−AIC`), 
               fill=factor(Cure, levels = c(0.1, 0.3, 0.6, 0.8))),
           position='dodge') +
  scale_fill_manual(values = c('#5C5958',"#8B8786",'#B6B3B2','#D4D3D2','#EDECEC')) +
  labs(x = "n = 1000", y = NULL, fill = "% Curados") +
  theme_bw() +
  theme(text = element_text(size=15), legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

png('Tempo_Exp.png', height = 500, width = 800)
grid_arrange_shared_legend(p1, p2, p5, p6, p9, p10, ncol=2, nrow = 3, 
                           xaxis.title=textGrob('Proporção de modelos apropriados (follow-up)', 
                                                rot = 90, gp = gpar(fontsize = 15)))
dev.off()

png('Tempo_Wei.png', height = 500, width = 800)
grid_arrange_shared_legend(p3, p4, p7, p8, p11, p12, ncol=2, nrow = 3,
                           xaxis.title=textGrob('Proporção de modelos apropriados (follow-up)', 
                                                rot = 90, gp = gpar(fontsize = 15)))

dev.off()
