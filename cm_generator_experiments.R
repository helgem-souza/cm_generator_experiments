library(pacman)

p_load(tidyverse, clusterGeneration, MASS, gencor, mvtnorm, ggpubr, gencor, janitor, ggridges, data.table)
setwd("G:\\Meu Drive\\Estatistica\\Doutorado\\Tese\\Testes e Simulações\\Aplicacao e Comparacoes\\graficos")

#Fun??o para remover valores iguais a 1
removone <- function(x) x[-which(x == 1)]

set.seed(1234)

#Experimentos

##Alguns exemplos de matrizes geradas ---

mlow <- numeric()
mmedium <- numeric()
mhigh <- numeric()
mrandom <- numeric()
mcustom504010 <- numeric()
mcustom404020 <- numeric()
mcustom105040 <- numeric()
mcustom302050 <- numeric()
mcustom108010 <- numeric()
mcustom008020 <- numeric()
mcustom500050 <- numeric()
mcustom703000 <- numeric()

prop_custom504010 <- numeric()
prop_custom404020 <- numeric()
prop_custom105040 <- numeric()
prop_custom302050 <- numeric()
prop_custom108010 <- numeric()
prop_custom008020 <- numeric()
prop_custom500050 <- numeric()
prop_custom703000 <- numeric()

tempos <- numeric()


for(i in 1:1000){
  
  glow <- gencor(20, "low")
  mlow <- c(mlow, glow$Matrix)
  
  gmedium <- gencor(20, "medium")
  mmedium <- c(mmedium, gmedium$Matrix)
  
  ghigh <- gencor(20, "high")
  mhigh <- c(mhigh, ghigh$Matrix)
  
  #mrandom <- c(mrandom, gencor(20, "random")$Matrix)
  
  g504010 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.5, .4, .1))
  mcustom504010 <- c(mcustom504010, g504010$Matrix)
  prop_custom504010 <- rbind(prop_custom504010, g504010$Proportions)
  
  g404020 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.4, .4, .2))
  mcustom404020 <- c(mcustom404020, g404020$Matrix)
  prop_custom404020 <- rbind(prop_custom404020, g404020$Proportions)
  
  g105040 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.1, .5, .4))
  mcustom105040 <- c(mcustom105040, g105040$Matrix)
  prop_custom105040 <- rbind(prop_custom105040, g105040$Proportions)
  
  g302050 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.3, .2, .5))
  mcustom302050 <- c(mcustom302050, g302050$Matrix)
  prop_custom302050 <- rbind(prop_custom302050, g302050$Proportions)
  
  g108010 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.1, .8, .1))
  mcustom108010 <- c(mcustom108010, g108010$Matrix)
  prop_custom108010 <- rbind(prop_custom108010, g108010$Proportions)
  
  g008020 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.0, .8, .2))
  mcustom008020 <- c(mcustom008020, g008020$Matrix)
  prop_custom008020 <- rbind(prop_custom008020, g008020$Proportions)
  
  g500050 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.5, .0, .5))
  mcustom500050 <- c(mcustom500050, g500050$Matrix)
  prop_custom500050 <- rbind(prop_custom500050, g500050$Proportions)
  
  g703000 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.7, .3, .0))
  mcustom703000 <- c(mcustom703000, g703000$Matrix)
  prop_custom703000 <- rbind(prop_custom703000, g703000$Proportions)
  
  tempos <- rbind(tempos,data.frame("Cenario" = c("Low", 
                                                  "Medium", 
                                                  "High", 
                                                  "Custom 504010",
                                                  "Custom 404020",
                                                  "Custom 105040",
                                                  "Custom 302050",
                                                  "Custom 108010",
                                                  "Custom 008020",
                                                  "Custom 500050",
                                                  "Custom 703000"),
                                    tempo = c(glow$Runtime,
                                              gmedium$Runtime,
                                              ghigh$Runtime,
                                              g504010$Runtime,
                                              g404020$Runtime,
                                              g105040$Runtime,
                                              g302050$Runtime,
                                              g108010$Runtime, 
                                              g008020$Runtime,
                                              g500050$Runtime,
                                              g703000$Runtime)))
  
  cat("\014")
  print(i)
}

#Tempos m?dios e desvio padr?o por m?todo

sink("Tempos_simulacao_custom.txt")
L <- lapply(split(tempos$tempo, tempos$Cenario), function(x)return(c(min(x), mean(x), sd(x), max(x))))
tempos2 <- Reduce(rbind, L) %>% data.frame
rownames(tempos2) <- names(L)
names(tempos2) <- c("Min", "Mean", "Sd", "Max")
tempos2
sink()

#Propor??o M?dia simulada
prop_med <- rbind(colMeans(prop_custom504010),
                  colMeans(prop_custom404020),
                  colMeans(prop_custom105040),
                  colMeans(prop_custom302050),
                  colMeans(prop_custom108010),
                  colMeans(prop_custom008020),
                  colMeans(prop_custom500050),
                  colMeans(prop_custom703000)) %>% as.data.frame()

rownames(prop_med) <- c("Custom 504010",
                        "Custom 404020",
                        "Custom 105040",
                        "Custom 302050",
                        "Custom 108010",
                        "Custom 008020",
                        "Custom 500050",
                        "Custom 703000")
names(prop_med) <- c("Obs_low", "Obs_medium", "Obs_high")

esperado <- data.frame(Exp_low = c(0.5, 0.4, .1, .3, .1, 0, .5, .7), Exp_medium = c(.4,.4,.5,.2,.8,.8,0,.3), Exp_high = c(.1,.2,.4,.5,.1,.2,.5,0))

prop_med <- data.frame(prop_med, esperado, abs(prop_med - esperado))
names(prop_med)[7:9] <- c("Dif_low", "Dif_medium", "Dif_high")


postscript("basic_histogram.eps", width = 30, height = 10, paper = "special", pointsize = 25, horizontal = T)

par(mfrow = c(1, 3), mai = c(1.5, 1.5, 0.2, 0.2), oma = c(1, 1, 1, 1))

mlow %>% removone() %>% hist(main = "", xlim = c(-1, 1), col = "gray", xlab = expression(rho), ylab = "Frequ?ncia")
legend("topright", "(a)", bty = "n", cex = 1.5)
mmedium %>% removone() %>% hist(main = "", xlim = c(-1, 1), col = "gray", xlab = expression(rho), ylab = "Frequ?ncia")
legend("topright", "(b)", bty = "n", cex = 1.5)
mhigh %>% removone() %>% hist(breaks = 32, main = "", xlim = c(-1, 1), col = "gray", xlab = expression(rho), ylab = "Frequ?ncia")
legend("topright", "(c)", bty = "n", cex = 1.5)
#mrandom %>% removone() %>% hist(main = "", xlim = c(-1, 1), col = "gray", xlab = expression(rho))
#legend("topright", "(d)", bty = "n", cex = 1.5)

dev.off()

postscript("custom_histogram.eps", width = 30, height = 16, paper = "special", pointsize = 25)

par(mfrow = c(2, 4), mai = c(1.5, 1.5, 0.2, 0.2), oma = c(2, 1, 1, 1))

mcustom504010  %>% removone() %>% hist(breaks = 20, main = "", xlim = c(-1, 1), col = "gray", xlab = expression(rho), ylab = "Frequ?ncia");abline(v = c(.3, .6, -.3, -.6), lty = c(2,3,2,3), lwd = 2)
legend("topright", "(a)", bty = "n", cex = 1.5)
mcustom404020  %>% removone() %>% hist(breaks = 20, main = "", xlim = c(-1, 1), col = "gray", xlab = expression(rho), ylab = "Frequ?ncia");abline(v = c(.3, .6, -.3, -.6), lty = c(2,3,2,3), lwd = 2)
legend("topright", "(b)", bty = "n", cex = 1.5)
mcustom105040  %>% removone() %>% hist(breaks = 20, main = "", xlim = c(-1, 1), col = "gray", xlab = expression(rho), ylab = "Frequ?ncia");abline(v = c(.3, .6, -.3, -.6), lty = c(2,3,2,3), lwd = 2)
legend("topright", "(c)", bty = "n", cex = 1.5)
mcustom302050  %>% removone() %>% hist(breaks = 20, main = "", xlim = c(-1, 1), col = "gray", xlab = expression(rho), ylab = "Frequ?ncia");abline(v = c(.3, .6, -.3, -.6), lty = c(2,3,2,3), lwd = 2)
legend("topright", "(d)", bty = "n", cex = 1.5)
mcustom108010  %>% removone() %>% hist(breaks = 20, main = "", xlim = c(-1, 1), col = "gray", xlab = expression(rho), ylab = "Frequ?ncia");abline(v = c(.3, .6, -.3, -.6), lty = c(2,3,2,3), lwd = 2)
legend("topright", "(e)", bty = "n", cex = 1.5)
mcustom008020  %>% removone() %>% hist(breaks = 20, main = "", xlim = c(-1, 1), col = "gray", xlab = expression(rho), ylab = "Frequ?ncia");abline(v = c(.3, .6, -.3, -.6), lty = c(2,3,2,3), lwd = 2)
legend("topright", "(f)", bty = "n", cex = 1.5)
mcustom500050  %>% removone() %>% hist(breaks = 20, main = "", xlim = c(-1, 1), col = "gray", xlab = expression(rho), ylab = "Frequ?ncia");abline(v = c(.3, .6, -.3, -.6), lty = c(2,3,2,3), lwd = 2)
legend("topright", "(g)", bty = "n", cex = 1.5)
mcustom703000  %>% removone() %>% hist(breaks = 18, main = "", xlim = c(-1, 1), col = "gray", xlab = expression(rho), ylab = "Frequ?ncia");abline(v = c(.3, .6, -.3, -.6), lty = c(2,3,2,3), lwd = 2)
legend("topright", "(h)", bty = "n", cex = 1.5)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
#plot(1, type = "n", axes=FALSE, xlab="", ylab="", asp = .1)
legend("bottom", legend = c("|0.3|", "|0.6|"), lty = c(2, 3), lwd = c(2,2), horiz = T, cex = 1.5, xpd = T, bty = "n")

dev.off()

#Figuras: Matrizes de dimens?o 3; 4; 5; 10; 20; 40; 50; 100; 200
#par(mfrow = c(3,5))

#for(k in c(0.001, 0.005, .01, .015, .02, .025, .03, .035)){

postscript("liminf_density.eps", width = 30, height = 30)

for(k in c(0.001, 0.005, 0.01, 0.015, 0.02, 0.025)){
  
  correlations <- numeric()
  #for(i in c(4, 5, 10, 20, 40, 50, 100, 200)){
  for(i in 1:1000){
    
    
    
    
    correlations <- c(correlations, gencor(30, random_liminf = k)$Matrix %>% removone())
    
  }
  
  #hist(correlations, col = "gray", main = paste("Dimension =", i, "liminf =", k), freq = F)
  #hist(correlations, col = "gray", main = paste("liminf =", k), freq = F, xlim = c(-.3, .3))
  
  if(k == 0.001) plot(density(correlations, bw = .15), lwd = 2, xlim = c(-1, 1), xlab = expression(rho), ylab = "Densidade", main = "")
  if(k != 0.001) lines(density(correlations, bw = .15), lwd = 2, lty = which(k == c(0.001, 0.005, 0.01, 0.015, 0.02, 0.025)))
  
  
  #HIstograma modificado
  # y <- hist(correlations, col = alpha("gray", 0.3), border = "white", xlim = c(-1, 1), xlab = expression(rho), ylab = "Densidade", main = "")
  # lines(y$breaks, c(y$counts, 0), type = "s")
  
  
}

legend("topright", legend = c(0.001, 0.005, 0.01, 0.015, 0.02, 0.025) %>% as.character(), lty= 1:8, lwd = 2)

dev.off()




x <- numeric()
for(i in 1:100){
  x <- c(x, gencor2(20, "custom", custom_lim = c(.05, .24, .49, .7), custom_prop = c(.04,.15,.19,.14,.48))$Matrix %>% removone)
}
hist(x, col = "gray")


#Compara??o - C-Vine, Onion e Gencor - diversos random_liminf

desc <- function(x)return(c(min(x), mean(x), sd(x), max(x)))

set.seed(1234)

random_results <- data.frame()
time_onion <- numeric()
time_cvine <- numeric()
time_gencor <- numeric()

nsim <- 1000

time_results <- data.frame()

#par(mai = c(0.1,0.1,0.1,0.1))

#for(li in c(0.005, 0.025)){
#for(li in c(0.005, 0.01, 0.015, 0.02, 0.025)){
for(li in c(0.025)){
  
  
  
  #pdf(paste("Liminf_s_suav", li, ".pdf", sep = ""), width = 30, height = 20, pointsize = 25)
  postscript(paste("Liminf_", li, ".eps", sep = ""), width = 30, height = 16)
  layout(matrix(c(1:12, 13, 13, 13, 13), nc=4, nr = 4, byrow = T))
  par(mfrow = c(3, 4), mai = c(.5, .5,0.2,0.2), oma = c(4, 1, 1, 1))
  
  #for(dim in c(3:10, 20, 50, 100, 200)){
  for(dim in 100){
    dados_comp <- NULL
    
    cor_cvine <- numeric()
    cor_onion <- numeric()
    cor_uncor <- numeric()
    cor_gencor <- numeric()
    rt_cvine <- numeric()
    rt_onion <- numeric()
    rt_gencor <- numeric()
    
    for(i in 1:nsim){
      
      #Onion
      t1 <- Sys.time()
      cor_onion <- c(cor_onion, genPositiveDefMat(dim, "onion")$Sigma %>% cov2cor)
      t2 <- Sys.time()
      rt_onion <- c(rt_onion, as.numeric(difftime(t2, t1, units = "secs")))
      
      #C-vine
      t1 <- Sys.time()
      cor_cvine <- c(cor_cvine, genPositiveDefMat(dim, "c-vine")$Sigma %>% cov2cor)
      t2 <- Sys.time()
      rt_cvine <- c(rt_cvine, as.numeric(difftime(t2, t1, units = "secs")))
      
      #Uncorrelated
      p <- 10
      n <- dim
      temp <- MASS::mvrnorm(n, rep(0,p), diag(p)) %>% t() %>%
        cor() 
      cor_uncor <- c(cor_uncor, temp)
      
      #Gencor
      x <- gencor(dim, random_liminf = li)
      cor_gencor <- c(cor_gencor, x$Matrix )
      rt_gencor <- c(rt_gencor, x$Runtime)
      cat("\014")
      print(paste(i, dim, li))
      
      
      
    }
    
    time_onion <- rbind(time_onion, c(dim, desc(rt_onion)))
    time_cvine <- rbind(time_cvine, c(dim, desc(rt_cvine)))
    time_gencor <- rbind(time_gencor, c(li, dim, desc(rt_gencor)))
    
    #Dados Onion
    #data_onion[[list_index]] <- list(Correlation = cor_onion, Runtime = rt_onion)
    #names(data_onion[[list_index]]) <- c("Correlation", "Runtime")
    
    #Dados C-vine
    #data_cvine[[list_index]] <- list(Correlation = cor_cvine, Runtime = rt_cvine)
    #names(data_cvine[[list_index]]) <- c("Correlation", "Runtime")
    
    #Dados #Gencor
    #data_gencor[[list_index]] <- list(Correlation = cor_gencor, Runtime = rt_gencor)
    #names(data_gencor[[list_index]]) <- c("Correlation", "Runtime")
    
    #list_index <- list_index + 1
    
    nrep <- length(cor_cvine %>% removone)
    
    dados_comp <- data.frame(dim = rep(dim, 4*nrep),
                             li = rep(li, 4*nrep),
                             cor = c(cor_cvine %>% removone,
                                     cor_onion %>% removone,
                                     cor_uncor %>% removone,
                                     cor_gencor %>% removone),
                             method = c(rep("cvine", nrep),
                                        rep("onion", nrep),
                                        rep("uncor", nrep),
                                        rep("gencor", nrep))) %>% 
      mutate(method = factor(method, levels = c("cvine", "onion", "uncor", "gencor"))) %>% 
      data.table
    
    #Gera os gráficos de borda
    if(dim %in% c(3, 7, 20, 100)){
      
      #Histograma
      dados_comp %>% ggplot(aes(x = cor, y = method, group =method)) + 
        geom_density_ridges(stat = 'binline', bins = 50, alpha = 0.5) +
        annotate("text", x = -.8, y = 4.8, label = paste0("Dim = ", dim), size = 7) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(color = "black", fill = "white"),
              axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 25, angle = -60),
              #axis.text.y = element_blank(),
              axis.title.x = element_blank()) +
        labs(y = " ")+
        scale_y_discrete(expand = expansion(add = c(.1, 2))) + 
        xlim(-1.2,1.2)
      
      ggsave(paste0("hist_dim_", dim,"_lim_", li,".pdf"),
             width = 6, height = 6)
      
      #Densidade
      dados_comp %>% ggplot(aes(x = cor, y = method, group =method)) + 
        geom_density_ridges(alpha = 0.5) +
        annotate("text", x = -.8, y = 4.8, label = paste0("Dim = ", dim), size = 7) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(color = "black", fill = "white"),
              axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 25, angle = -60),
              #axis.text.y = element_blank(),
              axis.title.x = element_blank()) +
        labs(y = " ")+
        scale_y_discrete(expand = expansion(add = c(.1, 2))) + 
        xlim(-1.2,1.2)
      
      ggsave(paste0("dens_dim_", dim,"_lim_", li,".pdf"),
             width = 6, height = 6)
      
      #Densidade suavizada
      dados_comp %>% ggplot(aes(x = cor, y = method, group =method)) + 
        geom_density_ridges(alpha = 0.5, bandwidth = 0.15) +
        annotate("text", x = -.8, y = 4.8, label = paste0("Dim = ", dim), size = 7) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(color = "black", fill = "white"),
              axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 25, angle = -60),
              #axis.text.y = element_blank(),
              axis.title.x = element_blank()) +
        labs(y = " ")+
        scale_y_discrete(expand = expansion(add = c(.1, 2))) + 
        xlim(-1.2,1.2)
      
      ggsave(paste0("dens_suav_dim_", dim,"_lim_", li,".pdf"),
             width = 6, height = 6)
      
      #Gráficos de centro
    }else{
      
      #Histograma
      dados_comp %>% ggplot(aes(x = cor, y = method, group =method)) + 
        geom_density_ridges(stat = 'binline', bins = 50, alpha = 0.5) +
        annotate("text", x = -.8, y = 4.8, label = paste0("Dim = ", dim), size = 7) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(color = "black", fill = "white"),
              axis.text.x = element_text(size = 20),
              #axis.text.y = element_text(size = 25, angle = -60),
              axis.text.y = element_blank(),
              axis.title.x = element_blank()) +
        labs(y = " ")+
        scale_y_discrete(expand = expansion(add = c(.1, 2))) + 
        xlim(-1.2,1.2)
      
      ggsave(paste0("hist_dim_", dim,"_lim_", li,".pdf"),
             width = 5.1, height = 6)
      
      #Densidade
      dados_comp %>% ggplot(aes(x = cor, y = method, group =method)) + 
        geom_density_ridges(alpha = 0.5) +
        annotate("text", x = -.8, y = 4.8, label = paste0("Dim = ", dim), size = 7) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(color = "black", fill = "white"),
              axis.text.x = element_text(size = 20),
              #axis.text.y = element_text(size = 25, angle = -60),
              axis.text.y = element_blank(),
              axis.title.x = element_blank()) +
        labs(y = " ")+
        scale_y_discrete(expand = expansion(add = c(.1, 2))) + 
        xlim(-1.2,1.2)
      
      ggsave(paste0("dens_dim_", dim,"_lim_", li,".pdf"),
             width = 5.1, height = 6)
      
      #Densidade suavizada
      dados_comp %>% ggplot(aes(x = cor, y = method, group =method)) + 
        geom_density_ridges(alpha = 0.5, bandwidth = 0.15) +
        annotate("text", x = -.8, y = 4.8, label = paste0("Dim = ", dim), size = 7) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(color = "black", fill = "white"),
              axis.text.x = element_text(size = 20),
              #axis.text.y = element_text(size = 25, angle = -60),
              axis.text.y = element_blank(),
              axis.title.x = element_blank()) +
        labs(y = " ")+
        scale_y_discrete(expand = expansion(add = c(.1, 2))) + 
        xlim(-1.2,1.2)
      
      ggsave(paste0("dens_suav_dim_", dim,"_lim_", li,".pdf"),
             width = 5.1, height = 6)
      
    }
    
    
    #rm(cor_cvine, cor_gencor, cor_onion, cor_uncor)
    
    # write.csv(dados_comp, paste0("comp_cor_li", li, ".csv"))
    
    #Suavizado
    #dcvine <- density(cor_cvine %>% removone,  bw = .15)
    #donion <- density(cor_onion %>% removone,  bw = .15)
    #duncor <- density(cor_uncor %>% removone,  bw = .15)
    #dgencor <- density(cor_gencor %>% removone,  bw = .15)
    
    # #Sem suavização
    dcvine <- density((cor_cvine %>% removone))
    donion <- density((cor_onion %>% removone))
    duncor <- density((cor_uncor %>% removone))
    dgencor <- density((cor_gencor %>% removone))
    
    # #Em cores
    # plot(dcvine, lty = 1, col = "red", lwd = 3, xlim = c(-1, 1), ylim = c(0, max(dcvine$y, donion$y, dgencor$y, duncor$y)), xlab = expression(rho), ylab = "Densidade", main = "")
    # lines(donion, lty = 1, lwd = 3, col = "blue")
    # lines(dgencor, lty = 1, lwd = 3, col = "orange")
    # lines(duncor, lty = 1, lwd = 3, col = "green")
    # text(x = -.8, y = .9*max(dcvine$y, donion$y, dgencor$y, duncor$y), labels = paste("Dim:",dim), cex = .8)
    
    #Em linhas
    plot(dcvine, lty = 1, lwd = 2, xlim = c(-1, 1), ylim = c(0, max(dcvine$y, donion$y, dgencor$y, duncor$y)), xlab = expression(rho), ylab = "Densidade", main = "")
    lines(donion, lty = 2, lwd = 2)
    lines(dgencor, lty = 3, lwd = 2)
    lines(duncor, lty = 4, lwd = 2)
    text(x = -.8, y = .9*max(dcvine$y, donion$y, dgencor$y, duncor$y), labels = paste("Dim:",dim), cex = .8)
    
    
  }
  
  #Em cores
  # par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  # plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  # #plot(1, type = "n", axes=FALSE, xlab="", ylab="", asp = .1)
  # legend("bottom", legend = c("C-vine", "Onion", "CM-generator", "Uncorrelated"),
  #        lty = c(1, 1, 1, 1), lwd = c(2,2,2,2), horiz = T, cex = 1.5, xpd = T,
  #        col = c("red", "blue", "orange", "green"))
  # dev.off()
  
  #Em linhas
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  plot(1, type = "n", axes=FALSE, xlab="", ylab="", asp = .1)
  legend("bottom", legend = c("C-vine", "Onion", "CM-generator", "Uncorrelated"),
         lty = c(1, 2, 3, 4), lwd = c(2,2,2,2), horiz = T, cex = 1.5, xpd = T)
  dev.off()
  
  #names(data_onion) <- paste("Liminf:", li, "- Dimens?o:", c(3:10, 20, 40, 50, 100, 200))
  #names(data_cvine) <- paste("Liminf:", li, "- Dimens?o:", c(3:10, 20, 40, 50, 100, 200))
  #names(data_gencor) <- paste("Liminf:", li, "- Dimens?o:", c(3:10, 20, 40, 50, 100, 200))
  
  #random_results[[list_index2]] <- list(data_onion, data_cvine, data_gencor)
  
  #list_index2 <- list_index2 + 1
  
}

time_onion  <- data.frame(time_onion )
time_cvine  <- data.frame(time_cvine )
time_gencor <- data.frame(time_gencor)

names(time_onion ) <- c("dim", "min", "mean", "sd", "max")
names(time_cvine ) <- c("dim", "min", "mean", "sd", "max")
names(time_gencor) <- c("li", "dim", "min", "mean", "sd", "max")


write.table(time_onion, "runtime_onion.txt", row.names = F)
write.table(time_cvine, "runtime_cvine.txt", row.names = F)
write.table(time_gencor, "runtime_gencor.txt", row.names = F)


#Converg?ncia dos limites - Ex: 0.3
par(mfrow = c(2, 4))

for(j in c(1000, 2000, 5000, 10000, 50000, 100000, 200000, 500000)){
  teste <- numeric()
  
  for(i in 1:1000){
    z <- rnorm(j, 0, 1)
    x1 <- rnorm(j, 0, 1.5275252) + z
    x2 <- rnorm(j, 0, 1.5275252) + z
    
    teste <- c(teste, cor(x1, x2))
  }
  
  hist(teste, xlim = c(.18, .4), 
       main = paste("Tamanho do vetor -", j,
                    "\nAmplitude da gera??o:", round(max(teste) - min(teste), 3),
                    "\nErro m?ximo:", abs(max(.3-c(min(teste), max(teste)))) %>% round(3),
                    "\n% de erros superiores a 0.01 =", 100*table(abs(teste - 0.3) > 0.01)[2]/1000), col = "gray")
  abline(v = c(min(teste), max(teste)))
}

save.image("Simulacoes.RData")





#Histograma 
dados_comp %>% ggplot(aes(x = cor, y = method, group = method)) + 
  geom_density_ridges(stat = 'binline', bins = 40) + facet_wrap(~dim)
