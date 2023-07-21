#Experiments presented on paper "CM-generator: an approach for generating customized correlation matrices"

##Note: given that the authors are brazillian some terms may be in Portuguese

#Used packages
pacman::p_load(gencor, tidyverse, clusterGeneration, MASS, gencor, mvtnorm, janitor, ggridges, data.table)

#Sets the work directory
setwd(choose.dir())

#Function to remove 1's of correlation matrices
removone <- function(x) x[-which(x == 1)]

#Random seed
set.seed(1234)

#Experiments

#Figure 1 and 2 - Comparison among methods C-vine, Onion, CM-generator and uncorrelated generation

#Descriptive statistics
desc <- function(x)return(c(min(x), mean(x), sd(x), max(x)))

#Empty objects to store simulation values
random_results <- data.frame()
time_onion <- numeric()
time_cvine <- numeric()
time_gencor <- numeric()

#Simulation number
nsim <- 1000

#Object to store execution time
time_results <- data.frame()

#Loop for lower limits
for(li in c(0.005, 0.01, 0.015, 0.02, 0.025)){

  #Loop for matrix dimension
  for(dim in c(3:10, 20, 50, 100, 200)){
    
    #Empty objects to store correlations, dimensions and limits
    dados_comp <- NULL
    
    #Empty vectors to store correlations and runtime for each method
    cor_cvine <- numeric()
    cor_onion <- numeric()
    cor_uncor <- numeric()
    cor_gencor <- numeric()
    rt_cvine <- numeric()
    rt_onion <- numeric()
    rt_gencor <- numeric()
    
    #Loop for simulations
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
      
      #Counter
      cat("\014")
      print(paste(i, dim, li))
      
    }
    
    #Stores descriptive statistics for runtime at each method
    time_onion <- rbind(time_onion, c(dim, desc(rt_onion)))
    time_cvine <- rbind(time_cvine, c(dim, desc(rt_cvine)))
    time_gencor <- rbind(time_gencor, c(li, dim, desc(rt_gencor)))
    
    #Setup for data.table size
    nrep <- length(cor_cvine %>% removone)
    
    #Data.table with the results for each dimension
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
    
    #Generate border graphics (With y-axis)
    if(dim %in% c(3, 7, 20, 100)){
      
      #Histograms
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
      
      #Density plots
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
      
      #Smoothed density plots
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
      
      #Center graphics (Without y-axis)
    }else{
      
      #Histograms
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
      
      #Density plots
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
      
      #Smoothed density plots
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
  }
}

#Descriptive statistics for runtime
time_onion  <- data.frame(time_onion )
time_cvine  <- data.frame(time_cvine )
time_gencor <- data.frame(time_gencor)

names(time_onion ) <- c("dim", "min", "mean", "sd", "max")
names(time_cvine ) <- c("dim", "min", "mean", "sd", "max")
names(time_gencor) <- c("li", "dim", "min", "mean", "sd", "max")

#Figure 3 - Histograms for each lower limit

##Object to store correlations
corr <- NULL

#Loop for lower limits
for(k in c(0.001, 0.005, 0.01, 0.015, 0.02, 0.025)){
  
  correlations <- numeric()
  #for(i in c(4, 5, 10, 20, 40, 50, 100, 200)){
  for(i in 1:1000){
    
    correlations <- c(correlations, gencor(30, random_liminf = k)$Matrix %>% removone())
    
  }
  
  #Stores simulated correlations
  corr <- rbind(corr, data.frame(cor = correlations, li = rep(k, length(correlations))))
  
}


#Plots the graphic
corr %>% 
  ggplot(aes(x = cor, y = factor(li), group = factor(li)))+ 
  geom_density_ridges(stat = "binline", bins = 50, alpha = 0.5, size = 1.1) + 
  theme_bw() + 
  theme(plot.background =  element_blank(),
        panel.grid = element_blank()) + 
  labs(x = element_blank(),
       y = element_blank()) + 
  scale_y_discrete(expand = expansion(c(0.1, 0.3)))

#Saves the graphic
ggsave("liminf_density.pdf", width = 10, height = 7)


#Figures 4 and 5 - Limited and customized correlation matrices

#Empty vectors to store correlations
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

#Empty vectors to store proportions of correlations in each intensity
prop_custom504010 <- numeric()
prop_custom404020 <- numeric()
prop_custom105040 <- numeric()
prop_custom302050 <- numeric()
prop_custom108010 <- numeric()
prop_custom008020 <- numeric()
prop_custom500050 <- numeric()
prop_custom703000 <- numeric()

#Stores runtime
tempos <- numeric()


#Loop for simulations
for(i in 1:1000){
  
  #Matrix correlation intensity: low correlations: r in (-0.3, 0.3)
  #                              medium correlations: r in (-0.6, -0.3] U [0.3, 0.6)
  #                              high correlations: r in (-1, -0.6] U [0.6, 1)
  
  #Low correlations matrix
  glow <- gencor(20, "low")
  mlow <- c(mlow, glow$Matrix)
  
  #Medium correlations matrix
  gmedium <- gencor(20, "medium")
  mmedium <- c(mmedium, gmedium$Matrix)
  
  #High correlations matrix
  ghigh <- gencor(20, "high")
  mhigh <- c(mhigh, ghigh$Matrix)
  
  
  #Customized matrices: low correlations: r in (-0.3, 0.3)
  #                     medium correlations: r in (-0.6, -0.3] U [0.3, 0.6)
  #                     high correlations: r in (-1, -0.6] U [0.6, 1)
  
  #Customized matrix - 50% low correlations, 40% medium correlations, 10% high correlations
  g504010 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.5, .4, .1))
  mcustom504010 <- c(mcustom504010, g504010$Matrix)
  prop_custom504010 <- rbind(prop_custom504010, g504010$Proportions)
  
  #Customized matrix - 40% low correlations, 40% medium correlations, 20% high correlations
  g404020 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.4, .4, .2))
  mcustom404020 <- c(mcustom404020, g404020$Matrix)
  prop_custom404020 <- rbind(prop_custom404020, g404020$Proportions)
  
  #Customized matrix - 10% low correlations, 50% medium correlations, 40% high correlations
  g105040 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.1, .5, .4))
  mcustom105040 <- c(mcustom105040, g105040$Matrix)
  prop_custom105040 <- rbind(prop_custom105040, g105040$Proportions)
  
  #Customized matrix - 30% low correlations, 20% medium correlations, 50% high correlations
  g302050 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.3, .2, .5))
  mcustom302050 <- c(mcustom302050, g302050$Matrix)
  prop_custom302050 <- rbind(prop_custom302050, g302050$Proportions)
  
  #Customized matrix - 10% low correlations, 80% medium correlations, 10% high correlations
  g108010 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.1, .8, .1))
  mcustom108010 <- c(mcustom108010, g108010$Matrix)
  prop_custom108010 <- rbind(prop_custom108010, g108010$Proportions)
  
  #Customized matrix - 0% low correlations, 80% medium correlations, 20% high correlations
  g008020 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.0, .8, .2))
  mcustom008020 <- c(mcustom008020, g008020$Matrix)
  prop_custom008020 <- rbind(prop_custom008020, g008020$Proportions)
  
  #Customized matrix - 50% low correlations, 0% medium correlations, 50% high correlations
  g500050 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.5, .0, .5))
  mcustom500050 <- c(mcustom500050, g500050$Matrix)
  prop_custom500050 <- rbind(prop_custom500050, g500050$Proportions)
  
  #Customized matrix - 70% low correlations, 30% medium correlations, 0% high correlations
  g703000 <- gencor(20, "custom", custom_lim = c(.3, .6), custom_prop = c(.7, .3, .0))
  mcustom703000 <- c(mcustom703000, g703000$Matrix)
  prop_custom703000 <- rbind(prop_custom703000, g703000$Proportions)
  
  #Simulation runtime for each configuration
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

#Average time and standard deviation

sink("Tempos_simulacao_custom.txt")
L <- lapply(split(tempos$tempo, tempos$Cenario), function(x)return(c(min(x), mean(x), sd(x), max(x))))
tempos2 <- Reduce(rbind, L) %>% data.frame
rownames(tempos2) <- names(L)
names(tempos2) <- c("Min", "Mean", "Sd", "Max")
tempos2
sink()

#Average simulation proportions in each intensity
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

#Expected values
esperado <- data.frame(Exp_low = c(0.5, 0.4, .1, .3, .1, 0, .5, .7), Exp_medium = c(.4,.4,.5,.2,.8,.8,0,.3), Exp_high = c(.1,.2,.4,.5,.1,.2,.5,0))

prop_med <- data.frame(prop_med, esperado, abs(prop_med - esperado))
names(prop_med)[7:9] <- c("Dif_low", "Dif_medium", "Dif_high")


#Graphics for basic methods (low, medium and high correlations) - Figure 4
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

#Graphics for customized methods - Figure 5
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
