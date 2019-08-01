setwd('/Users/jerome/Documents/Imperial/ICL2019/')
rm(list=ls())
CONFIG.N <- 200
set.seed(3)

rextreme <- function(N, t_bar=N/2, p=0.05, delta=5) {
 rnorm(N) + (seq(1, N) >= t_bar)*rbinom(N, size=1, prob=p)*delta
}
plot(rextreme(CONFIG.N), pch=21, bg='black', cex=0.2)

rjump <- function(N, t_bar=N/2, delta_J=1){
  rnorm(N) + (seq(1, N) >= t_bar)*delta_J
}
plot(rjump(CONFIG.N), pch=21, bg='black', cex=0.2)

rperiodic <- function(N, t_bar=N/2, period=20,
                      sigma_P=sqrt(1-mean(sin(2*pi*seq(1,period)/period)^2))){
  ((seq(1, N) >= t_bar)*sigma_P + (seq(1, N) < t_bar))*rnorm(N) +
    (seq(1,N) >= t_bar)*sin(2*pi*seq(1,N)/period)
}
plot(rperiodic(CONFIG.N), pch=21, bg='black', cex=0.2)
plot(rperiodic(CONFIG.N)[(CONFIG.N/2):(CONFIG.N/2+100)], pch=21, bg='black', cex=0.5)
mean(rperiodic(CONFIG.N)[(CONFIG.N/2):CONFIG.N]^2)


rar1 <- function(N, t_bar=N/2, rho_ar=0.7, sigma_ar=sqrt(1 - rho_ar^2)){
  rvs <- rep(NA, N)
  rvs[1:(t_bar-1)] <- rnorm(t_bar-1)
  for (k in seq(t_bar, N)){
    rvs[k] <- rnorm(1, mean=rho_ar*rvs[k-1], sd=sigma_ar)
  }
  return(rvs)
}

plot(rar1(CONFIG.N), pch=21, bg='black', cex=0.2)
plot(rar1(CONFIG.N)[(CONFIG.N/2):(CONFIG.N/2+100)], pch=21, bg='black', cex=0.5)
mean(rar1(CONFIG.N)[(CONFIG.N/2):CONFIG.N]^2)


# So we have our four processes, now let us make a faceted plot containing each one.

require(ggplot2)

Stream_index <- seq(1, CONFIG.N)

# Extreme value process plot
pextreme <- ggplot() + 
  geom_line(aes(x=c(CONFIG.N/2, CONFIG.N/2), y=c(-7, 7)), linetype='dashed') +
  geom_line(aes(x=c(0, CONFIG.N), y=c(0, 0)), color='gray') +
  geom_point(aes(x=Stream_index, y=rextreme(CONFIG.N)), 
             pch=21, fill='black', color='white', stroke=0) + 
  theme(plot.margin=unit(c(0,0,-0.2,0),"cm"), axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  scale_x_continuous('', breaks=c(0,CONFIG.N), limits=c(0, CONFIG.N)) +
  scale_y_continuous('Residual value', breaks=seq(-7, 7, 2), limits=c(-7, 7))

# Jump process plot
pjump <- ggplot() + 
  geom_line(aes(x=c(CONFIG.N/2, CONFIG.N/2), y=c(-7, 7)), linetype='dashed') +
  geom_line(aes(x=c(0, CONFIG.N), y=c(0, 0)), color='gray') +
  geom_point(aes(x=Stream_index, y=rjump(CONFIG.N)), 
             pch=21, fill='black', color='white', stroke=0) + 
  theme(plot.margin=unit(c(0,0,-0.2,0),"cm"), axis.text=element_text(size=8), axis.title=element_text(size=8)) +
  scale_x_continuous('', breaks=c(0,CONFIG.N), limits=c(0, CONFIG.N)) +
  scale_y_continuous('Residual value', breaks=seq(-7, 7, 2), limits=c(-7, 7))

# Periodic process plot
pperiodic <-  ggplot() + 
      geom_line(aes(x=c(CONFIG.N/2, CONFIG.N/2), y=c(-7, 7)), linetype='dashed') +
      geom_line(aes(x=c(0, CONFIG.N), y=c(0, 0)), color='gray') +
      geom_point(aes(x=Stream_index, y=rperiodic(CONFIG.N)), 
                 pch=21, fill='black', color='white', stroke=0) + 
      theme(plot.margin=unit(c(0,0,-0.2,0),"cm"), axis.text=element_text(size=8), axis.title=element_text(size=8)) +
      scale_x_continuous('', breaks=c(0,CONFIG.N), limits=c(0, CONFIG.N)) +
      scale_y_continuous('Residual value', breaks=seq(-7, 7, 2), limits=c(-7, 7)) 

# AR process plot
par <-  ggplot() + 
       geom_line(aes(x=c(CONFIG.N/2, CONFIG.N/2), y=c(-7, 7)), linetype='dashed') +
       geom_line(aes(x=c(0, CONFIG.N), y=c(0, 0)), color='gray') +
       geom_point(aes(x=Stream_index, y=rar1(CONFIG.N)), 
                  pch=21, fill='black', color='white', stroke=0) + 
       theme(plot.margin=unit(c(0,0,0,0),"cm"), axis.text=element_text(size=8), axis.title=element_text(size=8)) +
       scale_x_continuous('Stream index', breaks=c(0,CONFIG.N), limits=c(0, CONFIG.N)) +
       scale_y_continuous('Residual value', breaks=seq(-7, 7, 2), limits=c(-7, 7))

require(gridExtra)

pdf('001_SimulationProcessExamples.pdf', width=5, height=6) # a4 is 8.3 x 11.7 inches
grid.arrange(pextreme, pjump, pperiodic, par, nrow=4)
dev.off()
