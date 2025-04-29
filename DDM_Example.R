library(ggplot2)

# parameter values for 1 subject (example)
drift_rate <- 0.8747562  
threshold  <- 1.0  
noise   <- 0.29     
z <- threshold/2    
NDT <- 326     
nsamples <- 1500 
dt       <- 0.001 
evidence <- rep(NA, 1, nsamples)
timestep <- 1
evidence[1] = z;

#sampling until threshold
while (TRUE){
  evidence[timestep+1] <- evidence[timestep] + rnorm(mean = drift_rate*dt, sd = noise, n = 1)
  
  if (evidence[timestep] >= threshold || evidence[timestep] < 0 || timestep == nsamples) {
    break
  }
  
  timestep <- timestep + 1
}

# model prediction about RT and choice
if (timestep >= nsamples) { 
  RT <- NA 
  Choice <- NA
} else {
  RT <- timestep + NDT
  Choice <- which.min(c(abs(evidence[timestep]-0), abs(evidence[timestep]-threshold)))
}

#plot trajectories in stimulus-locked manner
windows(1, 4)
plot(evidence[1:timestep], col = "#002060", type = 'l',xlim = c(0,400), xlab="", ylim = c(0,threshold), ylab="",
     main="", lwd = 2, xaxt = "n", yaxt = "n", bty = "n")

abline(h = 0, col = "#E97132", lwd = 1)    
abline(h = 1, col = "#E97132", lwd = 1)    

arrows(x0 = 0, y0 = 0.5, 
       x1 = 300, y1 = 0.5, 
       length = 0.1,     
       col = "#E97132", 
       lwd = 2)


