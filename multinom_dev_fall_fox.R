#  Multinomial analysis to calculate molt start and end dates 
#     (and covariates effects) in SHH
# 3 categories of molt
# Random effects on camera traps
#  06/2017
#  Josh Nowak
################################################################################
library(R2jags)
library(readr)
library(purrr)
library(dplyr)
library(beepr)
library(mcmcplots)
################################################################################
#  Set working directory
#setwd("~/Documents/Animal ecology paper/New model/multinomial_molt_analysis_Foxes")
setwd("/Users/marketzimova/Documents/WORK/DISSERTATION/GitHub/Foxes")
#setwd("G:/Github/Foxes")

#  Path to data
#jjn <- "~/Documents/Animal ecology paper/New model/molts5.3.csv"
jjn <- "molts5.3.csv"

#  Source functions
#source("code/utility_functions_fox.R")
source("utility_functions_fox.R")
################################################################################
#  Load data
rawd <- read_csv(
  jjn,
  col_types = "cciiiiccccci"
)
################################################################################
#  Morph raw data
hares <- morph_data(rawd) %>%
  filter(
    Season == "Fall",
    Year == 2016,
    Area == "helags",
    Morph == "white"
  )
################################################################################
#  Call a single model step by step - mimics jags_call
#  Set time_scale for the analysis
#  Options are in the column names of hares, Month, Week, Julian
time_scale <- "Julian"

load.module("glm")

#  Subset to days - to reduce redundancy and ease inits and data create
days <- as.integer(unlist(hares[,time_scale]))
first_day <- min(days)
last_day <- max(days)

#  Create categorical response
response <- cut(hares$White3, 3, labels = 1:3)

#  Inits
inits <- function(){
  list(
    alpha = rnorm(3)
  )
}

#  Gather data
dat <- list(
  nobs = nrow(hares),
  day = days, 
  cam = as.numeric(as.factor(hares$CameraNum)),
  y = response,
  nbins = 3,
  ndays = last_day,
  ncam = length(unique(hares$CameraNum))#,
#  elev = as.numeric(hares$Elevation)
)

# Parameters to monitor
parms <- c(
  "pp", "beta", "alpha", "p_rand"#, "elev_eff"
)

#  Call jags
start.time <- Sys.time()
out <- jags(
  data = dat, 
  inits = NULL,
  parameters.to.save = parms,
  model.file = "models/multinom.txt", 
  n.chains = 3,
  n.iter = 100000,
  n.burnin = 50000,
  n.thin = 3
)
end.time <- Sys.time();(time.taken <-end.time-start.time)
beep()

################################################################################
# Save results out
#save(out, file = "G:/GitHub/multinomial_molt_analysis/results/2016out.RData"); load("2014out.RData")

# Save results as csv
#writes csv with results
out.sum <- out$BUGS$summary 
write.table(out.sum, file="Results/Hel_White_2016_fall_100K.csv",sep=",")

#options(max.print=100000) #extend maximum for print
#print(out)
#out$BUGS$mean$elev_eff

################################################################################
# Plots
#  Find start dates
starts <- apply(out$BUGS$sims.list$pp[,1,], 1, function(x){ 
  min(which(x < 0.5)) 
})
hist(starts, xlab = "Day")
quantile(starts, c(0.025, 0.5, 0.975))

#  Find end dates
ends <- apply(out$BUGS$sims.list$pp[,3,], 1, function(x){ 
  min(which(x > 0.5)) 
})
hist(ends, xlab = "Day")
quantile(ends, c(0.025, 0.5, 0.975))

#  Find mid-points
mids <- apply(out$BUGS$sims.list$pp[,2,], 1, function(x){ 
  min(which(x == max(x)))
})
hist(mids, xlab = "Day")
quantile(mids, c(0.025, 0.5, 0.975))

#Plot start and end dates and mean pps
plot(0, 0, type = "n", col = "red", bty = "l",
     ylim = c(-.1, 1.1), xlim = c(150, 350),
     xlab = "Time", ylab = "Probability of being in bin 'x'")

day_seq <- 1:dim(out$BUGS$mean$pp)[2]
points(hares$Julian, jitter(hares$White3/100), pch = 19, cex = 1, col = "gray60")

for(i in 1:3){
  lines(day_seq, out$BUGS$mean$pp[i,], col = i, type = "l")
}
abline(v=c(quantile(starts, 0.5)), col="green");abline(v=c(quantile(mids, 0.5)), col="red");abline(v=c(quantile(ends, 0.5)), col="black")
abline(v=c(quantile(starts, 0.025), quantile(starts, 0.975)), col = "green", lty = 3)
abline(v=c(quantile(mids, 0.025), quantile(mids, 0.975)), col = "red", lty = 3)
abline(v=c(quantile(ends, 0.025), quantile(ends, 0.975)), col = "black", lty = 3)
hist(starts, add = T, freq = F, col = "green", border = "green")
hist(ends, add = T, freq = F, col = "black", border = "black")  
hist(mids, add = T, freq = F, col = "red", border = "red")  
text(150, 0.2, paste("Helags White Fall 2016",
                    "\n100K/50K conv",
                   #"\nelev_eff1 =", quantile(signif(out$BUGS$sims.list$elev_eff[,1],digits=2),0.025),quantile(signif(out$BUGS$sims.list$elev_eff[,1],digits=2),0.5),quantile(signif(out$BUGS$sims.list$elev_eff[,1],digits=2),0.925),
                   #"\nelev_eff2 =", quantile(signif(out$BUGS$sims.list$elev_eff[,2],digits=2),0.025),quantile(signif(out$BUGS$sims.list$elev_eff[,2],digits=2),0.5),quantile(signif(out$BUGS$sims.list$elev_eff[,2],digits=2),0.925),
                   "\nStarts =", quantile(starts, 0.025),quantile(starts, 0.5),quantile(starts, 0.975),
                   "\nMids =", quantile(mids, 0.025),quantile(mids, 0.5),quantile(mids, 0.975),
                   "\nEnds =", quantile(ends, 0.025),quantile(ends, 0.5),quantile(ends, 0.975)), pos = 4, cex=0.9)



########################################################################################################################
#  Plot with random effects
plot(0, 0, type = "n", col = "red", bty = "l",
     ylim = c(-.1, 1.1), xlim = c(0, 200),
     xlab = "Time",ylab = "Probability of being in bin 'x'")

day_seq <- 1:dim(out$BUGS$mean$pp)[2]

pr_dim <- dim(out$BUGS$mean$p_rand)
ncamera <- pr_dim[2]
ncategories <- pr_dim[1]
nday <- pr_dim[3]

#  Save values of per camera bin probabilities for export
mat <- expand.grid( 
  cats = 1:ncategories,
  cam = 1:ncamera,
  day = day_seq
)

# Create df with all cameras
for(i in 1:nrow(mat)){
  mat$bin_prob[i] <- out$BUGS$mean$p_rand[
    mat$cats[i],
    mat$cam[i],
    mat$day[i]
    ]
}
#mat

#  Add lines to plot for each camera
points(hares$Julian, jitter(hares$White3/100), pch = 19, cex = 1, col = "gray80")
for(i in 1:ncategories){
  for(j in 1:ncamera){
    lines(day_seq, out$BUGS$mean$p_rand[i,j,], col = "gray70", type = "l") #or col =i
  }
}

for(i in 1:3){
  lines(day_seq, out$BUGS$mean$pp[i,], col = i, type = "l")
}
abline(v=c(quantile(starts, 0.5)), col="green");abline(v=c(quantile(mids, 0.5)), col="red");abline(v=c(quantile(ends, 0.5)), col="black")
abline(v=c(quantile(starts, 0.025), quantile(starts, 0.975)), col = "green", lty = 3)
abline(v=c(quantile(mids, 0.025), quantile(mids, 0.975)), col = "red", lty = 3)
abline(v=c(quantile(ends, 0.025), quantile(ends, 0.975)), col = "black", lty = 3)
hist(starts, add = T, freq = F, col = "green", border = "green")
hist(ends, add = T, freq = F, col = "black", border = "black")  
hist(mids, add = T, freq = F, col = "red", border = "red")  
text(0, 0.2, paste("Starts =", quantile(starts, 0.025),quantile(starts, 0.5),quantile(starts, 0.975),
                   "\nMids =", quantile(mids, 0.025),quantile(mids, 0.5),quantile(mids, 0.975),
                   "\nEnds =", quantile(ends, 0.025),quantile(ends, 0.5),quantile(ends, 0.975)), pos = 4, cex=0.9)
# legend("topright",legend = paste(c(0,50,100),"% white"),lty = 1,col = 1:3)


################################################################################
#Diagnostics plots
mcmcplot(out, parms = c("pp", "beta", "alpha", "sigma", "rho"#, "p_rand"
                        #, "elev_eff", "cat_mu")
))


################################################################################ 
#Diagnostics plots
mcmcplot(out, parms = c("pp", "beta", "alpha", "sigma", "rho","elev_eff"#
                        #, "p_rand", "cat_mu")
))
################################################################################ 
hist(out$BUGS$sims.list$elev_eff[,1], breaks = 200);hist(out$BUGS$sims.list$elev_eff[,2], breaks = 200)
hist(out$BUGS$sims.list$sigma[,1], breaks = 200);hist(out$BUGS$sims.list$sigma[,2], breaks = 200)
hist(out$BUGS$sims.list$rho, breaks = 200)
hist(out$BUGS$sims.list$alpha[,1], breaks=200);hist(out$BUGS$sims.list$alpha[,2], breaks=200)
hist(out$BUGS$sims.list$beta[,1], breaks=200);hist(out$BUGS$sims.list$beta[,2], breaks=200)

out.mcmc <- as.mcmc(out) # Convert model output into an MCMC object
str(out.mcmc)
library(coda)
plot(out.mcmc)
#out.mtx <- as.matrix(out.mcmc)
#out.df <- as.data.frame(out.mcmc) # all itterations
#mymodel.p <- out.df[, grep("p[", colnames(out.df), fixed=T)] #only p's
#write.csv(mymodel.p, file = "model.csv") # all itterations for p

#print(out$BUGS$sd) # or instead of mean: sd, median
#a <-print(out$BUGSoutput$sims.array)
#str(out)

hist(out$BUGS$sims.list$beta[,1])
plot(density(out$sims.matrix[,"pp"]))

jpeg(file =/Users/marketzimova/Documents/WORK/DISSERTATION/GitHub/data/SSH/myplot.jpeg)
#or
library(lattice)
xyplot(out.mcmc, layout=c(10,10), aspect="fill") # chains history
dev.off()


densityplot(out.mcmc, layout=c(10,10), aspect="fill") # posteriors
#autocorr.plot(out.mcmc) # autocorrelation plot
#gelman.plot(out.mcmc) 

#####