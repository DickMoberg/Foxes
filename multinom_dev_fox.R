#  Multinomial analysis to calculate molt start and end dates 
#     (and covariates effects) in foxes
# 3 categories of molt
# Random effects on individuals
#  06/2017
#  Josh Nowak
################################################################################
    library(R2jags)
    library(readr)
    library(purrr)
    library(dplyr)
################################################################################
    #  Set working directory
    #setwd("~/Documents/Animal ecology paper/New model/multinomial_molt_analysis_Foxes")
    setwd("~/Documents/WORK/DISSERTATION/GitHub")
    #  Path to data
    #jjn <- "~/Documents/Animal ecology paper/New model/molts5.3.csv"
    jjn <- "data/foxes/molts5.3.csv"
     
    #  Source functions
    #source("code/utility_functions_fox.R")
    source("Foxes/utility_functions_fox.R")
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
        Season == "Spring",
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
      ncam = length(unique(hares$CameraNum))
    )
    
    # Parameters to monitor
    parms <- c(
      "pp", "beta", "alpha", "sig_cam", "tau_cam"#,"p_rand"
    )

    #  Call jags
    out <- jags(
      data = dat, 
      inits = NULL,
      parameters.to.save = parms,
      model.file = "models/multinom_randCam.txt", 
      n.chains = 3,
      n.iter = 100000,
      n.burnin = 50000,
      n.thin = 3 
    )
################################################################################
    options(max.print=300) #extend maximum for print
    print(out)
    #out$BUGS$mean$p_rand
    
    #  Find start dates
    starts <- apply(out$BUGS$sims.list$pp[,3,], 1, function(x){ 
      min(which(x < 0.5)) 
    })
    
    hist(starts, xlab = "Day")
    quantile(starts, c(0.025, 0.5, 0.975))
    
    #  Find end date
    ends <- apply(out$BUGS$sims.list$pp[,1,], 1, function(x){ 
      min(which(x > 0.5)) 
    })
    
    hist(ends, xlab = "Day")
    quantile(ends, c(0.025, 0.5, 0.975))
  
    plot(0, 0, 
      type = "n", 
      col = "red",
      ylim = c(-.1, 1.1),
      xlim = c(100, 218),
      xlab = "Time",
      ylab = "Probability of being in bin 'x'",
      bty = "l"
    )
    
    day_seq <- 1:dim(out$BUGS$mean$pp)[2]
    
    for(i in 1:3){
      lines(day_seq, out$BUGS$mean$pp[i,], col = i, type = "l")
    }
    points(hares$Julian, jitter(hares$White/100), pch = 19, cex = 1, col = "gray90")
    abline(v=c(128,197),col = "gray90")
    abline(v=c(quantile(starts, 0.5), quantile(ends, 0.5)))
    hist(starts, add = T, freq = F, col = "green", border = "green")
    hist(ends, add = T, freq = F, col = "black", border = "black")  




    #  Plot with random effects
    plot(0, 0, 
      type = "n", 
      col = "red",
      ylim = c(-.1, 1.1),
      xlim = c(100, 250),
      xlab = "Time",
      ylab = "Probability of being in bin 'x'",
      bty = "l"
    )
    
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
    
    
    #  Add lines to plot for each camera
    for(i in 1:ncategories){
      for(j in 1:ncamera){
        lines(day_seq, out$BUGS$mean$p_rand[i,j,], col = i, type = "l")
      }
    }
    points(hares$Julian, jitter(hares$White/100), pch = 19, cex = 1, col = "gray90")
    

    # legend(
    #   "topright",
    #   legend = paste(c(0,50,100),"% white"),
    #   lty = 1,
    #   col = 1:3
    # )
    
    out.sum <- out$BUGS$summary 
    write.table(out.sum, file="out2014.csv",sep=",") #writes csv with results
    #xx <- read.csv("out2015.csv"); str(xx)
    

################################################################################ 
    
    #Diagnostics plots
    out.mcmc <- as.mcmc(out) # Convert model output into an MCMC object
    #library(coda)
    #plot(out.mcmc)
    #out.mtx <- as.matrix(out.mcmc)
    #out.df <- as.data.frame(out.mcmc) # all itterations
    #mymodel.p <- out.df[, grep("p[", colnames(out.df), fixed=T)] #only p's
    #write.csv(mymodel.p, file = "model.csv") # all itterations for p
   
    #print(out$BUGS$sd) # or instead of mean: sd, median
    #a <-print(out$BUGSoutput$sims.array)
    #str(out)

    library(lattice)
    xyplot(out.mcmc, layout=c(3,3), aspect="fill") # chains history
    densityplot(out.mcmc, layout=c(3,3), aspect="fill") # posteriors
    #autocorr.plot(out.mcmc) # autocorrelation plot
    #gelman.plot(out.mcmc) 
    
    #####