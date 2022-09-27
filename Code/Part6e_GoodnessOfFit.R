######################################################################################################
## Project: Falcon Occupancy Analysis 2022
## Script Purpose: Code assess goodness of fit of the best model in multinomial
## Libraries and versions used: unmarked
#####################################################################################################

source("Code/Part6f_FitBigStaticCausalModels.R")
library(unmarked)

### Goodness of fit statistics

# AICcmodavg::mb.gof.test(mod04b_stacked_fit) # doesn't work with occuMS()
# residuals



plot(BEST_MODEL_multi) #nothing too crazy


## trying patch from Ken Kellner to run ranef and parpoot on model with NAs (2022-07-26)
source("Code/occuMS_patch.R") 
posteriroDist = ranef(BEST_MODEL_multi)# Works !
# bup(posteriroDist) extract mean
parboot(BEST_MODEL_multi) # error?

# Stats for parboot to fit
# basically want chi.sq and c-hat
# function
fitstats <- function(BEST_MODEL_multi, 
                     method = "nonparboot") {
  observed <- getY(BEST_MODEL_multi@data)
  expected <- fitted(BEST_MODEL_multi)
  resids <- residuals(BEST_MODEL_multi,
                      method = "nonparboot")
  sse <- sum(resids^2,
             na.rm = TRUE)
  chisq <- sum((observed - expected)^2 / expected,
               na.rm = TRUE)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2, 
                  na.rm = TRUE)
  out <- c(SSE = sse,
           Chisq = chisq,
           freemanTukey = freeTuke)
  return(out)
}



#run parboot
pb <- parboot(BEST_MODEL_multi,
              fitstats,
              nsim = 100,
              report = TRUE,
              method = "nonparboot")
pb

# plot space
par(mfrow = c(3,1))

plot(pb,
     main = "",
     xlab = c("SSE", "Chisq", "FT"))

#c-hat equation from google group
cHat_pb <- pb@t0 / mean(pb@t.star)
cHat_pb



## additional GOF measures -------------------

## cross val
#k-fold cross validation with 10 folds
# require multinom
(kfold = crossVal(BEST_MODEL_multi, method="Kfold", folds=20))

#holdout method
(holdout = crossVal(BEST_MODEL_multi,method='holdout', holdoutPct=0.25))

#Leave-one-out method
(leave = crossVal(BEST_MODEL_multi, method='leaveOneOut'))

# When the sign of a coefficient is not what one expects (or actually even if that is not the case), 
# one can check to see if the rank of the hessian is equal to the number of parameters in the model.  
# If the rank is less than the number of parameters, then one has overfitted the data with the model and that 
# can cause unexpected signs on coefficients.  
# The quick way to check on that is to use the following on the result from colext
# (and even occu and several other unmarked functions).  If that result is named fm, then execute the following:

qr(BEST_MODEL_multi@opt$hessian)$rank - length(BEST_MODEL_multi@opt$par) 

# If this is less than zero, you have problems. 




