### Goodness of fit statistics

# AICcmodavg::mb.gof.test(mod04b_stacked_fit) # doesn't work with occuMS()
# residuals
plot(mod04b_stacked_fit) #nothing too crazy


## trying patch from Ken Kellner to run ranef and parpoot on model with NAs (2022-07-26)
source("Code/occuMS_patch.R") 
ranef(mod04b_stacked_fit)  # Works !
parboot(mod04b_stacked_fit) # works !

# Stats for parboot to fit
# basically want chi.sq and c-hat
# function
fitstats <- function(mod04b_stacked_fit, 
                     method = "nonparboot") {
  observed <- getY(mod04b_stacked_fit@data)
  expected <- fitted(mod04b_stacked_fit)
  resids <- residuals(mod04b_stacked_fit,
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



## insert model name here:
MODEL_NAME <- modBig01_stacked_fit

#run parboot
pb <- parboot(MODEL_NAME,
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




