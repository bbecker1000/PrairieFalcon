cat("Patching unmarked\n")

setMethod("ranef", "unmarkedFitOccuMS", function(object, ...)
{

  N <- numSites(object@data)
  S <- object@data@numStates

  psi <- predict(object, "psi", se.fit=F)
  psi <- sapply(psi, function(x) x$Predicted)
  z <- 0:(S-1)

  p_all <- getP(object)
  y <- getY(getData(object))

  post <- array(0, c(N,S,1))
  colnames(post) <- z

  if(object@parameterization == "multinomial"){

    psi <- cbind(1-rowSums(psi), psi)

    guide <- matrix(NA,nrow=S,ncol=S)
    guide <- lower.tri(guide,diag=TRUE)
    guide[,1] <- FALSE
    guide <- which(guide,arr.ind=TRUE)
    for (i in 1:N){
      f <- psi[i,]
      g <- rep(1, S)
      p_raw <- sapply(p_all, function(x) x[i,])
      for (j in 1:nrow(p_raw)){
        if(any(is.na(p_raw[j,])) | is.na(y[i,j])) next
        sdp <- matrix(0, nrow=S, ncol=S)
        sdp[guide] <- p_raw[j,]
        sdp[,1] <- 1 - rowSums(sdp)
        for (s in 1:S){
          g[s] <- g[s] * sdp[s, (y[i,j]+1)]
        }
      }
      fudge <- f*g
      post[i,,1] <- fudge / sum(fudge)
    }

  } else if(object@parameterization == "condbinom"){

    psi <- cbind(1-psi[,1], psi[,1]*(1-psi[,2]), psi[,1]*psi[,2])

    for (i in 1:N){
      f <- psi[i,]
      g <- rep(1, S)
      p_raw <- sapply(p_all, function(x) x[i,])
      for (j in 1:nrow(p_raw)){
        probs <- p_raw[j,]
        if(any(is.na(probs)) | is.na(y[i,j])) next
        sdp <- matrix(0, nrow=S, ncol=S)
        sdp[1,1] <- 1
        sdp[2,1:2] <- c(1-probs[1], probs[1])
        sdp[3,] <- c(1-probs[2], probs[2]*(1-probs[3]), probs[2]*probs[3])
        for (s in 1:S){
          g[s] <- g[s] * sdp[s, (y[i,j]+1)]
        }
      }
      fudge <- f*g
      post[i,,1] <- fudge / sum(fudge)
    }
  }

  new("unmarkedRanef", post=post)

})




setMethod("simulate", "unmarkedFitOccuMS",
    function(object, nsim = 1, seed = NULL, na.rm=TRUE)
{

  S <- object@data@numStates
  N <- numSites(object@data)
  T <- object@data@numPrimary
  J <- obsNum(object@data) / T

  prm <- object@parameterization
  psi_raw <- predict(object, "psi", se.fit=F)
  psi_raw <- sapply(psi_raw, function(x) x$Predicted)
  p <- getP(object)

  guide <- matrix(NA,nrow=S,ncol=S)
  guide <- lower.tri(guide,diag=TRUE)
  guide[,1] <- FALSE
  guide <- which(guide,arr.ind=TRUE)

  out <- vector("list",nsim)

  for (i in 1:nsim){

  #State process
  if(prm == "multinomial"){
    psi <- cbind(1-apply(psi_raw,1,sum),psi_raw)
  } else if (prm == "condbinom"){
    psi <- matrix(NA, nrow=N, ncol=S)
    psi[,1] <- 1-psi_raw[,1]
    psi[,2] <- (1-psi_raw[,2])*psi_raw[,1]
    psi[,3] <- psi_raw[,1]*psi_raw[,2]
  }

  z <- matrix(NA, nrow=N, ncol=T)

  #initial occupancy
  for (n in 1:N){
    z[n,1] <- sample(0:(S-1), 1, prob=psi[n,])
  }

  #transitions if T>1----------------------------------------------------------
  get_phimat <- function(prob_vec){
    if(prm=="multinomial"){
      out <- matrix(NA, nrow=S, ncol=S)
      out[outer(1:S, 1:S, function(i,j) i!=j)] <- prob_vec
      out <- t(out)
      diag(out) <- 1 - rowSums(out,na.rm=T)
      return(out)
    } else if(prm == "condbinom"){
      out <- matrix(prob_vec, nrow=S)
      return(cbind(1-out[,1], out[,1]*(1-out[,2]), out[,1]*out[,2]))
    }
  }

  if(T>1){
    phi_raw <- predict(object, "phi", se.fit=F)
    phi_raw <- sapply(phi_raw, function(x) x$Predicted)
    phi_index <- 1
    for (n in 1:N){
      for (t in 2:T){
        phimat <- get_phimat(phi_raw[phi_index,])
        phi_t <- phimat[(z[n,(t-1)]+1),]
        z[n,t] <- sample(0:(S-1), 1, prob=phi_t)
        phi_index <- phi_index+1
      }
    }
  }
  #----------------------------------------------------------------------------

  #Detection process
  y <- matrix(0, nrow=N, ncol=J*T)
  for (n in 1:N){
    yindex <- 1
    for (t in 1:T){
      if (z[n,t] == 0) {
        yindex <- yindex + J
        next
      }
      for (j in 1:J){

        if(prm == "multinomial"){
          probs_raw <- sapply(p, function(x) x[n,yindex])

          sdp <- matrix(0, nrow=S, ncol=S)
          sdp[guide] <- probs_raw
          sdp[,1] <- 1 - rowSums(sdp)

          probs <- sdp[z[n,t]+1,]

        } else if (prm == "condbinom"){
          p11 <- p[[1]][n,yindex]
          p12 <- p[[2]][n,yindex]
          p22 <- p[[3]][n,yindex]
          probs <- switch(z[n,t]+1,
                          c(1,0,0),
                          c(1-p11,p11,0),
                          c(1-p12,p12*(1-p22),p12*p22))
        }
        if(all(!is.na(probs))){
          y[n,yindex] <- sample(0:(S-1), 1, prob=probs)
        } else {
          y[n,yindex] <- NA
        }
        yindex <- yindex + 1
      }
    }
  }

  out[[i]] <- y
  }

  out
})
