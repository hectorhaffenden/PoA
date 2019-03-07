# Workflow
library(tidyverse) # Tibbles etc
library(pracma) # Used for integrals
options(scipen=999)
############################
    # OPTIMAL FLOW #
############################
OptimalFlow <- function(cost.matrix, flo.matrix, accuracy, variable.limits){
  # Use variables alpha, beta, theta...
  # Keep accruacy to 0.005, unless u have a long time
  #
  #
  #
  Composite <- function(f,g) function(...) f(g(...))
  Cost_Comp_Flow <- function(costs.matrix, flows.matrix){
    # First compose the cost and flo
    cf.hold <- costs.matrix
    for (i in 1:nrow(costs.matrix)){
      for (j in 1:ncol(costs.matrix)){
        want <- NULL
        c.hold <- costs.matrix[[i,j]]
        f.hold <- flows.matrix[[i,j]]
        want <- Composite(c.hold, f.hold)
        # Why the hell does this make it work? seems like calling the function every time helps?
        # Currently handles functions that hold up to 6 arguments, easily updated to handle more.
        if (length(formals(f.hold)) == 1){
          want(1)
        } else if (length(formals(f.hold)) == 2){
          want(1,1)
        } else if (length(formals(f.hold)) == 3){
          want(1,1,1)
        } else if (length(formals(f.hold)) == 4){
          want(1,1,1,1)
        } else if (length(formals(f.hold)) == 5){
          want(1,1,1,1,1)
        } else if (length(formals(f.hold)) == 6){
          want(1,1,1,1,1,1)
        }
        cf.hold[[i,j]] <- want
      }
    }
    # Now run through each and times it by the flow
    return(cf.hold)
  }
  
  fin.values <- NULL
  
  if (length(variable.limits) == 2){
    alpha.seq <- seq(from = 0, to = variable.limits[1], by = accuracy)
    beta.seq <- seq(from = 0, to = variable.limits[2], by = accuracy)
    
    holding <- NULL
    a <- Cost_Comp_Flow(cost.matrix, flo.matrix)
    for (i in 1:length(alpha.seq)){
      print(i)
      alp <- alpha.seq[i]
      for (j in 1:length(beta.seq)){
        bet <- beta.seq[j]
        
        times.matrix <- matrix(0, nrow = nrow(flo.matrix), ncol = ncol(flo.matrix))
        for (i in 1:nrow(flo.matrix)){
          for (j in 1:ncol(flo.matrix)){
            times.matrix[i,j] <- flo.matrix[[i,j]](alpha = alp, beta = bet)
          }
        }
        
        comp.values <- matrix(0, nrow = nrow(flo.matrix), ncol = ncol(flo.matrix))
        for (i in 1:nrow(flo.matrix)){
          for (j in 1:ncol(flo.matrix)){
            comp.values[i,j] <- a[[i,j]](alpha = alp, beta = bet)
          }
        }
        
        fin <- comp.values * times.matrix
        g <- sum(fin, na.rm = T)
        holding <- c(holding, g)
        
        if (g <= min(holding)){
          fin.values <- c(alp, bet)
        }
      }
    }
    
    return(ls = list(min(holding), fin.values))
    ######
  } else {
    num.of.players <- length(variable.limits)
    ##################
    # Gets the sequences for n players
    longest.seq <- length(seq(from = 0, to = max(variable.limits), by = accuracy))
    seq.matrix <- matrix(NA, nrow = num.of.players, ncol = length(seq(from = 0, to = max(variable.limits), by = accuracy)))
    for (i in 1:num.of.players){
      my.seq <- seq(from = 0, to = variable.limits[i], by = accuracy)
      seq.matrix[i,] <- c(my.seq, rep(NA, longest.seq-length(my.seq)))
    }
    ##################
    ###
    if (num.of.players == 3){
      alpha.seq <- as.vector(na.omit(seq.matrix[1,]))
      beta.seq <- as.vector(na.omit(seq.matrix[2,]))
      theta.seq <- as.vector(na.omit(seq.matrix[3,]))
      
      holding <- NULL
      
      a <- Cost_Comp_Flow(cost.matrix, flo.matrix)
      
      for (i in 1:length(alpha.seq)){
        print(i)
        alp <- alpha.seq[i]
        for (j in 1:length(beta.seq)){
          bet <- beta.seq[j]
          for (k in 1:length(theta.seq)){
            thet <- theta.seq[k]
            times.matrix <- matrix(0, nrow = nrow(flo.matrix), ncol = ncol(flo.matrix))
            for (i in 1:nrow(flo.matrix)){
              for (j in 1:ncol(flo.matrix)){
                times.matrix[i,j] <- flo.matrix[[i,j]](alpha = alp, beta = bet, theta = thet)
              }
            }
            
            comp.values <- matrix(0, nrow = nrow(flo.matrix), ncol = ncol(flo.matrix))
            for (i in 1:nrow(flo.matrix)){
              for (j in 1:ncol(flo.matrix)){
                comp.values[i,j] <- a[[i,j]](alpha = alp, beta = bet, theta = thet)
              }
            }
            
            fin <- comp.values * times.matrix
            g <- sum(fin, na.rm = T)
            holding <- c(holding, g)
            
            if (g <= min(holding)){
              fin.values <- c(alp, bet, thet)
            }
          }
        }
      }
      return(ls = list(min(holding), fin.values))
      
    } else if (num.of.players == 4){
      
      alpha.seq <- as.vector(na.omit(seq.matrix[1,]))
      beta.seq <- as.vector(na.omit(seq.matrix[2,]))
      theta.seq <- as.vector(na.omit(seq.matrix[3,]))
      gamma.seq <- as.vector(na.omit(seq.matrix[4,]))
      
      holding <- NULL
      
      a <- Cost_Comp_Flow(cost.matrix, flo.matrix)
      
      for (i in 1:length(alpha.seq)){
        print(i)
        alp <- alpha.seq[i]
        for (j in 1:length(beta.seq)){
          bet <- beta.seq[j]
          for (k in 1:length(theta.seq)){
            thet <- theta.seq[k]
            for (g in 1:length(gamma.seq)){
              gam <- gamma.seq[g]
              times.matrix <- matrix(0, nrow = nrow(flo.matrix), ncol = ncol(flo.matrix))
              for (i in 1:nrow(flo.matrix)){
                for (j in 1:ncol(flo.matrix)){
                  times.matrix[i,j] <- flo.matrix[[i,j]](alpha = alp, beta = bet, theta = thet, gamma = gam)
                }
              }
              
              comp.values <- matrix(0, nrow = nrow(flo.matrix), ncol = ncol(flo.matrix))
              for (i in 1:nrow(flo.matrix)){
                for (j in 1:ncol(flo.matrix)){
                  comp.values[i,j] <- a[[i,j]](alpha = alp, beta = bet, theta = thet, gamma = gam)
                }
              }
              
              fin <- comp.values * times.matrix
              g <- sum(fin, na.rm = T)
              holding <- c(holding, g)
              
              if (g <= min(holding)){
                fin.values <- c(alp, bet, thet, gam)
              }
            }
          }
        }
      }
      return(ls = list(min(holding), fin.values))
      
    } else {
      "Dosen't currently support more than 4 variables, v easy to increase, mainly copy paste, generalisation needs work."
    }
    
  } 
}


############################
      # NASH FLOW #
############################
# Just getting rid of warnings
integral <- function (fun, xmin, xmax, method = c("Kronrod", "Clenshaw", "Simpson"), no_intervals = 8, random = FALSE, reltol = 0.00000001, abstol = 0, ...) {
  stopifnot(is.numeric(xmin), length(xmin) == 1, is.numeric(xmax), 
            length(xmax) == 1)
  no_intervals <- max(1, floor(no_intervals))
  fun <- match.fun(fun)
  f <- function(x) fun(x, ...)
  if (length(f(xmin)) > 1 || length(f(xmax)) > 1) {
    stop("Function 'fun' is array-valued! Use 'quadv'.\n")
  }
  if (length(f(c(xmin, xmax))) != 2) {
    f = Vectorize(f)
  }
  if (xmin == xmax) 
    return(0)
  method <- match.arg(method)
  tol <- if (abstol > 0) 
    min(reltol, abstol)
  else reltol
  if (is.infinite(xmin) || is.infinite(xmax)) {
    cat("For infinite domains Gauss integration is applied!\n")
    Q <- quadinf(f, xmin, xmax, tol = tol)$Q
    return(Q)
  }
  if (random) {
    xs <- c(xmin, (xmax - xmin) * sort(runif(no_intervals - 
                                               1)) + xmin, xmax)
  }
  else {
    xs <- linspace(xmin, xmax, no_intervals + 1)
  }
  Q <- 0
  if (method == "Kronrod") {
    for (i in 1:no_intervals) {
      Q = Q + quadgk(f, xs[i], xs[i + 1], tol = tol)
    }
  }
  else if (method == "Clenshaw") {
    for (i in 1:no_intervals) {
      Q = Q + quadcc(f, xs[i], xs[i + 1], tol = tol)
    }
  }
  else if (method == "Simpson") {
    for (i in 1:no_intervals) {
      Q = Q + simpadpt(f, xs[i], xs[i + 1], tol = tol)
    }
  }
  else {
    stop("Unknown method; not available as integration routine.")
  }
  return(Q)
}

NashIntegral <- function(costs.matrix, flo.matrix, variable.limits, accuracy){
  # Note you must put a variable limit for each variable!
  fin.values <- NULL
  if (length(variable.limits) == 2){
    holding <- NULL
    alpha.seq <- seq(from = 0, to = variable.limits[1], by = accuracy)
    beta.seq <- seq(from = 0, to = variable.limits[2], by = accuracy)
    
    for (i in 1:length(alpha.seq)){
      print(i)
      alp <- alpha.seq[i]
      for (j in 1:length(beta.seq)){
        bet <- beta.seq[j]    
        
        # flow matrix
        flow.hold <- matrix(0, nrow = nrow(flo.matrix), ncol = ncol(flo.matrix))
        for (i in 1:nrow(flo.matrix)){
          for (j in 1:ncol(flo.matrix)){
            flow.hold[i,j] <- flo.matrix[[i,j]](alp, bet)
          }
        }
        
        holding.fin <- matrix(0, nrow = nrow(flo.matrix), ncol = ncol(flo.matrix))
        for (i in 1:nrow(costs.matrix)){
          for (j in 1:ncol(costs.matrix)){
            uppers <- flow.hold[i,j]
            func <- costs.matrix[[i,j]]
            if(is.na(uppers)){
              holding.fin[i,j] <- NA
            } else {
              value <- integral(func, xmin = 0, xmax = uppers)
              holding.fin[i,j] <- value
            }
          }
        }
        
        fin <- holding.fin
        g <- sum(fin, na.rm = T)
        holding <- c(holding, g)
        if (g <= min(holding)){
          fin.values <- c(alp, bet)
        }
      }
    }
    return(ls = list(min(holding), fin.values))
  } else {
    ##################
    # Gets the sequences for n players
    longest.seq <- length(seq(from = 0, to = max(variable.limits), by = accuracy))
    seq.matrix <- matrix(NA, nrow = length(variable.limits), ncol = length(seq(from = 0, to = max(variable.limits), by = accuracy)))
    for (i in 1:length(variable.limits)){
      my.seq <- seq(from = 0, to = variable.limits[i], by = accuracy)
      seq.matrix[i,] <- c(my.seq, rep(NA, longest.seq-length(my.seq)))
    }
    ##################
    if (length(variable.limits) == 3){
      holding <- NULL
      
      alpha.seq <- as.vector(na.omit(seq.matrix[1,]))
      beta.seq <- as.vector(na.omit(seq.matrix[2,]))
      theta.seq <- as.vector(na.omit(seq.matrix[3,]))
      
      print(length(alpha.seq))
      for (i in 1:length(alpha.seq)){
        print(i)
        alp <- alpha.seq[i]
        for (j in 1:length(beta.seq)){
          bet <- beta.seq[j]    
          for (k in 1:length(theta.seq)){
            thet <- theta.seq[k]
            
            
            
            # flow matrix
            flow.hold <- matrix(0, nrow = nrow(flo.matrix), ncol = ncol(flo.matrix))
            for (i in 1:nrow(flo.matrix)){
              for (j in 1:ncol(flo.matrix)){
                flow.hold[i,j] <- flo.matrix[[i,j]](alp, bet, thet)
              }
            }
            
            holding.fin <- matrix(0, nrow = nrow(flo.matrix), ncol = ncol(flo.matrix))
            for (i in 1:nrow(costs.matrix)){
              for (j in 1:ncol(costs.matrix)){
                uppers <- flow.hold[i,j]
                func <- costs.matrix[[i,j]]
                if(is.na(uppers)){
                  holding.fin[i,j] <- NA
                } else {
                  value <- integral(func, xmin = 0, xmax = uppers)
                  holding.fin[i,j] <- value
                }
              }
            }
            
            fin <- holding.fin
            g <- sum(fin, na.rm = T)
            holding <- c(holding, g)
            if (g <= min(holding)){
              fin.values <- c(alp, bet, thet)
            }
          }
        }
      }
      return(ls = list(min(holding), fin.values))
      
      #######
      
    } else if(length(variable.limits) == 4){
      holding <- NULL
      
      alpha.seq <- as.vector(na.omit(seq.matrix[1,]))
      beta.seq <- as.vector(na.omit(seq.matrix[2,]))
      theta.seq <- as.vector(na.omit(seq.matrix[3,]))
      gamma.seq <- as.vector(na.omit(seq.matrix[4,]))
      
      print(length(alpha.seq))
      for (i in 1:length(alpha.seq)){
        print(i)
        alp <- alpha.seq[i]
        for (j in 1:length(beta.seq)){
          bet <- beta.seq[j]    
          for (k in 1:length(theta.seq)){
            thet <- theta.seq[k]
            for (g in 1:length(gamma.seq)){
              gam <- gamma.seq[g]
              
              
              # flow matrix
              flow.hold <- matrix(0, nrow = nrow(flo.matrix), ncol = ncol(flo.matrix))
              for (i in 1:nrow(flo.matrix)){
                for (j in 1:ncol(flo.matrix)){
                  flow.hold[i,j] <- flo.matrix[[i,j]](alp, bet, thet, gam)
                }
              }
              
              holding.fin <- matrix(0, nrow = nrow(flo.matrix), ncol = ncol(flo.matrix))
              for (i in 1:nrow(costs.matrix)){
                for (j in 1:ncol(costs.matrix)){
                  uppers <- flow.hold[i,j]
                  func <- costs.matrix[[i,j]]
                  if(is.na(uppers)){
                    holding.fin[i,j] <- NA
                  } else {
                    value <- integral(func, xmin = 0, xmax = uppers)
                    holding.fin[i,j] <- value
                  }
                }
              }
              
              fin <- holding.fin
              g <- sum(fin, na.rm = T)
              holding <- c(holding, g)
              if (g <= min(holding)){
                fin.values <- c(alp, bet, thet, gam)
              }
            }
          }
        }
      }
      return(ls = list(min(holding), fin.values))
      
    } else {
      "Dosen't currently support more than 4 variables, v easy to increase, mainly copy paste, generalisation needs work."
    }
  }
}


############################
    # EXAMPLES FLOW #
############################
### 2 player ###

cost.matrix <- tibble(s1 = c(function(x){0}, function(x){x^2}), s2 = c(function(x){0}, function(x){(3/2)*x}), a = c(function(x){NA}, function(x){x}))
flo.matrix <- tibble(s1.flow = c(function(alpha,beta){1-alpha}, function(alpha,beta){alpha}), s2.flow = c(function(alpha,beta){1-beta}, function(alpha,beta){beta}), a.flow = c(function(alpha,beta){NA}, function(alpha, beta){1-alpha-beta}))

OptimalFlow(cost.matrix = cost.matrix, flo.matrix = flo.matrix, accuracy = 0.005, variable.limits = c(0.5, 0.5))
NashIntegral(cost.matrix, flo.matrix, variable.limits = c(0.5, 0.5), accuracy = 0.005)

### 3 player ###
costs.matrix <- tibble(s1 = c(function(x){x}, function(x){x}), s2 = c(function(x){x}, function(x){x^2}), s3 = c(function(x){x}, function(x){x^2}), a = c(function(x){NA}, function(x){x}))
flo.matrix <- tibble(s1.flow = c(function(alpha,beta,theta){(3/12)-alpha}, function(alpha,beta,theta){alpha}), s2.flow = c(function(alpha,beta,theta){(4/12)-beta}, function(alpha,beta,theta){beta}), s3.flow = c(function(alpha,beta,theta){(5/12)-theta}, function(alpha,beta,theta){theta}), a = c(function(alpha,beta,theta){NA}, function(alpha,beta,theta){1-alpha-beta-theta}))

OptimalFlow(cost.matrix = cost.matrix, flo.matrix = flo.matrix, accuracy = 0.05, variable.limits = c((3/12), (4/12), (5/12)))
NashIntegral(cost.matrix, flo.matrix, variable.limits = c((3/12), (4/12), (5/12)), accuracy = 0.05)


### 2 player - 2 middle bits ###
cost.matrix <- tibble(s1 = c(function(x){x}, function(x){x}, function(x){x^2}), s2 = c(function(x){x}, function(x){x}, function(x){x}), a = c(function(x){NA}, function(x){NA}, function(x){x^2}), b = c(function(x){NA}, function(x){NA}, function(x){x^2}))
flo.matrix <- tibble(s1.flow = c(function(alpha,beta,theta,gamma){beta}, function(alpha,beta,theta,gamma){(1/4)-alpha-beta}, function(alpha,beta,theta,gamma){alpha}), s2.flow = c(function(alpha,beta,theta,gamma){theta}, function(alpha,beta,theta,gamma){gamma}, function(alpha,beta,theta,gamma){(3/4)-theta-gamma}), a = c(function(alpha,beta,theta,gamma){NA}, function(alpha,beta,theta,gamma){NA}, function(alpha,beta,theta,gamma){NA}), b = c(function(alpha,beta,theta,gamma){NA}, function(alpha,beta,theta,gamma){NA}, function(alpha,beta,theta,gamma){(1/4)-alpha-beta-gamma}))


OptimalFlow(cost.matrix = cost.matrix, flo.matrix = flo.matrix, accuracy = 0.05, variable.limits = c((1/4), (1/4), (3/4), (3/4)))
NashIntegral(cost.matrix, flo.matrix, variable.limits = c((1/4),(1/4),(3/4),(3/4)), accuracy = 0.05)

############################