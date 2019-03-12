### GT Efficient
library(tidyverse) # Tibbles etc
library(pracma) # Used for integrals
options(scipen=999)
PriceOfAnarchy <- function(cost.and.flow.tibble, variable.limits){
  # Build cost and flow tibble with the cost column containing all costs, and flow with the respective flows.
  # When building flow colum, use every variable in each function, even if the function dosen't call it.
  #
  #
  #
  # function to compost costs and flows
  Composite <- function(f, g) {
    force(f)
    force(g)
    function(...) f(g(...))
  }
  # Inputs
  data <- cost.and.flow.tibble %>% 
    mutate(cost.flow.comp = mapply(Composite, cost.and.flow.tibble$cost, cost.and.flow.tibble$flow))
  # Optimal Solver
  Optimal <- function(x){
    compss <- data$cost.flow.comp
    flowss <- data$flow
    
    
    n <- length(variable.limits)
    txt <- sprintf("list(%s)", toString(paste0("x[", 1:n, "]")))
    cobj <- parse(text = txt)[[1]]
    xs <- eval(cobj)
    
    hold <- NULL
    for (i in 1:nrow(data)){
      hold <- c(hold, do.call(compss[[i]], xs[1:length(variable.limits)]) * do.call(flowss[[i]], xs[1:length(variable.limits)]))
    }
    return(sum(na.omit(hold)))
  }
  # Nash Solver
  Nash <- function(x){
    costss <- data$cost
    compss <- data$cost.flow.comp
    flowss <- data$flow
    
    n <- length(variable.limits)
    txt <- sprintf("list(%s)", toString(paste0("x[", 1:n, "]")))
    cobj <- parse(text = txt)[[1]]
    xs <- eval(cobj)
    
    # Making uppers
    flowss.raw <- NULL
    for (i in 1:length(flowss)){
      flowss.raw <- c(flowss.raw, do.call(flowss[[i]], xs[1:length(variable.limits)]))
    }
    data$intergralss <- costss
    for (i in 1:nrow(data)){
      if (is.na(flowss.raw[i])){
        data$intergralss[[i]] <- 0
      } else {
        data$intergralss[[i]] <- integral(Vectorize(data$cost[[i]]), xmin = 0, xmax = flowss.raw[i])
      }
    }
    return(sum(unlist(data$intergralss)))
  }
  # Solution - Using Nash and Optimal
  S <- bobyqa(c(rep(0,length(variable.limits))), Optimal, lower = c(rep(0,length(variable.limits))), upper = variable.limits)
  N <- bobyqa(c(rep(0,length(variable.limits))), Nash, lower = c(rep(0,length(variable.limits))), upper = variable.limits)
  real.nash.value <- Optimal(N$par)
  price.of.anarchy <- S$value / real.nash.value
  return(list(PoA = price.of.anarchy, optimal.value = S$value, optimal.pars = S$par, nash.value = real.nash.value, nash.pars = N$par))
}



############################
#        EXAMPLES          #
############################
### 2 player ###
player2 <- tibble(cost = c(function(x){0}, function(x){0}, function(x){NA}, function(x){x^2}, function(x){(3/2)*x},  function(x){x}),
                  flow = c(function(alpha,beta){(1/2)-alpha},function(alpha,beta){(1/2)-beta},  function(alpha,beta){NA}, function(alpha,beta){alpha},  function(alpha,beta){beta},
                           function(alpha, beta){1-alpha-beta}))

PriceOfAnarchy(player2, c((1/2), (1/2)))

## Extra 1 - Changes the variable limit for beta
hol <- NULL
my_seq <- seq(0.12, (0.22), by=0.001)
for (i in my_seq){
  f1 <- PriceOfAnarchy(player2, c((1/2), i))$PoA
  hol <- c(hol, f1)
  
}
plot(hol, x = my_seq, main = "Change in PoA given different starting proportions",
     xlab = "Different set of parameters",
     ylab = "PoA",
     type = "b")

# Extra 2 - Changes coeficient for alpha flow
hol2 <- NULL
my_seq <- seq(0, (1), by=0.01)
for (i in my_seq){
  player2 <- tibble(cost = c(function(x){0}, function(x){0}, function(x){NA}, function(x){x^2}, function(x){(3/2)*x},  function(x){x}),
                    flow = c(function(alpha,beta){(1/2)-alpha},function(alpha,beta){(1/2)-beta},  function(alpha,beta){NA}, function(alpha,beta){(i^2)*alpha},  function(alpha,beta){beta},
                             function(alpha, beta){1-alpha-beta}))
  f1 <- PriceOfAnarchy(player2, c((1/2), (1/2)))$PoA
  hol2 <- c(hol2, f1)
  
}
plot(hol, x = my_seq, main = "Change in PoA given different coeficient for alpha",
     xlab = "Different set of parameters",
     ylab = "PoA",
     type = "b")


### 3 player ###
player3 <- tibble(cost = c(function(x){x}, function(x){x}, function(x){x}, function(x){x^2}, function(x){x}, function(x){x^2}, function(x){NA}, function(x){x}),
                  flow = c(function(alpha,beta,theta){(3/12)-alpha}, function(alpha,beta,theta){alpha}, function(alpha,beta,theta){(4/12)-beta}, function(alpha,beta,theta){beta}, function(alpha,beta,theta){(5/12)-theta}, function(alpha,beta,theta){theta}, function(alpha,beta,theta){NA}, function(alpha,beta,theta){1-alpha-beta-theta}))


PriceOfAnarchy(player3, c((3/12), (4/12), (5/12)))





### 3 player - 2x2 middle ###
player3middle2x2 <- tibble(cost = c(function(x){0}, function(x){x},function(x){x^2},function(x){x},function(x){x},function(x){0},function(x){x},function(x){x^2},function(x){x^2},function(x){x},function(x){(1/2)*x},function(x){x}),
                           flow = c(function(d1,d2,d3,d4,d5){d1},function(d1,d2,d3,d4,d5){1-d1},function(d1,d2,d3,d4,d5){d2},function(d1,d2,d3,d4,d5){1-d2},function(d1,d2,d3,d4,d5){d3},function(d1,d2,d3,d4,d5){1-d3},function(d1,d2,d3,d4,d5){d4},function(d1,d2,d3,d4,d5){d1+d2+d3-d4},function(d1,d2,d3,d4,d5){d5},function(d1,d2,d3,d4,d5){1-d1-d2-d3-d5},function(d1,d2,d3,d4,d5){d4+d5},function(d1,d2,d3,d4,d5){1-d4-d5}))



PriceOfAnarchy(player3middle2x2, c((6/10),(1/10),(3/10),1,1))



