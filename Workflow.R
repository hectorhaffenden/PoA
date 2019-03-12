### GT Efficient no matrix
library(tidyverse) # Tibbles etc
library(pracma) # Used for integrals
options(scipen=999)

cost.and.flow.tib <- tibble(cost = c(function(x){0}, function(x){0}, function(x){NA}, function(x){x^2}, function(x){(3/2)*x},  function(x){x}),
                      flow = c(function(alpha,beta){(1/2)-alpha},function(alpha,beta){(1/2)-beta},  function(alpha,beta){NA}, function(alpha,beta){alpha},  function(alpha,beta){beta},
                               function(alpha, beta){1-alpha-beta}))
variable.lim <- c(0.5,0.5)
PriceOfAnarchy <- function(cost.and.flow.tibble, variable.limits){
  # Build cost and flow tibble with the cost column containing all costs, and flow with the respective flows.
  # When building flow colum, use every variable in each function, even if the function dosen't call it.
  #
  #
  #
  #
  Composite <- function(f, g) {
    force(f)
    force(g)
    function(...) f(g(...))
  }
  ###########
  # Optimal #
  ###########
  # Inputs
  data <- cost.and.flow.tibble %>% 
    mutate(cost.flow.comp = mapply(Composite, cost.and.flow.tibble$cost, cost.and.flow.tibble$flow))
  
  
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
  
  ###########
  #  Nash   #
  ###########
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
  
  
  data <- cost.and.flow.tibble %>% 
    mutate(cost.flow.comp = mapply(Composite, cost.and.flow.tibble$cost, cost.and.flow.tibble$flow))
  
  
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
        data$intergralss[[i]] <- integral(data$cost[[i]], xmin = 0, xmax = flowss.raw[i])
      }
    }
    hold <- NULL
    for (i in 1:nrow(data)){
      hold <- c(hold, do.call(compss[[i]], xs[1:length(variable.limits)]))
    }
    return(sum(unlist(data$intergralss)))
  }

  N <- bobyqa(c(rep(0,length(variable.limits))), Nash, lower = c(rep(0,length(variable.limits))), upper = variable.limits)
  S <- bobyqa(c(rep(0,length(variable.limits))), Optimal, lower = c(rep(0,length(variable.limits))), upper = variable.limits)
  real.nash.value <- Optimal(N$par)
  
  price.of.anarchy <- S$value / real.nash.value
  return(list(PoA = price.of.anarchy, optimal.value = S$value, optimal.pars = S$par, nash.value = real.nash.value, nash.pars = N$par))
}
PriceOfAnarchy(cost.and.flow.tib, variable.lim)
