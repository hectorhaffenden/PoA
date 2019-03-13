#' Compoute the Price of Anarchy for a Routing Game.
#'
#' `PriceOfAnarchy()` returns Price of Anarchy (PoA) for a routing game.
#' This is a routing game solver for a generic network.
#' For this to work properly, set the inputs as shown in the examples.
#' This is as a tibble, with one column containing the cost functions,
#' and another containing the flow.
#'
#'
#' @name PriceOfAnarchy
#'
#'
#'
#' @param cost.and.flow.tibble A tibble with 2 columns, cost which contains the cost
#'  functions, and flow containing the respective flows. When defining the flow functions,
#'   use all variables as inputs for each function.
#' @param variable.limits A vector of limits for each variable, described by the proportions
#'  at each source node.
#' @return Returns the Price of Anarchy, Optimal cost and the respective parameters,
#' Nash cost and the respective parameters.
#'
#' Requires tidyverse, pracma and nloptr packacges
#'
#'
#'
#' @examples
#' library(stats, "na.omit")
#' library(dplyr)
#' library(tibble)
#' library(pracma)
#' library(nloptr)
#'
#' ### 2 player game ###
#' player2 <- tibble(cost = c(function(x){0}, function(x){0}, function(x){NA},
#'                  function(x){x^2}, function(x){(3/2)*x},  function(x){x}),
#'                   flow = c(function(alpha,beta){(1/2)-alpha},function(alpha,beta){(1/2)-beta},
#'                     function(alpha,beta){NA},function(alpha,beta){alpha},
#'                     function(alpha,beta){beta},function(alpha, beta){1-alpha-beta}))
#' PriceOfAnarchy(player2, c((1/2), (1/2)))
#' \dontrun{
#' ### 3 player - 2x2 middle ###
#' player3middle2x2 <- tibble(cost = c(function(x){0}, function(x){x},function(x){x^2},
#' function(x){x},function(x){x},function(x){0},function(x){x},function(x){x^2},
#' function(x){x^2},function(x){x},function(x){(1/2)*x},function(x){x}),
#'                           flow = c(function(d1,d2,d3,d4,d5){d1},
#'                           function(d1,d2,d3,d4,d5){1-d1},function(d1,d2,d3,d4,d5){d2},
#'                           function(d1,d2,d3,d4,d5){1-d2},function(d1,d2,d3,d4,d5){d3},
#'                           function(d1,d2,d3,d4,d5){1-d3},function(d1,d2,d3,d4,d5){d4},
#'                           function(d1,d2,d3,d4,d5){d1+d2+d3-d4},function(d1,d2,d3,d4,d5){d5},
#'                           function(d1,d2,d3,d4,d5){1-d1-d2-d3-d5},
#'                           function(d1,d2,d3,d4,d5){d4+d5},function(d1,d2,d3,d4,d5){1-d4-d5}))
#' PriceOfAnarchy(player3middle2x2, c((6/10),(1/10),(3/10),1,1))
#' }
#' @importFrom stats na.omit
#' @import dplyr
#' @import tibble
#' @import pracma
#' @import nloptr
#'
#' @export

### GT Efficient no matrix
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
  price.of.anarchy <- real.nash.value / S$value
  return(list(PoA = price.of.anarchy, optimal.value = S$value, optimal.pars = S$par, nash.value = real.nash.value, nash.pars = N$par))
}


