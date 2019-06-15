PriceOfAnarchy <- function(new_input, new_variables_flow, variable.limits){
  cost.and.flow.tibble <- InputConverter(new_input, new_variables_flow)
  # Build cost and flow tibble with the cost column containing all costs, and flow with the respective flows.
  # When building flow colum, use every variable in each function, even if the function dosen't call it.
  data <- cost.and.flow.tibble %>%
    mutate(cost.flow.comp = mapply(Composite, cost.and.flow.tibble$cost, cost.and.flow.tibble$flow))
  # Solution - Using Nash and Optimal
  S <- bobyqa(c(rep(0,length(variable.limits))), Optimal, lower = c(rep(0,length(variable.limits))), upper = variable.limits, data = data)
  N <- bobyqa(c(rep(0,length(variable.limits))), Nash, lower = c(rep(0,length(variable.limits))), upper = variable.limits, data = data)
  real.nash.value <- Optimal(N$par, data = data)
  price.of.anarchy <- real.nash.value / S$value
  return(list(PoA = price.of.anarchy, optimal.value = S$value, optimal.pars = S$par, nash.value = real.nash.value, nash.pars = N$par))
}