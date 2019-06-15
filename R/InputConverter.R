InputConverter <- function(new_input, new_variables_flow){
  # For example, inputs are like this,
  # new_variables_flow <- c("alpha", "beta")
  # new_input <- tibble(cost = c("0", "0", "NA", "x^2", "(3/2)*x", "x"),
  #      flow = c("(1/2)-alpha", "(1/2)-beta", "NA", "alpha", "beta", "1-alpha-beta"))
  
  cost_functions <- paste0("function(x){", new_input$cost, "}") %>%
    tibble()
  cost_fucntins_good_form <- apply(cost_functions, 1, function(x){eval(parse(text=x))})
  
  vars__in_one_string <- paste0(new_variables_flow, collapse = ", ")
  
  flow_functions <- new_input$flow %>%
    paste0("function(", vars__in_one_string, "){", ., "}") %>%
    tibble()
  flow_functions_good_form <- apply(flow_functions, 1, function(x){eval(parse(text=x))})
  players_done <- tibble(cost = cost_fucntins_good_form, flow = flow_functions_good_form)
  return(players_done)
}