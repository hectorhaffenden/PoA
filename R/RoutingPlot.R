RoutingPlot <- function(new_input_plot){
  
  gtest <- graph(strsplit(paste0(new_input_plot$from," ", new_input_plot$to, collapse = " "), split = " ")[[1]])
  
  labels_cost_and_flow <- paste0(new_input_plot$flow, " \n c(x) = " ,new_input_plot$cost)
  
  plot(gtest,
       edge.arrow.size=.4, edge.curved=.1, vertex.size = 20,
       edge.label	= labels_cost_and_flow,
       edge.arrow.size = 5,
       edge.arrow.width	= 3,
       edge.width = 2,
       layout = layout_with_fr(gtest))
}