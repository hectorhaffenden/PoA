Optimal <- function(x, data){
  
  compss <- data$cost.flow.comp
  flowss <- data$flow
  
  n <-  length(formals(data$flow[[1]]))
  txt <- sprintf("list(%s)", toString(paste0("x[", 1:n, "]")))
  cobj <- parse(text = txt)[[1]]
  xs <- eval(cobj)
  
  hold <- NULL
  for (i in 1:nrow(data)){
    hold <- c(hold, do.call(compss[[i]], xs[1:n]) * do.call(flowss[[i]], xs[1:n]))
  }
  
  return(sum(na.omit(hold)))
}