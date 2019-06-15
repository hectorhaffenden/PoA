Nash <- function(x, data){
  costss <- data$cost
  compss <- data$cost.flow.comp
  flowss <- data$flow
  
  n <-  length(formals(data$flow[[1]]))
  txt <- sprintf("list(%s)", toString(paste0("x[", 1:n, "]")))
  cobj <- parse(text = txt)[[1]]
  xs <- eval(cobj)
  
  # Making uppers
  flowss.raw <- NULL
  for (i in 1:length(flowss)){
    flowss.raw <- c(flowss.raw, do.call(flowss[[i]], xs[1:n]))
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