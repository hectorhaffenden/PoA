Composite <- function(f, g) {
  force(f)
  force(g)
  function(...) f(g(...))
}