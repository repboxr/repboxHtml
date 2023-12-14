is.true = function(x) {
  val = x == TRUE
  val[is.na(x)] = FALSE
  val
}

na.val = function(x, val=0) {
  x[is.na(x)] = val
  x
}
