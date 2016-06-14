metric <- function(pred, target){
  if(length(pred) != length(target)) stop("Different length of vectors")
  e <- sqrt(sum(((log(pred + 1) - log(target + 1))^2))/length(pred))
  return(e)
}