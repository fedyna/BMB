metric <- function(pred, target){
    if(length(pred) != length(target)) stop("Different length of vectors")
    e <- sqrt(mean((log(pred + 1) - log(target + 1))^2))
    return(e)
}