submission <- function(pred, sub.name){
  if(length(pred) != 6999251) stop("Wrong length")
  answer <- data.frame(id = seq(0, 6999250), Demanda_uni_equil = pred)
  write.csv(answer, paste(sub.name, ".csv", sep = ""), row.names = F)
}