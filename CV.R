# Uploading libraries
libs <- c("data.table")
sapply(libs, library, character.only = T, logical.return = T, 
       quietly = T, warn.conflicts = F)

#load("reduced.Rdata")
a <- fread("train.csv")

metric <- function(pred, target){
    if(length(pred) != length(target)) stop("Different length of vectors")
    e <- sqrt(sum(((log(pred + 1) - log(target + 1))^2))/length(pred))
    return(e)
}

error <- rep(0, length(unique(a$Semana)))
time <- rep(0, length(unique(a$Semana)))

for(i in 1:length(unique(a$Semana))){
    
    # Split data set
    ind <- which(a$Semana == i+2) # Because week are in 3:9, not 1:7
    train <- a[-ind,]
    test <- a[ind,]
    
    # Model building
    t <- Sys.time()
    fit <- lm(a$Demanda_uni_equil ~ Agencia_ID + 
                  Canal_ID + 
                  Ruta_SAK + 
                  Cliente_ID + 
                  Producto_ID, data = a)
    
    
    # Prediction
    answers <- predict(fit, test)
    answers[which(answers < 0)] <- 0
    error[i] <- metric(answers, test$Demanda_uni_equil)
    time[i] <- Sys.time() - t
    
    print(i) # For progress control
    
}

summary(error)
summary(time)

