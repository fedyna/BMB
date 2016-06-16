# Uploading libraries
libs <- c("data.table", "xgboost", "rpart", "randomForest")
sapply(libs, library, character.only = T, logical.return = T, 
       quietly = T, warn.conflicts = F)

load("reduced.Rdata")
#a <- fread("train.csv")

metric <- function(pred, target){
    if(length(pred) != length(target)) stop("Different length of vectors")
    e <- sqrt(mean((log(pred + 1) - log(target + 1))^2))
    return(e)
}

error <- rep(0, length(unique(a$Semana)))
time <- rep(0, length(unique(a$Semana)))
a <- a[ , -c(7:10)]

t <- Sys.time()
for(i in 1:length(unique(a$Semana))){
    
    # Split data set
    ind <- which(a$Semana == i+2) # Because week are in 3:9, not 1:7
    trTrain <- a[-ind,]
    trTest <- a[ind,]
    
    # Model building
    fit <- randomForest(Demanda_uni_equil ~ ., trTrain, ntree = 500)
    
    # Prediction and evaluation
    answers <- predict(fit, trTest)
    answers[which(answers < 0)] <- 0
    error[i] <- metric(answers, trTest$Demanda_uni_equil)
    
    print(i) # For progress control
    
}

summary(error)
time <- Sys.time() - t
time



############################# TRIES ############################# 

####################### Linear regression #######################
fit <- lm(Demanda_uni_equil ~ Agencia_ID + 
              Canal_ID + 
              Ruta_SAK + 
              Cliente_ID + 
              Producto_ID, data = trTrain)

############################ XGBoost ########################### 
# Preparation for xgboost
train.matrix = as.matrix(trTrain)
mode(train.matrix) = "numeric"
test.matrix = as.matrix(trTest)
mode(test.matrix) = "numeric"
watchlist <- list(train=trTrain, test=trTest)

# Model building
fit <- xgboost(data = train.matrix[,1:6], 
               label = train.matrix[,7], 
               booster = "gbtree",
               objective = "reg:linear",
               watchlist=watchlist,
               nround = 5)

# Prediction and evaluation
answers <- predict(fit, test.matrix[,1:6])
answers[which(answers < 0)] <- 0
error[i] <- metric(answers, trTest$Demanda_uni_equil)

############################# rpart ############################
fit <- rpart(Demanda_uni_equil ~., data = trTrain, method="anova")

