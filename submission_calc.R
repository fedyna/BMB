libs <- c("xgboost", "Matrix", "data.table", "caret")
sapply(libs, library, character.only = T, logical.return = T,
       quietly = T, warn.conflicts = F)

############################ 1% ############################
# load("f3_reduced.Rdata")
############################################################

############################ n% ############################
set.seed(8310)
n = 100
dt <- fread("../BMB/train.csv")
inTrain <- createDataPartition(dt$Semana, p=n/100, list = FALSE)
dt <- dt[inTrain,]

# Add clients
towns <- read.csv("../BMB/town_state.csv", stringsAsFactors = F)
dt <- merge(dt, towns, all.x = T, by = "Agencia_ID")

# Median
dt <- dt[, `:=` (Cliente_ID = as.double(Cliente_ID), 
                 Producto_ID = as.double(Producto_ID), 
                 Demanda_uni_equil  = as.double(Demanda_uni_equil))]
setkey(dt, Producto_ID, Cliente_ID)
median <- dt[, median(Demanda_uni_equil)]
median_Prod <- dt[, median(Demanda_uni_equil), by = Producto_ID]
setnames(median_Prod,"V1","M2")
median_Client_Prod <- dt[, as.double(median(Demanda_uni_equil)),
                         by = .(Producto_ID,Agencia_ID,Cliente_ID)]
setnames(median_Client_Prod,"V1","M3")
setkey(dt, Producto_ID, Agencia_ID, Cliente_ID)
dt <- merge(dt, median_Client_Prod, all.x = TRUE)
dt$M2 <- merge(dt, median_Prod, by = "Producto_ID", all.x = TRUE)$M2
dt$Median <- dt$M3
dt[is.na(M3)]$Median <- round(dt[is.na(M3)]$M2)
dt[is.na(Median)]$Median <- round(median)
dt <- dt[ ,`:=`(M3 = NULL, M2 = NULL)]

#Prices 
dt[, Price := Venta_hoy/Venta_uni_hoy]
dt[is.na(dt$Price), Price := Dev_proxima/Dev_uni_proxima]
dt[is.na(dt$Price), Price := 0]
pairs <- dt[, .(Producto_ID, Price)]
pairs <- unique(pairs)
pairs <- pairs[,.(Prezzo = median(Price, na.rm = T)), by = Producto_ID]
dt <- merge(dt, pairs, by = "Producto_ID", all.x = T)
dt <- dt[, Price := NULL]




######################## Calculation ########################

dt <- dt[ ,`:=`(Venta_uni_hoy = NULL, Venta_hoy = NULL,
                Dev_uni_proxima = NULL, Dev_proxima = NULL,
                Town =NULL)]

dt$State <- as.factor(dt$State)
dt$Canal_ID <- as.factor(dt$Canal_ID)

label <- dt$Demanda_uni_equil
dt <- dt[ ,`:=`(Demanda_uni_equil = NULL)]
dt <- sparse.model.matrix( ~ . - 1, data = dt, row.names = F)

fit <- xgboost(data = dt, 
               label = label, 
               booster = "gbtree",
               objective = "reg:linear",
               nround = 5,
               verbose = 1)

# Reading and preparing test set
load("test_prepared.RData")
a <- a[ ,`:=`(Town =NULL)]
a$State <- as.factor(a$State)
a$Canal_ID <- as.factor(a$Canal_ID)
id <- a$id
a <- a[ ,`:=`(id = NULL)]
a <- sparse.model.matrix( ~ . - 1, data = a, row.names = F)

# Prediction
answers <- predict(fit, a)

# Preparing submission and write on disc
answers <- data.frame(id = id, Demanda_uni_equil = answers)
answers <- answers[order(answers$id),]
write.csv(answers, 
          paste(as.character(n), "%_submission.csv", sep = ""), 
          row.names = F)


