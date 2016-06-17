# Libraries
libs <- c("dplyr", "ggplot2", "caret", "reshape2", "data.table",
          "forecast")
sapply(libs, library, character.only = T, logical.return = T, 
       quietly = T, warn.conflicts = F)

# Uploading small data
load("reduced.Rdata")

# Just convert to data table
################################ ATTENTION!!!! ################################
# Use fread in case of full data set!
# dt <- fread('train.csv', 
        #colClasses=c(Cliente_ID="numeric",
        #Producto_ID="numeric",Demanda_uni_equil="numeric"))
dt <- setDT(a)
# Colums to integer and setting keys
dt <- dt[, `:=` (Cliente_ID = as.double(Cliente_ID), 
                 Producto_ID = as.double(Producto_ID), 
                 Demanda_uni_equil  = as.double(Demanda_uni_equil))]
setkey(train, Producto_ID, Cliente_ID)

train <- dt[Semana != 9]
test <- dt[Semana == 9]


############################ FIRST FEATURE - MEDIAN ###########################
# From Kaggle script
median <- train[, median(Demanda_uni_equil)]
median_Prod <- train[, median(Demanda_uni_equil), by = Producto_ID]
setnames(median_Prod,"V1","M2")
median_Client_Prod <- train[, as.double(median(Demanda_uni_equil)), 
                            by = .(Producto_ID, Cliente_ID)]
setnames(median_Client_Prod,"V1","M3")
submit <- merge(test, median_Client_Prod, all.x = TRUE)
submit$M2 <- merge(test, median_Prod, by = "Producto_ID", all.x = TRUE)$M2
submit[is.na(M3)]$Pred <- submit[is.na(M3)]$M2
submit[is.na(Pred)]$Pred <- median

# Let us see error
e <- sqrt(mean((log(submit$Pred + 1) - log(submit$Demanda_uni_equil + 1))^2))
# 0.68743

############################ SECOND FEATURE - PRICE ##########################

train[, Price := Venta_hoy/Venta_uni_hoy]
# Let us check do all products numbers have unique price.
num.of.products <- length(unique(train$Producto_ID))
pairs <- train[, .(Producto_ID, Price)]
pairs <- unique(pairs)
num.of.pairs <- nrow(pairs)
num.of.pairs == num.of.products # !!!! FALSE !!!!!!!
# OOOOOOOHHHHHHHH!!!!!!!! let's adjust it.

pairs <- pairs[,.(Prezzo = median(Price, na.rm = T)), by = Producto_ID]
train <- merge(train, pairs, by = "Producto_ID", all.x = T)
train <- train[, Price := NULL]
test <- merge(test, pairs, by = "Producto_ID", all.x = T)




