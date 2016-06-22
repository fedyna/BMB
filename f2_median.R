# Libraries
library(data.table)

# Reading data. Instead of reduced data we have to use full train data set.
#Preparation
load("f1_reduced.Rdata")
dt <- dt[, `:=` (Cliente_ID = as.double(Cliente_ID), 
                 Producto_ID = as.double(Producto_ID), 
                 Demanda_uni_equil  = as.double(Demanda_uni_equil))]
setkey(dt, Producto_ID, Cliente_ID)

#Calculation medians
median <- dt[, median(Demanda_uni_equil)]
median_Prod <- dt[, median(Demanda_uni_equil), by = Producto_ID]
setnames(median_Prod,"V1","M2")
median_Client_Prod <- dt[, as.double(median(Demanda_uni_equil)),
                         by = .(Producto_ID,Agencia_ID,Cliente_ID)]
setnames(median_Client_Prod,"V1","M3")

#Merging medians with data set
setkey(dt, Producto_ID, Agencia_ID, Cliente_ID)
dt <- merge(dt, median_Client_Prod, all.x = TRUE)
dt$M2 <- merge(dt, median_Prod, by = "Producto_ID", all.x = TRUE)$M2
dt$Median <- dt$M3
#DO WE NEED "ROUND" HERE?????
dt[is.na(M3)]$Median <- round(dt[is.na(M3)]$M2)
dt[is.na(Median)]$Median <- round(median)

dt <- dt[ ,`:=`(M3 = NULL, M2 = NULL)]

save(dt, file = "f2_reduced.RData")
