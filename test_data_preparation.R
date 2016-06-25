# Libraries

libs <- c("data.table")
sapply(libs, library, character.only = T, logical.return = T, 
       quietly = T, warn.conflicts = F)

# Reading data
a <- fread("../test.csv")
#clients <- fread("../cliente_tabla.csv", stringsAsFactors = F)
towns <- fread("../town_state.csv", stringsAsFactors = F)

# Attach clients info
a <- merge(a, towns, all.x = T, by = "Agencia_ID")

# Attach medians from Kaggle scriptt
t <- fread("../submit_med_by_Agency_Client.csv")
a <- merge(a, t, all.x = T, by = "id")
colnames(a)[colnames(a) == "Demanda_uni_equil"] <- "Median"


# Prices
load("f2_reduced.Rdata")
dt[, Price := Venta_hoy/Venta_uni_hoy]
dt[is.na(dt$Price), Price := Dev_proxima/Dev_uni_proxima]
dt[is.na(dt$Price), Price := 0]
pairs <- dt[, .(Producto_ID, Price)]
pairs <- unique(pairs)
pairs <- pairs[,.(Prezzo = median(Price, na.rm = T)), by = Producto_ID]

a <- merge(a, pairs, all.x = T, by = "Producto_ID")
a[is.na(a$Prezzo),]$Prezzo <- 0

save(a, file = "test_prepared.RData")