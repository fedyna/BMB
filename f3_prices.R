# Libraries
libs <- c("data.table")
sapply(libs, library, character.only = T, logical.return = T, 
       quietly = T, warn.conflicts = F)

# Uploading small data
load("f2_reduced.Rdata")

dt[, Price := Venta_hoy/Venta_uni_hoy]
dt[is.na(dt$Price), Price := Dev_proxima/Dev_uni_proxima]
dt[is.na(dt$Price), Price := 0]
# Let us check do all products numbers have unique price.
#num.of.products <- length(unique(dt$Producto_ID))#DO NOT RUN ON BIG SET
pairs <- dt[, .(Producto_ID, Price)]
pairs <- unique(pairs)
#num.of.pairs <- nrow(pairs)#DO NOT RUN ON BIG SET
#num.of.pairs == num.of.products # !!!! FALSE !!!!!!!#DO NOT RUN ON BIG SET
# OOOOOOOHHHHHHHH!!!!!!!! let's adjust it.

pairs <- pairs[,.(Prezzo = median(Price, na.rm = T)), by = Producto_ID]
dt <- merge(dt, pairs, by = "Producto_ID", all.x = T)
dt <- dt[, Price := NULL]

save(dt, file = "f3_reduced.RData")





