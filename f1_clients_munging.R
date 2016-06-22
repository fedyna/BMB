# Libraries
library(data.table)

# Reading data. Instead of recuced data we have to use full train data set.
load("reduced.Rdata")
clients <- read.csv("../cliente_tabla.csv", stringsAsFactors = F)
towns <- read.csv("../town_state.csv", stringsAsFactors = F)

dt <- setDT(a)
dt <- merge(dt, clients, all.x = T, by = "Cliente_ID")
dt <- merge(dt, towns, all.x = T, by = "Agencia_ID")

save(dt, file = "f1_reduced.RData")
