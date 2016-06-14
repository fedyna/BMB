
# Libraries
libs <- c("dplyr", "ggplot2", "caret", "reshape2", "data.table",
          "forecast")
sapply(libs, library, character.only = T, logical.return = T, 
       quietly = T, warn.conflicts = F)

# Fast reading - thank you, data.table
a <- fread("train.csv")

## Let's reduce size of data for easy 
inTrain <- createDataPartition(a$Semana, p=0.01, list = FALSE)
a <- a[inTrain,] # 741807 lines
save(a, file = "reduced.RData")

## Reduced dataset can be exchanged for complete one.
load(file = "reduced.RData")

##################################################################
####################### TRENDS BY PRODUCT ########################
##################################################################

#can be converted to trenbs by whatever

# Find six biggest
s <- a %>% group_by(Producto_ID) %>% summarise(SUM = sum(Venta_uni_hoy))
ind <- head(s[order(s$SUM, decreasing = T),])$Producto_ID

# Subset and aggregate
xx <- a[which(a$Producto_ID %in% ind),]
xx <- aggregate(xx, by = list(xx$Semana, xx$Producto_ID), sum)
xx <- xx[,c(1,2,9)]
xx$Group.2 <- as.factor(xx$Group.2)

# Plotting
ggplot(data=xx, 
       aes(x = Group.1, y = Venta_uni_hoy, colour = Group.2)) + 
    geom_line() 

##################################################################
######################## AUTOCORRELATION #########################
##################################################################

# Six biggest sales
x <- data.frame(cbind(xx[1:7,3], 
                      xx[8:14,3], 
                      xx[15:21,3], 
                      xx[22:28,3], 
                      xx[29:35,3], 
                      xx[36:42,3]))
Acf(x[,1])
Acf(x[,2])
Acf(x[,3])
Acf(x[,4])
Acf(x[,5])
Acf(x[,6]) #NO AUTOCORRELATION AT ALL!!

##################################################################
################## SEVERAL ROUTES = AGENCIA_ID ###################
##################################################################

# one route per one agencia?
routes <- unique(a$Ruta_SAK)
l <- length(routes)
r <- rep(NA, l)
for(i in 1:l){r[i] <- length(unique(a$Agencia_ID[which(a$Ruta_SAK == routes[i])]))}
summary(r) # NOT AT ALL!!

# one route per oner canal?
r <- rep(NA, l)
for(i in 1:l){r[i] <- length(unique(a$Canal_ID[which(a$Ruta_SAK == routes[i])]))}
summary(r)
length(r[which(r>1)]) # Almost. Only 169 out of 2208 are bigger than 1. Max = 4

#summary(r) (for all 75 mln rows)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   1.000   2.000   7.661   7.000 154.000 
