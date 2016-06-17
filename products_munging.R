
products <- read.csv("../producto_tabla.csv", stringsAsFactors = F)

# Last numbers in NombreProducto look like Producto_ID
# Let's check
last.numbers <- sapply(products$NombreProducto, 
                       function(x) tail(unlist(strsplit(x, " ")), 1))
last.numbers <- as.integer(last.numbers)
sum(last.numbers - products$Producto_ID) 
# ZERO! So we can just remove all those last numbers
products$NombreProducto <- gsub(' [0-9]+$', '', products$NombreProducto)

# Now let us have a look on those capital letters at the end of lines
# We put them into separate column and remove from the line
products$CAP.LETTES <- as.character(regmatches(products$NombreProducto, 
                                               gregexpr("[[:upper:]* [[:upper:]]*$", 
                                                        products$NombreProducto)))
# Remove leading space
products$CAP.LETTES <- sapply(products$CAP.LETTES, 
                              function (x)  sub("^\\s+", "", x))
# Remove from NombreProducto
products$NombreProducto <- gsub("[[:upper:]* [[:upper:]]*$", "",
                                products$NombreProducto)


# OK, now let us separate numbers and its measurements
# We use as.character to unlist result of regmatches

# Here we read all numbers followed by letters
k <- regmatches(products$NombreProducto, 
                gregexpr("[0-9]+[a-z]+", products$NombreProducto))
# And remove all numbers
k <- gsub("[0-9]+", "", k)
# And finally have a look on what we have
unique(k)



products$Weights <- as.character(regmatches(products$NombreProducto,
                             gregexpr("[0-9]+g|[0-9]+Kg|
                                      [0-9]+kg|[0-9]+gProm|[0-9]+oz",
                                      products$NombreProducto)))


# Now let us separate measurements
products$Weights.measure <- gsub("[0-9]+", "", products$Weights)

unique(products$Weights.measure)
# Dirty result! Let us clean it step by step
##1
products$Weights.measure[which(products$Weights.measure == "gProm")] <- "g"
##2
products$Weights.measure[which(products$Weights.measure == "Kg")] <- "kg"
##3
products[which(products$Weights.measure == "c(\"g\", \"g\")"),]
#412       30302 Tostado 210g y Cajeta Quemada 18g        BIM c(" 210g", " 18g")     c("g", "g")
#obviously, Watson!
products[which(products$Weights.measure == 
                   "c(\"g\", \"g\")"),]$Weights <- "210g"
products[which(products$Weights.measure == 
                   "c(\"g\", \"g\")"),]$Weights.measure <- "g"
##4
products[which(products$Weights.measure == "c(\"g\", \"oz\")"),]
#163    2575 Vasos 226 8g8oz        NES c("8g", "8oz")    c("g", "oz")
#nor so obvious, Watson! Let it be grams
products[which(products$Weights.measure == 
                   "c(\"g\", \"oz\")"),]$Weights <- "8g"
products[which(products$Weights.measure == 
                   "c(\"g\", \"oz\")"),]$Weights.measure <- "g"
##5
products$Weights.measure[1] <- "g"
products$Weights[1] <- 0


nrow(products[which(products$Weights.measure == "character()"),])
# [1] 64 !!!!! What are they?
products[which(products$Weights.measure == "character()"),]

#### WHY THERE IS "1kg" LEFT????!!
#### THE EXPRESSION IS CORRECT
#### t <- "Tortillas Bolsa 2a 1kg"
#### as.character(regmatches(t, gregexpr("[0-9]+kg",t)))