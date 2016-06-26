
products <- read.csv("../BMB/producto_tabla.csv", stringsAsFactors = F)

productsOriginal <- products

#change "G" to "g" and "McD" to "McDonalds" 
products$NombreProducto <- gsub('G ', 'g', products$NombreProducto, ignore.case = FALSE)
products$NombreProducto <- gsub('McD ', 'McDonalds ', products$NombreProducto, ignore.case = FALSE)

# Last numbers in NombreProducto look like Producto_ID
# Let's check
last.numbers <- sapply(products$NombreProducto, 
                       function(x) tail(unlist(strsplit(x, " ")), 1))
last.numbers <- as.integer(last.numbers)
unique(last.numbers - products$Producto_ID) 
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
                gregexpr("[0-9]+[a-zA-Z]+", products$NombreProducto))
# And remove all numbers
k <- gsub("[0-9]+", "", k)
# And finally have a look on what we have
unique(k)

products$WeightsVolumes <- as.character(regmatches(products$NombreProducto,
        gregexpr("[0-9]+g|[0-9]+Kg|[0-9]+kg|[0-9]+gProm|[0-9]+oz|[0-9]+ml|[0-9]+ ml", 
                                                            products$NombreProducto)))

products$TMP <- as.character(regmatches(products$NombreProducto,
                gregexpr(" [0-9]+ [0-9]+Kg| [0-9]+ [0-9]+g", products$NombreProducto)))
#products$TMP <- sapply(products$TMP, function (x)  sub("^\\s+", "", x))
products$TMP <- sub("^\\s+", "", products$TMP)
products$TMP <- sub("[[:space:]]", ".", products$TMP)
products$TMP <- sub("character\\(0\\)", "fix", products$TMP)
products[which(products$TMP != "fix"),]$WeightsVolumes <- products[which(products$TMP != "fix"),]$TMP

# Now let us separate measurements
products$Measure <- gsub("[0-9]+", "", products$WeightsVolumes)
products$Measure <- gsub("[.]", "", products$Measure)
products$Measure <- gsub("Kg", "kg", products$Measure)
products$WeightsVolumes <- gsub("[A-Za-z]+", "", products$WeightsVolumes)

unique(products$Measure)
# Dirty result! Let us clean it step by step

products$Measure[which(products$Measure == "gProm")] <- "g"
products$Measure[which(products$Measure == " ml")] <- "ml"

products[which(products$Measure == "c(\"g\", \"g\")"),]
#412       30302 Tostado 210g y Cajeta Quemada 18g        BIM c(" 210g", " 18g")     c("g", "g")
txt2g <- products[which(products$Measure == 'c(\"g\", \"g\")'),]$NombreProducto
products[which(products$Measure == "c(\"g\", \"g\")"),]$WeightsVolumes <- 
        regmatches(txt2g, regexpr('[0-9]+', txt2g))
#change all 'c(\"g\", \"g\")' to 'g'
products[which(products$Measure == "c(\"g\", \"g\")"),]$Measure <- "g"

products$Measure[1] <- "g"
products$WeightsVolumes[1] <- 0

#change "kg" to "g"
products$WeightsVolumes <- as.numeric(products$WeightsVolumes)
products[which(products$Measure == 'kg'),]$WeightsVolumes <- 
        products[which(products$Measure == 'kg'),]$WeightsVolumes*1000
products$Measure <- gsub('kg', 'g', products$Measure)
unique(products$Measure)

#now we have to solve with character()
nrow(products[which(products$Measure == "character()"),])
# [1] 50 !!!!! What are they?
#products[which(products$Measure == "character()"),]

#manually fix all empty (g, ml & others) producto 
products[which(products$NombreProducto == 'Paletina'),]$Measure <- "gds"
products[which(products$NombreProducto == 'Paletina para Cafe'),]$Measure <- "gds"
products[which(products$NombreProducto == 'Camioncitos Bimbo'),]$Measure <- "gds"
products[which(products$NombreProducto == 'Camioncito Bimbo Modelo 3'),]$Measure <- "gds"
products[which(products$NombreProducto == 'Servilletero Bimbollos'),]$Measure <- "gds"
products[which(products$NombreProducto == 'Almohada del Osito Bimbo'),]$Measure <- "gds"
products[which(products$NombreProducto == 'Exhibidor PDQ Bran Frut'),]$Measure <- "gds"

products[which(products$NombreProducto == 'Tuinky Fresas con Crema 2p'),]$WeightsVolumes <- 38
products[which(products$NombreProducto == 'Bollo Regular 2pq 30p McDonalds'),]$WeightsVolumes <- 1200
products[which(products$NombreProducto == 'Donas 6P Prom'),]$WeightsVolumes <- 157.5
products[which(products$NombreProducto == 'Doraditas 4p'),]$WeightsVolumes <- 110
products[which(products$NombreProducto == 'Mantecadas Chocolate 3p'),]$WeightsVolumes <- 142.5
products[which(products$NombreProducto == 'Mantecadas 2p'),]$WeightsVolumes <- 105
products[which(products$NombreProducto == 'Leche Gansito Chocolate 24p'),]$WeightsVolumes <- 24*200
products[which(products$NombreProducto == 'Empanaditas Pina 20p'),]$WeightsVolumes <- 506
products[which(products$NombreProducto == 'Empanaditas Pina 20p Prom'),]$WeightsVolumes <- 506
products[which(products$NombreProducto == 'Empanzador Crujiente Prom'),]$WeightsVolumes <- 175
products[which(products$NombreProducto == 'Bran Frut Mix 20p'),]$WeightsVolumes <- 40
products[which(products$NombreProducto == 'Bran Frut Mix 8p'),]$WeightsVolumes <- 40
products[which(products$NombreProducto == 'Bran Frut Fresa 18p Prom'),]$WeightsVolumes <- 40
products[which(products$NombreProducto == 'BranFrut Fresa 18p'),]$WeightsVolumes <- 40
products[which(products$NombreProducto == 'Barra Multigrano Nuez 12p Prom'),]$WeightsVolumes <- 408
products[which(products$NombreProducto == 'Choco Roles Fresa 3X10 Prom'),]$WeightsVolumes <- 67
products[which(products$NombreProducto == 'Pan Blanco Freihofers 28Reb'),]$WeightsVolumes <- 820
products[which(products$NombreProducto == 'Pan Blanco Mrs Bairds 28Reb'),]$WeightsVolumes <- 820
products[which(products$NombreProducto == 'Pan Blanco Stroehmann 28Reb'),]$WeightsVolumes <- 820
products[which(products$NombreProducto == 'Pan Blanco Rainbo 28Reb'),]$WeightsVolumes <- 820
products[which(products$NombreProducto == 'Pan Blanco Pullman 28Reb'),]$WeightsVolumes <- 820
products[which(products$NombreProducto == 'Pan con Fibra Freihofers 28Reb'),]$WeightsVolumes <- 820
products[which(products$NombreProducto == 'Pan con Fibra Mrs Bairds 28Reb'),]$WeightsVolumes <- 820
products[which(products$NombreProducto == 'Pan con Fibra Stroehmann 28Reb'),]$WeightsVolumes <- 820
products[which(products$NombreProducto == 'Pan con Fibra Pullman 28Reb'),]$WeightsVolumes <- 820
products[which(products$NombreProducto == 'Tarima Twin Pack Thins Multig'),]$WeightsVolumes <- 680
products[which(products$NombreProducto == 'Choco Roles 15p Cj'),]$WeightsVolumes <- 5
products[which(products$NombreProducto == 'Mantecadas 2p'),]$WeightsVolumes <- 105
products[which(products$NombreProducto == 'Donas 6p Prom'),]$WeightsVolumes <- 158
products[which(products$NombreProducto == 'Doraditas 4p'),]$WeightsVolumes <- 147
products[which(products$NombreProducto == 'Mantecadas 2p'),]$WeightsVolumes <- 105
products[which(products$NombreProducto == 'Tostada Ondulada Tubo 30p'),]$WeightsVolumes <- 360

products[which(products$NombreProducto == 'Leche Gansito Chocolate 24p'),]$WeightsVolumes <- 236
products[which(products$NombreProducto == 'Leche Gansito Chocolate 24p'),]$Measure <- "ml"
products$Measure[which(products$Measure == "character()")] <- "g"

# lets produce new column with number of the pieces
products$Pieces <- as.character(regmatches(products$NombreProducto, 
                                           gregexpr('[0-9]+p ', products$NombreProducto)))
products$Pieces <- sub("character\\(0\\)", "1p", products$Pieces)
products$Pieces <- sub("p", "", products$Pieces)

# remove column TMP
products <- products[,c(-5)]