towns <- read.csv("town_state1.csv", sep = ";") #because encoding

towns$State <- gsub("Queretaro de Arteaga", "QUERETARO", towns$State)
towns$Popul <- as.numeric(0)
#We rank the states by population
towns$Popul[which(towns$State == "ESTADO DE MEXICO")] <- 1
towns$Popul[which(towns$State == "MEXICO, D.F.")] <- 2
towns$Popul[which(towns$State == "VERACRUZ")] <- 3
towns$Popul[which(towns$State == "JALISCO")] <- 4
towns$Popul[which(towns$State == "PUEBLA")] <- 5
towns$Popul[which(towns$State == "GUANAJUATO")] <- 6
towns$Popul[which(towns$State == "CHIAPAS")] <- 7
towns$Popul[which(towns$State == "NUEVO LEON")] <- 8
towns$Popul[which(towns$State == "MICHOACAN")] <- 9
towns$Popul[which(towns$State == "OAXACA")] <- 10
towns$Popul[which(towns$State == "CHIHUAHUA")] <- 11
towns$Popul[which(towns$State == "GUERRERO")] <- 12
towns$Popul[which(towns$State == "TAMAULIPAS")] <- 13
towns$Popul[which(towns$State == "BAJA CALIFORNIA NORTE")] <- 14
towns$Popul[which(towns$State == "SINALOA")] <- 15
towns$Popul[which(towns$State == "COAHUILA")] <- 16
towns$Popul[which(towns$State == "HIDALGO")] <- 17
towns$Popul[which(towns$State == "SONORA")] <- 18
towns$Popul[which(towns$State == "SAN LUIS POTOSI")] <- 19
towns$Popul[which(towns$State == "TABASCO")] <- 20
towns$Popul[which(towns$State == "YUCATAN")] <- 21
towns$Popul[which(towns$State == "QUERETARO")] <- 22
towns$Popul[which(towns$State == "MORELOS")] <- 23
towns$Popul[which(towns$State == "DURANGO")] <- 24
towns$Popul[which(towns$State == "ZACATECAS")] <- 25
towns$Popul[which(towns$State == "QUINTANA ROO")] <- 26
towns$Popul[which(towns$State == "AGUASCALIENTES")] <- 27
towns$Popul[which(towns$State == "TLAXCALA")] <- 28
towns$Popul[which(towns$State == "NAYARIT")] <- 29
towns$Popul[which(towns$State == "CAMPECHE")] <- 30
towns$Popul[which(towns$State == "COLIMA")] <- 31
towns$Popul[which(towns$State == "BAJA CALIFORNIA SUR")] <- 32

#look how many times same address appear in the column Town or 
#how many different agencies passes at each address
freqaddress <- as.data.frame(table(towns$Town))
freqaddress$addnum <- as.numeric(sub(".*(\\d{4}).*", "\\1", freqaddress$Var1))
save(freqaddress, file = "prepared_freq_address.RData")

towns$addnum <- as.numeric(sub(".*(\\d{4}).*", "\\1", towns$Town))
towns <- towns[,-c(2,3)]

save(towns, file = "prepared_towns.RData")