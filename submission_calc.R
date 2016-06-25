libs <- c("xgboost", "Matrix", "data.table", "caret")
sapply(libs, library, character.only = T, logical.return = T,
       quietly = T, warn.conflicts = F)
load("test_prepared.Rdata")
load("f3_reduced.Rdata")

dt <- dt[ ,`:=`(Venta_uni_hoy = NULL, Venta_hoy = NULL,
                Dev_uni_proxima = NULL, Dev_proxima = NULL,
                Town =NULL)]

a <- a[ ,`:=`(Town =NULL)]

dt$State <- as.factor(dt$State)
dt$Canal_ID <- as.factor(dt$Canal_ID)
a$State <- as.factor(a$State)
a$Canal_ID <- as.factor(a$Canal_ID)

label <- dt$Demanda_uni_equil
dt <- dt[ ,`:=`(Demanda_uni_equil = NULL)]
dt <- sparse.model.matrix( ~ . - 1, data = dt, row.names = F)

id <- a$id
a <- a[ ,`:=`(id = NULL)]
a <- sparse.model.matrix( ~ . - 1, data = a, row.names = F)

fit <- xgboost(data = dt, 
               label = label, 
               booster = "gbtree",
               objective = "reg:linear",
               nround = 5,
               verbose = 1)

answers <- predict(fit, a)

answers <- data.frame(id = id, Demanda_uni_equil = answers)
answers <- answers[order(answers$id),]
write.csv(answers, "submission_1.csv", row.names = F)


