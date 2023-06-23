# Modello per stroke
# carico i dati
setwd("/Users/massimilianodimarco/Desktop/R_Data")
library(tidyverse)
stroke <- read_csv("healthcare-dataset-stroke-data.csv")
stroke <- as_tibble(stroke)




# PULISCO NOME COLONNA RESIDENCE
stroke <- rename(stroke, residence_type = Residence_type)




# FATTORIZZAZIONE DELLE COLONNE 
stroke$work_type <- factor(stroke$work_type)
stroke$smoking_status <- factor(stroke$smoking_status)
stroke$ever_married <- factor(ifelse(stroke$ever_married == "Yes", "Si", "No"))
stroke$residence_type <- factor(ifelse(stroke$residence_type == "Urban", "Città",
                                       "Campagna"))
stroke$gender <- factor(ifelse(stroke$gender == "Male", "Maschio", "Femmina"))
stroke$bmi <- as.numeric(stroke$bmi)
stroke$hypertension <- factor(ifelse(stroke$hypertension == 1, "Si", "No"))
stroke$gender <- factor(stroke$gender)
stroke$heart_disease <- factor(ifelse(stroke$heart_disease == 1, "Si", "No"))




# CAMBIO DEI NOMI DEI LIVELLI
levels(stroke$work_type) <- c("Bambini", "Pubblico", 
                              "Mai Lavorato", "Privato", "Libero Professionista")
levels(stroke$smoking_status) <- c("Ex-fumatore", "Mai fumato", 
                                   "Fumatore", "Sconosciuto")




# RIORDINO DEI LIVELLI
stroke$work_type <- relevel(stroke$work_type, ref = "Mai Lavorato")
stroke$smoking_status <- relevel(stroke$smoking_status, ref = "Sconosciuto",
                                 "Mai fumato")




# VEDO STROKE COME UN DATAFRAME
st <- as.data.frame(stroke)
# CAMBIO TIPO STROKE
st$stroke <- ifelse(st$stroke == 1, "True", "False")
st <- filter(st, !is.na(bmi))




# TOLGO VARIABILI NON INFLUENTI
st1 = subset(st, select = -c(id, gender,ever_married,
                             residence_type))




# CONTROLLO CHE LE VARIABILI NUMERICHE SIANO AUTONOME
numeric_cols <- unlist(lapply(st1, is.numeric))
my_cols <- c("red", "blue")
color_id <- c("True" = 2, "False" = 1)
pairs(st1[, numeric_cols], pch = 19,  cex = 0.5, col = my_cols[color_id[st1$stroke]])




library(caTools)
set.seed(17)
# DIVIDO IN GRUPPO TRAIN E GRUPPO TEST
split <- sample.split(st1, SplitRatio = 0.7)
train_cl <- subset(st1, split == "TRUE")
test_cl <- subset(st1, split == "FALSE")




library(rpart)
library(cluster)
library(maptree)
# CREO ALBERO 
decision.tree = rpart(stroke ~ ., data = train_cl, method = 'class')
draw.tree(decision.tree)

library(lattice)
library(caret)
# TESTO L'ALBERO E PREDICO GRUPPO TEST
st_pred <- predict(decision.tree, test_cl, type = "class")
# Confusion Matrix
st_cm <- table(test_cl$stroke, st_pred)
st_cm
# VALUTO IL MODELLO
confusionMatrix(st_cm, positive = "True")



library(e1071)
# USO NAIVE BAYES
nb <- naiveBayes(stroke ~ ., data = train_cl)
nb
# PREDICO IL GRUPPO TEST
nb_pred <- predict(nb, newdata = test_cl)
# Confusion Matrix
nb_cm <- table(test_cl$stroke, nb_pred)
nb_cm
# VALUTO IL MODELLO
confusionMatrix(nb_cm, positive = "True")



# PROVO A BILANCIARE IL TRAINSET
table(train_cl$stroke) 

# SELEZIONO VOCI STROKE VERE
true_samples <- train_cl[train_cl$stroke == "True", ]

# SCELGO A CASO GLI STROKE FALSI
false_samples <- train_cl[train_cl$stroke == "False", ]
selected_false_samples <- false_samples[sample(nrow(false_samples), 131), ]

# TRAINSET BILANCIATO
balanced_train <- rbind(true_samples, selected_false_samples)

# DISTRIBUZIONE
table(balanced_train$stroke)

# CREO ALBERO 
decision.tree = rpart(stroke ~ ., data = balanced_train, method = 'class')
draw.tree(decision.tree)

# TESTO L'ALBERO E PREDICO GRUPPO TEST
st_pred <- predict(decision.tree, test_cl, type = "class")
# Confusion Matrix
st_cm <- table(test_cl$stroke, st_pred)
st_cm
# VALUTO IL MODELLO
confusionMatrix(st_cm, positive = "True")



# USO NAIVE BAYES
nb <- naiveBayes(stroke ~ ., data = balanced_train)
nb
# PREDICO IL GRUPPO TEST
nb_pred <- predict(nb, newdata = test_cl)
# Confusion Matrix
nb_cm <- table(test_cl$stroke, nb_pred)
nb_cm
# VALUTO IL MODELLO
confusionMatrix(nb_cm, positive = "True")





