#IMPOSTAZIONI E IMPORTAZIONE
setwd("/Users/massimilianodimarco/Desktop/R_Data")
library(tidyverse)
stroke <- read_csv("healthcare-dataset-stroke-data.csv")
stroke <- as_tibble(stroke)




# PULISCO NOME COLONNA RESIDENCE
stroke <- rename(stroke, residence_type = Residence_type)



# FATTORIZZAZIONE DELLE COLONNE 
stroke$heart_disease <- factor(stroke$heart_disease)
stroke$work_type <- factor(stroke$work_type)
stroke$smoking_status <- factor(stroke$smoking_status)
stroke$smoking_status <- factor(stroke$smoking_status)
stroke$residence_type <- factor(stroke$residence_type)
stroke$ever_married <- factor(ifelse(stroke$ever_married == "Yes", "Si", "No"))
stroke$residence_type <- factor(ifelse(stroke$residence_type == "Urban", "Città",
                                       "Campagna"))
stroke$gender <- factor(ifelse(stroke$gender == "Male", "Maschio", "Femmina"))



# CAMBIO DEI NOMI DEI LIVELLI
levels(stroke$work_type) <- c("Bambini", "Dipendente Pubblico", 
                              "Mai Lavorato", "Privato", "Libero Professionista")
levels(stroke$smoking_status) <- c("Ex-fumatore", "Mai fumato", 
                                   "Fumatore", "Sconosciuto")



# RIORDINO DEI LIVELLI
stroke$work_type <- relevel(stroke$work_type, ref = "Mai Lavorato")
stroke$smoking_status <- relevel(stroke$smoking_status, ref = "Sconosciuto",
                                 "Mai fumato")



# CONVERSIONE COLONNA BMI IN NUMERICA
stroke$bmi <- as.numeric(stroke$bmi)



# CORRELAZIONE TRA EVER_MARRIED E STROKE
ratios <- stroke %>%
       group_by(ever_married, stroke) %>%
       summarise(count = n()) %>%
       pivot_wider(names_from = stroke, values_from = count) %>%
       mutate(ratio = `1` / `0`)
# GRAFICO A BARRE
ggplot(ratios, aes(x = ever_married, y = ratio, fill = ever_married)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Ever Married") +
  ylab("Ratio") +
  ggtitle("Rapporto Casi/Pazienti") +
  scale_fill_manual(values = c("grey", "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# DALLA VISUALIZZAZIONE SEMBRA CHE ESSERE SPOSATI ABBIA UNA DENSITA DI
# STROKE MAGGIORE, MA IL VALORE POTREBBE ESSERE SPORCATO DALL'ETA MEDIA DI CHI
# è SPOSATO
# CONTROLLO L'ETA MEDIA DEI DUE GRUPPI
stroke %>% 
  group_by(ever_married) %>%
  summarise(età_media = mean(age)) %>%
  ggplot( aes(x = ever_married , y = età_media, fill = ever_married)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Matrimonio") +
  ylab("Età Media") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# CONTROLLO L'ANDAMENTO NELL'ETà DELLA CATEGORIA EVER MARRIED
ggplot(stroke, aes(x = age, fill = ever_married)) +
  geom_density(alpha = 0.5) +
  labs(x = "Age", y = "Density", fill = "Ever Married") +
  theme_minimal()
# VEDO CHE LA MAGGIOR PARTE DEI GIOVANI SONO NON SPOSATI
# FILTRO ELIMINANDO UNA PARTE DEL TIBBLE PIù GIOVANE
ratios <- stroke %>%
  filter(age > 45) %>%
  group_by(ever_married, stroke) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count) %>%
  mutate(ratio = `1` / `0`)
# VISUALIZZAZIONE A BARRE
ggplot(ratios, aes(x = ever_married, y = ratio, fill = ever_married)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Ever Married") +
  ylab("Ratio") +
  ggtitle("Rapporto Casi/Pazienti Over 45") +
  scale_fill_manual(values = c("grey", "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# SEMBRA CHE PER GLI OVER 45 IL RATIO SIà PIù ALTO PER CHI NON SI è SPOSATO
# PROVO A VISUALIZZARE DIVIDENDO PER ETà E MATRIMONIO
stroke <- stroke %>%
  mutate(age_group = case_when(
    age >= 0 & age <= 10  ~ "0-10",
    age > 10 & age <= 20  ~ "10-20",
    age > 20 & age <= 30  ~ "20-30",
    age > 30 & age <= 40  ~ "30-40",
    age > 40 & age <= 50  ~ "40-50",
    age > 50 & age <= 60  ~ "50-60",
    age > 60 & age <= 70  ~ "60-70",
    age > 70              ~ "Over 70",))

ratios7 <- stroke %>% group_by(age_group, ever_married, stroke) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count, values_fill = 0) %>%
  mutate(ratio = `1` / `0`)
  
ggplot(ratios7, aes(x = age_group, y = ratio, fill = ever_married)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Age") +
  ylab("Ratio") +
  ggtitle("Stroke Ratio by Age Group and Marital Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# OTTENGO CHE LA VARIABILE CHE PIù INFLUISCE SULLA DENSITA DI ICTUS
# SIA L'ETA, SEMBRA CHE SPOSANDOSI DECRESCA LEGGERMENTE LA DESNSITà DI ICTUS



# OSSERVO IL RAPPORTO TRA WORK_TYPE E ICTUS
ratios2 <- stroke %>%
  group_by(work_type, stroke) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count, values_fill = 0) %>%
  mutate(ratio = `1` / `0`)
# GRAFICO ORDINATO
ggplot(ratios2, aes(x = reorder( work_type, ratio ), y = ratio, fill = work_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Work Type") +
  ylab("Ratio") +
  ggtitle("Rapporto Casi/Pazienti") +
  theme_minimal() +
  scale_fill_manual(values = c("grey","yellow","orange","red","purple"))+
  theme(plot.title = element_text(hjust = 0.5))
# ANCHE IN QUESTO CASO LA VARIABILE SEMBRA INFLUIRE POCO, è PIù 
# RELATA ALL'ETà COME SI VEDE DAL SEGUENTE TIBBLE
stroke %>% group_by(work_type) %>% summarise(avg_age = mean(age))



# RELAZIONE TRA RESIDENZA E ICTUS
ratios3 <- stroke %>%
  group_by(residence_type, stroke) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count) %>%
  mutate(ratio = `1` / `0`)
# GRAFICO A BARRE
ggplot(ratios3, aes(x = residence_type, y = ratio, fill = residence_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Residence Type") +
  ylab("Ratio") +
  ggtitle("Rapporto Casi/Pazienti") +
  theme_minimal() +
  scale_fill_manual(values = c("grey","purple"))+
  theme(plot.title = element_text(hjust = 0.5))
# NON CI SONO SIGNIFICATIVE CORRISPONDENZE



# CALCOLO PER GENDER
ratios4 <- stroke %>%
  group_by(gender, stroke) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count) %>%
  mutate(ratio = `1` / `0`)
# Creo il grafico a barre con residence_type
ggplot(ratios4, aes(x = gender, y = ratio, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Gender") +
  ylab("Ratio") +
  ggtitle("Rapporto Casi/Pazienti") +
  theme_minimal() +
  scale_fill_manual(values = c("grey","purple"))+
  theme(plot.title = element_text(hjust = 0.5))
# NON CI SONO SIGNIFICATIVE CORRISPONDENZE



# CALCOLO LA RELAZIONE TRA FUMO E ICTUS
ratios5 <- stroke %>%
  group_by(smoking_status, stroke) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count) %>%
  mutate(ratio = `1` / `0`) 
# GRAFICO A BARRE
ggplot(ratios5, aes(x = reorder(smoking_status, ratio) , y = ratio, fill = smoking_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Smoking") +
  ylab("Ratio") +
  ggtitle("Rapporto Casi/Pazienti") +
  theme_minimal() +
  scale_fill_manual(values = c("grey","purple", "yellow", "red"))+
  theme(plot.title = element_text(hjust = 0.5))
# ANCHE IN QUESTO CASO LA VARIABILE SEMBRA INFLUIRE POCO, è PIù 
# RELATA ALL'ETà COME SI VEDE DAL SEGUENTE TIBBLE
stroke %>% group_by(smoking_status) %>% summarise(avg_age = mean(age))



# ETà E ICTUS
# SUDDIVIDO PER ETà
# PLOT CON DISTRIBUZIONE DENSITà PER L'ETà
stroke %>% group_by(age, stroke) %>% summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count, values_fill = 0) %>%
  mutate(ratio = `1` / `0`) %>% ggplot(aes(age, ratio)) + 
  geom_point() + geom_jitter() + geom_smooth( se = FALSE) +
  ggtitle("Stroke Ratio by Age") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# LA LINEA DI TENDENZA è ESPLICATIVA DELLA RELAZIONE TRA
# AUMENTO D'ETà E DENSITà DI ICTUS
stroke %>% mutate(one_division = ifelse(age > 50, "over50", "under50")) %>%
  group_by(one_division) %>% 
  summarise(count = n(), stroke1 = sum(stroke), ratio = stroke1 / count) %>%
  ggplot(aes(one_division, ratio, color = one_division, fill = one_division)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal()
# VEDIAMO COME CI SIA QUASI UN ORDINE DI GRANDEZZA TRA LE DUE DENSITà



# IPERTENSIONE STROKE
ratios8 <- stroke %>%
  group_by(hypertension, stroke) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count, values_fill = 0) %>%
  mutate(ratio = `1` / `0`)
# PROVIAMO CON UN GRAFICO A BARRE
ggplot(ratios8, aes(x = hypertension, y = ratio, fill = hypertension)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Hypertension") +
  ylab("Ratio") +
  ggtitle("Stroke Ratio by Hypertension") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# VEDIAMO UNA RELAZIONE TRA IPERTENSIONE E AUMENTO DI ICTUS
# PROVIAMO A CONFRONTARE NEL TEMPO COME VARIANO ETà E IPERTENSIONE
stroke %>% 
  select(age, hypertension, stroke) %>% 
  group_by(age) %>%
  mutate(avg_stroke = mean(stroke), avg_hypertension = mean(hypertension)) %>%
  ggplot(aes(age)) + 
  geom_line(aes(y = avg_hypertension, color = "Ipertensione"), name = "Ipertensione") +
  geom_line(aes(y = avg_stroke, color = "Ictus"), name = "Ictus") +
  labs(x = "Age", y = "Ratio") +
  scale_color_manual(values = c("red", "blue"), labels = c("Ipertensione", "Ictus")) +
  theme_minimal()
# ANCHE QUI OSSERVIAMO UNA RELAZIONE TRA LE DUE VARIABILI



# CONFRONTO BMI STROKE
stroke %>% group_by(bmi, stroke) %>% filter(!is.na(bmi)) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count, values_fill = 0) %>%
  mutate(ratio = `1` / `0`) %>% ggplot(aes(bmi, ratio)) + 
  geom_point() + geom_jitter() + geom_smooth(se = FALSE)
# NON MI è MOLTO UTILE HO MOLTI VALORI INDIVDUALI CHE POSSO FALSARE LA VISUALIZZAZIONE
# DIVIDO BMI IN SOTTOGRUPPI
stroke <- stroke %>%
  mutate(bmi_group = case_when(
    bmi >= 0   & bmi <= 16.4  ~ "Sottopeso severo",
    bmi > 16.4 & bmi <= 18.3  ~ "Sottopeso",
    bmi > 18.3 & bmi <= 24.9  ~ "Normale",
    bmi > 24.9 & bmi <= 30    ~ "Sovrappeso",
    bmi > 30   & bmi <= 34.8  ~ "Obesità I",
    bmi > 34.8 & bmi <= 39.9  ~ "Obesità II",
    bmi > 39.9                ~ "Obesità III",))
stroke$bmi_group <- factor(stroke$bmi_group, levels = c("Sottopeso severo",
"Sottopeso", "Normale", "Sovrappeso", "Obesità I", "Obesità II", "Obesità III"))

ratios9 <- stroke %>% filter(!is.na(bmi)) %>%
  group_by(bmi_group, stroke) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count, values_fill = 0) %>%
  mutate(ratio = `1` / `0`)
# COSTRUISCO UN GRAFICO A BARRE
ggplot(ratios9, aes(x = reorder(bmi_group, ratio), y = ratio, fill = bmi_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("BMI") +
  ylab("Ratio") +
  ggtitle("Stroke Ratio by BMI Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# OSSERVIAMO COME LE QUATTRO VOCI SOVRAPPESO E OLTRE SIANO LE PRIME QUATTRO
# CI SUGGERISCE UNA RELAZIONE TRA INDICE MASSA CORPOREA E ICTUS
# PROVO A FARE UNO SCATTERPLOT CON GRANULITà PIù FINE
stroke %>% 
  filter(!is.na(bmi)) %>% 
  group_by(bmi) %>%
  summarise(avg_stroke = mean(stroke), count = n()) %>% 
  filter(count > 20,) %>%
  ggplot(aes(bmi, avg_stroke, size = count)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method='lm', formula= y~x, show.legend = FALSE) +
  labs(x = "BMI", y = "Average Stroke") +
  guides(size = guide_legend(title = "Count")) +  
  theme_minimal()
# SEMBRA ESSERCI UNA RELAZIONE FINO A 30 BMI PER POI ARRESTARSI



# GLUCOSIO E ICTUS
# DIVIDO PER SOTTOGRUPPI
stroke <- stroke %>%
  mutate(glu_group = case_when(
    avg_glucose_level >= 0    & avg_glucose_level <= 59.9  ~ "Ipoglicemia",
    avg_glucose_level > 59.9  & avg_glucose_level <= 99.9  ~ "Normale",
    avg_glucose_level > 99.9  & avg_glucose_level <= 129.9  ~ "Alterata glicemia a digiuno (IFG)",
    avg_glucose_level > 129.9 & avg_glucose_level <= 169.9  ~ "Diabete I",
    avg_glucose_level > 169.9 & avg_glucose_level <= 199.9 ~ "Diabete II",
    avg_glucose_level > 199.9  ~ "Diabete III"))
stroke$glu_group <- factor(stroke$glu_group, levels =
                             c("Ipoglicemia","Normale","Alterata glicemia a digiuno (IFG)",
                               "Diabete I", "Diabete II","Diabete III"))
# COSTRUISCO UN GRAFICO A BARRE
ratios10 <- stroke %>%
  group_by(glu_group, stroke) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count, values_fill = 0) %>%
  mutate(ratio = `1` / `0`)

ggplot(ratios10, aes(x = reorder(glu_group, ratio), y = ratio, fill = glu_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Glucosio Group") +
  ylab("Ratio") +
  ggtitle("Stroke Ratio by Glucosio Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# OSSERVIAMO UNA RELAZIONE PER ALTI LIVELLI GLICEMICI E RATIO DI ICTUS



# MALATTIE CARDIACHE E ICTUS
ratios11 <- stroke %>%
  group_by(heart_disease, stroke) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count, values_fill = 0) %>%
  mutate(ratio = `1` / `0`)
# GRAFICO A BARRE
ggplot(ratios11, aes(x = heart_disease, y = ratio, fill = heart_disease)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Malattie Cardiache") +
  ylab("Ratio") +
  ggtitle("Rapporto Casi/Pazienti") +
  scale_fill_manual(values = c("grey", "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# OSSERVIAMO UNA CORRELAZIONE TRA MALATTIE CARDIACHE E ICTUS
# OSSERVIAMO UNA CORRELAZIONE TRA MALATTIE CARDIACHE E ICTUS
# CONTROLLIAMO PER GRUPPI D'ETà
stroke %>% group_by(heart_disease) %>% summarise(avg_age = mean(age))
ratios12 <- stroke %>% group_by(age_group, heart_disease, stroke) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = stroke, values_from = count, values_fill = 0) %>%
  mutate(ratio = `1` / `0`)
# GRAFICO
ggplot(ratios12, aes(x = age_group, y = ratio, fill = heart_disease)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Age") +
  ylab("Ratio") +
  ggtitle("Stroke Ratio by Age Group and Heart Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# PROBLEMI CARDIACI EMERGONO CON L'ETà MA PER GRUPPO D'ETà PORTANO AD UN 
# PEGGIOR RATIO PER ICTUS



# IN ULTIMA ANALISI PROVIAMO A METTERE INSIEME TUTTE LE CARATTERISTICHE
# CHE PORTANO AD UN AUMENTO DELLA DENSITà DI UN ICTUS
# RAGGRUPPIAMO PER GRUPPI "A RISCHIO" E NO
rischio_ictus <- stroke %>%
  filter(hypertension == 1, avg_glucose_level > 180, heart_disease == 1, smoking_status == "Fumatore")
basso_rischio_ictus <- stroke %>% 
  filter(hypertension == 0, avg_glucose_level < 180, heart_disease == 0, !(smoking_status == "Fumatore"))

# VEDIAMO LA DENSITà DI ICTUS NEL PRIMO E SECONDO GRUPPO
x <- mean(rischio_ictus$stroke)

y <- mean(basso_rischio_ictus$stroke)
  
confronto <- tibble(ratio = c(x, y),
                    rischio = c("Rischio ictus", "Basso rischio"))
ggplot(confronto, aes(x = ratio, y = rischio)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "", y = "Valore") +
  coord_flip() +
  ggtitle("Combinazione Ipertensione, Diabete, Malattie Cardiache, Fumatore") +
  theme_minimal()
# NOTARE CHE SOLO 5 PERSONE HANNO QUESTA COMBINAZIONE RISCHIOSA

  

# PROVIAMO A TOGLIERE UNA
rischio_ictus <- stroke %>%
  filter(hypertension == 1, avg_glucose_level > 180, heart_disease == 1)
basso_rischio_ictus <- stroke %>% 
  filter(hypertension == 0, avg_glucose_level < 180, heart_disease == 0)
  
# VEDIAMO LA DENSITà DI ICTUS NEL PRIMO E SECONDO GRUPPO
x <- mean(rischio_ictus$stroke)
  
y <- mean(basso_rischio_ictus$stroke)

confronto <- tibble(ratio = c(x, y),
                    rischio = c("Rischio ictus", "Basso rischio"))
ggplot(confronto, aes(x = ratio, y = rischio)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "", y = "Valore") +
  ggtitle("Combinazione Ipertensione, Diabete, Malattie Cardiache, Fumatore") +
  coord_flip() +
  theme_minimal()
# NOTIAMO UN CAMBIAMENTO



# PROVIAMO UN ALTRA COMBINAZIONE
rischio_ictus <- stroke %>%
  filter(hypertension == 1, avg_glucose_level > 180, age > 65)
basso_rischio_ictus <- stroke %>% 
  filter(hypertension == 0, avg_glucose_level < 180, age < 65)

# VEDIAMO LA DENSITà DI ICTUS NEL PRIMO E SECONDO GRUPPO
x <- mean(rischio_ictus$stroke)

y <- mean(basso_rischio_ictus$stroke)

confronto <- tibble(ratio = c(x, y),
                    rischio = c("Rischio ictus", "Basso rischio"))
ggplot(confronto, aes(x = ratio, y = rischio)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "", y = "Valore") +
  ggtitle("Combinazione Ipertensione, Diabete, Over/Under 65") +
  coord_flip() +
  theme_minimal()




# PROVIAMO UN ALTRA COMBINAZIONE
rischio_ictus <- stroke %>%
  filter(heart_disease == 1, avg_glucose_level > 180)
basso_rischio_ictus <- stroke %>% 
  filter(heart_disease == 0, avg_glucose_level < 180)

# VEDIAMO LA DENSITà DI ICTUS NEL PRIMO E SECONDO GRUPPO
x <- mean(rischio_ictus$stroke)

y <- mean(basso_rischio_ictus$stroke)

confronto <- tibble(ratio = c(x, y),
                    rischio = c("Rischio ictus", "Basso rischio"))
ggplot(confronto, aes(x = ratio, y = rischio)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "", y = "Valore") +
  ggtitle("Combinazione Malattie Cardiache, Diabete") +
  coord_flip() +
  theme_minimal()




# PROVIAMO UN ULTIMA COMBINAZIONE
rischio_ictus <- stroke %>%
  filter(avg_glucose_level > 180, age > 45, bmi > 28)
basso_rischio_ictus <- stroke %>% 
  filter(avg_glucose_level < 180, age < 45, bmi < 28)

# VEDIAMO LA DENSITà DI ICTUS NEL PRIMO E SECONDO GRUPPO
x <- mean(rischio_ictus$stroke)

y <- mean(basso_rischio_ictus$stroke)

confronto <- tibble(ratio = c(x, y),
                    rischio = c("Rischio ictus", "Basso rischio"))
ggplot(confronto, aes(x = ratio, y = rischio)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "", y = "Valore") +
  ggtitle("Diabete, Over/Under 45, BMI<>28") +
  coord_flip() +
  theme_minimal()




















