## CHURN MODEL
# Il modello churn esprime la propensione di un cliente a cessare il suo rapporto con l'azienda. 


# Al fine di sviluppare tale modello si provvede innanzitutto al caricamento di alcune 
# librerie utili:
library(caret)
library(rpart)
library(rpart.plot)
library(MLmetrics)
library(randomForest)
library(funModeling)
library(e1071)
library(tidyverse)
library(glmnet)
library(LiblineaR)
library(arulesViz)

# Il modello churn produce come output la probabilit? di abbandono e per ottenerla si implementa
# un modello supervisionato di propensione. Quest'ultimo consta di alcune fasi:
# 1. scelta della data di riferimento nel passato
# 2. imporre la durata di un periodo di sospensione (holdout period) che parte dopo ciascuna data di riferimento,
#    quindi creazione di un intervallo di tempo entro il quale il cliente dovrebbe effettuare 
#    un acquisto (in caso di mancato acquisto, il cliente probabilmente abbandona, ? un cliente churn).
#    La durata di questo intervallo corrisponde alla frequenza dell'abbonamento e/o alla scala dei tempi di acquisto,
#    ossia il numero di giorni tale che l'80-90% dei clienti riacquisti entro questo intervallo di tempo dall'ultimo acquisto.
# 3. scelta della durata di un periodo di lookback prima della data di riferimento. Per definire
#    questa lunghezza ? necessario tenere conto della scala temporale di acquisto, ossia il numero di giorni
#    tale che l'80-90% dei clienti riacquisti entro questo intervallo di tempo.
# 4. assegnare ad ogni cliente una variabile target 0/1 tale che 1 ? assegnato ai clienti che probabilmente abbandonano
#    nel periodo di sospensione (clienti churn).
# 5. definire un insieme di variabili predittive potenzialmente rilevanti da calcolare entro il periodo di 
#    lookback
# Per la costruzione di questo modello ? fondamentale che i predittori includano infomazioni sul cliente, 
# informazioni relative al comportamento di quest'ultimo e come cambiano i comportamenti di essi nel periodo
# di riferimento. Inoltre, ? necessario effettuare un controllo del tasso di cambiamento, fare attenzione alla 
# stagionalit?, escludere clienti gi? perduti, escludere eventuali outlier ed infine escludere dal modello variabili
# che hanno un basso impatto sulle variabili target. 
# Il modello pu? essere creato attraverso diversi algoritmi, quali Random Forest, Naive Bayes, Logistic Regression,
# Support Vector Machine e Decision Tree. 

#### COSTRUZIONE DEL MODELLO CHURN ####
# Si implementano le fasi precedentemente sottolineate e i suggerimenti finali, al fine di costruire un modello 
# supervisionato di propensione. 

# FASE 1: identificare una data di riferimento nel passato. 
#         La data di riferimento ? stata fissata l'1 gennaio 2019.

# FASE 2: si ? scelto di fissare l'holdout period a 3 months ( dall' 1/1/2019 al 1/3/2019)
holdout <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1,
          TIC_DATE < as.Date("1/03/2019", format = "%d/%m/%Y"),
          TIC_DATE >= as.Date("01/01/2019", format = "%d/%m/%Y"))

no_churner <- unique(holdout$ID_CLI)

str(holdout)  

# FASE 3: si ? scelta la durata di un periodo di lookback prima della data di riferimento pari a tre mesi.
#         Al fine di implementare questa fase, sono state calcolate le misure di recency, frequency e monetary, dato che
#         i comportamenti dei clienti dipendono fortemente dall'intervallo di lookback scelto. 

lookback_period <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1,
         TIC_DATE < as.Date("1/1/2019",
                            format = "%d/%m/%Y"),
         TIC_DATE > as.Date("01/10/2018",
                            format = "%d/%m/%Y"))


# - Recency
churn_recency <- lookback_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(LAST_PURCHASE_DATE = max(TIC_DATE))

churn_recency$RECENCY <- difftime(as.Date("01/01/2019",
                                          format = "%d/%m/%Y"),         
                                  churn_recency$LAST_PURCHASE_DATE,
                                  units = "days")

# - Frequency 
churn_frequency <- lookback_period %>%
                    filter(DIREZIONE == 1) %>%
                    group_by(ID_CLI) %>%
                    summarise(TOT_PURCHASE = n_distinct(ID_SCONTRINO)) %>%
                    arrange(desc(TOT_PURCHASE))

# - Monetary 
churn_monetary <- lookback_period %>%
                    filter(DIREZIONE == 1) %>%
                    group_by(ID_CLI) %>%
                    summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
                              SCONTO = sum(SCONTO),
                              SPESA = IMPORTO_LORDO - SCONTO) %>%
                    ungroup() %>%
                    as.data.frame() %>%
                    arrange(desc(IMPORTO_LORDO))

churn <- merge(churn_recency, churn_frequency, by = "ID_CLI")
churn <- merge(churn, churn_monetary, by = "ID_CLI") %>%
          select(ID_CLI,
                 RECENCY,
                 SPESA, 
                 TOT_PURCHASE)
knitr::kable(head(churn))

# FASE 4: assegnare ad ogni cliente una variabile target 0/1 tale che 1 ? assegnato ai clienti che potrebbero abbandonare
#         nel periodo di holdout. 
#         Per l'attuazione di questa fase, si ? provveduto dapprima alla creazione di una variabile target.
#         In seguito, si ? effettuata la trasformazione di tale variabile da "chr" a "factor". 

churn$CHURN <- 1

for (i in c(1:nrow(churn))){
  if (churn$ID_CLI[i] %in% no_churner) churn$CHURN[i] <- 0
}

churn$CHURN <- as.factor(churn$CHURN)

table(churn$CHURN)
# La presente operazione mette in evidenza che i clienti che probabilmente abbandonano sono 59535, mentre quelli che dovrebbero restare
# sono 35590.

knitr::kable(head(churn))


View(df_2_cli_account_clean)
df_2_cli_account_clean$TYP_JOB_CLEAN <- df_2_cli_account_clean$TYP_JOB

# FASE 5 : Definizione di un insieme di variabili predittive potenzialmente rilevanti, da calcolare entro il periodo 
#          di lookback. 
#          Si sono ritenute di particolare importanza le variabili: 
#                                                                  - RECENCY,
#                                                                  - SPESA,
#                                                                  - TOT_PURCHASE,
#                                                                  - REGION,
#                                                                  - LAST_COD_FID,
#                                                                  - TYP_JOB_CLEAN.


churn <- left_join(churn, df_2_cli_account_clean[, c("ID_CLI", "TYP_JOB_CLEAN")], by = "ID_CLI")  #-- Add Type Job
churn <- left_join(churn, df_1_cli_fid_clean[, c("ID_CLI", "LAST_COD_FID")], by = "ID_CLI") #-- Add Type of Fidelity Card
region <- left_join(df_2_cli_account_clean[, c("ID_CLI", "ID_ADDRESS")],
                    df_3_cli_address_clean[, c("ID_ADDRESS", "REGION")], by = "ID_ADDRESS") #-- Add Region
churn <- left_join(churn, region, by = "ID_CLI")
churn <- churn[, -8]
churn
# Dal dataset ? stata eliminata la colonna ID_ADDRESS in quanto ritenuta necessaria essendo l'attributo chiave, per unire il
# secondo e il terzo dataset, ma considerata superflua per l'addestramento degli algoritmi. 


### ALGORITMI ####

# Il modello churn pu? essere creato attraverso diversi algoritmi, quali:
# - Decision Tree,
# - Random Forest, 
# - Naive Bayes, 
# - Logistic Regression,


# Per l'implementazione degli algoritmi ? necessario splittare il dataset in Train and Test, assicurandosi di aver omesso 
# eventuali valori mancanti.


churn <- na.omit(churn)

train_index <- createDataPartition(churn$CHURN, 
                                   p = .70, 
                                   list = FALSE, 
                                   times = 1)

#-- Train Test Split
train <- churn[train_index,]
test <- churn[-train_index,]

table(train$CHURN)
table(test$CHURN)


## Decision Tree ####

dec_tree <- rpart(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB_CLEAN + LAST_COD_FID,
              data = train)

rpart.plot(dec_tree, extra = "auto")

summary(dec_tree) 
# Dall'implementazione del modello ? possibile affermare che la variabile maggiormente rilevante ? TOT_PURCHASE.
printcp(dec_tree) 

prediction_tree <- predict(dec_tree, test[, -5], type = "class")
p1 <- unlist(prediction_tree)
confusionMatrix(p1, test$CHURN)

## Random Forest ####

memory.limit(100000)

random_forest <- randomForest(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB_CLEAN + LAST_COD_FID,
                        data = train, ntree = 100)
print(random_forest)

prediction_rf <- predict(random_forest, test[,-5], type = "class")
confusionMatrix(prediction_rf, test$CHURN)

## Logistic Regression ####

logistic_reg <- train(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB_CLEAN + LAST_COD_FID,
                  data = train,
                  method = "glm")

summary(logistic_reg)
# L'implementazione della regressione logistica sottolinea l'importanza delle seguenti variabili: RECENCY, SPESA, TOT_PURCHASE,
# REGIONBASILICATA, REGIONCALABRIA, REGIONMARCHE, REGIONTRENTINO ALTO ADIGE, TYP_JOB_CLEANImpiegato/a, LAST_COD_FIDSTANDARD, 
# LAST_COD_FIDSTANDARD BIZ.

prediction_lg <- predict(logistic_reg, test[, -5], type = "raw")
confusionMatrix(prediction_lg , test$CHURN)

## Naive Bayes ####
naive_bayes <- naiveBayes(CHURN ~ RECENCY + SPESA + TOT_PURCHASE + REGION + TYP_JOB_CLEAN + LAST_COD_FID,
                    data = train)
naive_bayes

prediction_nb <- predict(naive_bayes, test[, -5])
confusionMatrix(prediction_nb, test$CHURN)




###Performance degli algoritmi
##Accuracy
accuracy <- as.data.frame(t(cbind(
                                  confusionMatrix(prediction_nb, test$CHURN)$overall[1],
                                  confusionMatrix(prediction_lg, test$CHURN)$overall[1],
                                  confusionMatrix(prediction_rf, test$CHURN)$overall[1],
                                  confusionMatrix(prediction_tree, test$CHURN)$overall[1])))

accuracy <- as.data.frame(cbind(c( "naive_bayes","logistic_reg", "random_forest","dec_tree "),
                                accuracy))

colnames(accuracy) <- c("Models", "Accuracy")

ggplot(data = accuracy,
       aes(x = Accuracy,
           y = Models,
           fill = Models)) +
  geom_bar(stat = "identity") +
  coord_cartesian(xlim = c(0.663, 0.693)) +
  theme_minimal() +
  guides(scale = "none") +
  labs(title = "Accuracy",
       x = " ",
       y = "Models") +
   theme(plot.title = element_text(hjust = 0.5))+  #-- Centering Title
  
plot(accuracy$Accuracy)

str(accuracy)
# La misura di accuracy consente di affermare che il modello che spiega meglio i dati ? la regressione logistica (0.691).

##F_measure


F1_naive_bayes <- F1_Score(prediction_nb, test$CHURN, positive="1")
F1_logistic_reg <-F1_Score(prediction_lg, test$CHURN, positive="1")
F1_random_forest<-F1_Score(prediction_rf, test$CHURN, positive="1")
F1_dec_tree <-F1_Score(prediction_tree, test$CHURN, positive="1")

F1_measure <- as.data.frame

Models <- c( 'naive_bayes', 'logistic_reg', 'random_forest', 'dec_tree')
F1_Scores <- c(0.7833247,0.7821218,0.7740658, 0.7808354)
# Join the variables to create a data frame
F1_scores_df <- data.frame(Models,F1_Scores)

str(F1_scores_df)

plot_f1_measures <- ggplot(data=F1_scores_df, aes(x=Models, y=F1_Scores, fill=Models)) +
  geom_bar(stat="identity") +
  coord_cartesian(ylim=c(0.751, 0.791))+
  labs(title = "F1 Scores", x = "Models", y = "F1-Score") +
  theme_minimal()
plot_f1_measures
# Considerando la F1_measure, il modello migliore ? Naive Bayes (0.7833247).



