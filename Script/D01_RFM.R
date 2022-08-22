library(rfm)
library(gridExtra)

#RFM MODEL
#L' analisi RFM consente di classificare gli acquirenti usando come criterio lo storico delle transazioni,
# rispondendo a domande quali: 
# Quando e' stata l'ultima volta che i clienti hanno acquistato? - RECENCY 
# Quante volte il cliente effettua l'acquisto?  - FREQUENCY 
# Quanti soldi spende ogni cliente, in un determinato periodo di tempo? - MONETARY 
# Pertanto, lo sviluppo del presente modello consente all'azienda di rivolgersi in modo piu' preciso e customizzato a ogni differente
# categoria di clienti.


# Per sviluppare tale modello e' stato scelto di utilizzare i dati relativi alle transazioni avvenute nel 2019

#Creazione del dataset di riferimento scontrini_2019
scontrini_2019 <- df_7_tic_clean_final %>%
  filter(TIC_DATE > as.Date("01/01/2019", format = "%d/%m/%Y"))
head(scontrini_2019)


df_7_tic_clean_final %>%
  filter(TIC_DATE == max(TIC_DATE))  #2019-04-30 ultima data presente nel dataset 

#### RECENCY (R) - Tempo trascorso dall'ultimo acquisto
recency <- scontrini_2019 %>%
  filter(DIREZIONE == 1) %>%   # si e' ritenuto di considerare esclusivamente gli acquisti
  group_by(ID_CLI) %>%
  summarise(LAST_PURCHASE = max(TIC_DATE))

recency$RECENCY <- difftime(as.Date("30/04/2019", format = "%d/%m/%Y"), recency$LAST_PURCHASE, units = c("days"))
View(recency)  
# Per ogni ID cliente e' riportata l'ultima data di acquisto e la differenza di quest'ultima con il periodo di analisi, espressa in giorni.

## Suddivisione della customer base in clienti attivi e clienti inattivi 
## Si ritiene di lavorare esclusivamente sui clienti attivi 
recency <- recency %>% filter(recency$RECENCY<=95) 
# Come mostrato dal grafico relativo alla purchase curve implementato nello script denominato "C07_preparation_df7.R", 
# il 90% dei clienti riacquista dopo 95 gg.


#### FREQUENCY (F) - Numero totale di acquisti di ogni cliente
scontrini_2019_filtrati=scontrini_2019  %>%   # ridefinizione del dataset considerando solo i clienti attivi
  filter(ID_CLI %in% recency$ID_CLI)
View(scontrini_2019_filtrati)

frequency <- scontrini_2019_filtrati %>%
  filter(DIREZIONE == 1) %>%   # si e' ritenuto di considerare esclusivamente gli acquisti 
  group_by(ID_CLI)  %>%
  summarise(N_TRANSAZIONI = n_distinct(ID_SCONTRINO))
View(frequency)
# Per ogni ID cliente e' riportato il numero di transazioni effettuate.


#### MONETARY (M) - Valore monetario totale relativo a ogni cliente
monetary <- scontrini_2019_filtrati %>%   
  group_by(ID_CLI) %>%
  summarize(MONETARY_VALUE = sum((IMPORTO_LORDO)-(SCONTO))) %>%  
  ungroup() %>%
  as.data.frame()

monetary <- monetary %>% filter(MONETARY_VALUE > 0.01)
View(monetary)
# Il valore monetario e' stato calcolato come la somma del totale degli importi netti (importo lordo-sconto) 
# pagati dal cliente. 
# In questo caso e' stato scelto di rimuovere il vincolo DIREZIONE==1, affinche' i refunds vengano conteggiati nel monetary value 
# del cliente.
# Successivamente, si e' imposto il vincolo MONETARY_VALUE > 0.01 per evitare di considerare anche i casi in cui l'importo era negativo 
# o troppo piccolo per avere valore.


# IN seguito, si e' provveduto alla divisione in gruppi della customer base utilizzando i percentili:
#Recency
recency <- within(recency,
                  RECENCY_GROUP <- cut(as.numeric(recency$RECENCY),
                                       breaks = quantile(recency$RECENCY, probs = c(0, .25, .75, 1)),
                                       include.lowest = T,
                                       labels = c("LOW", "MEDIUM", "HIGH")))

recency_counts <- as.data.frame(table(recency$RECENCY_GROUP))
recency_counts

#Frequency 
frequency <- within(frequency,
                    FREQUENCY_GROUP <- cut(frequency$N_TRANSAZIONI,
                                           breaks = quantile(frequency$N_TRANSAZIONI, probs = c(0, .50, .75, 1)),
                                           include.lowest = T,
                                           right = F,
                                           labels = c("LOW", "MEDIUM", "HIGH")))

quantile(frequency$N_TRANSAZIONI,probs = seq(0, 1, 1/20))
frequency_counts <- as.data.frame(table(frequency$FREQUENCY_GROUP))
frequency_counts


#Monetary 
monetary <- within(monetary,
                   MONETARY_GROUP <- cut(monetary$MONETARY_VALUE,
                                         breaks = quantile(monetary$MONETARY_VALUE, probs = c(0, .25, .75, 1)),
                                         include.lowest = T,
                                         labels = c("LOW", "MEDIUM", "HIGH")))

monetary_counts <- as.data.frame(table(monetary$MONETARY_GROUP))
monetary_counts


##DONUGHT CHART - RECENCY CLASSES DISTRIBUTION 
# Create test chart_recency_classes.
chart_recency_classes <- data.frame(
  category=c("Low", "Medium", "High"),
  count=c(recency_counts$Freq)
)
str(chart_recency_classes)
# Compute percentages
chart_recency_classes$fraction = chart_recency_classes$count / sum(chart_recency_classes$count)
# Compute the cumulative percentages (top of each rectangle)
chart_recency_classes$ymax = cumsum(chart_recency_classes$fraction)
# Compute the bottom of each rectangle
chart_recency_classes$ymin = c(0, head(chart_recency_classes$ymax, n=-1))

# Make the plot
chart_recency_classes$label <- paste0(chart_recency_classes$category, "\n  ", chart_recency_classes$count)
chart_recency_classes$labelPosition <- (chart_recency_classes$ymax + chart_recency_classes$ymin) / 2

ggplot(chart_recency_classes, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=5) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution Recency Classes") 


##DONUGHT CHART - FREQUENCY CLASSES DISTRIBUTION 
# Create test chart_frequency_classes.
chart_frequency_classes <- data.frame(
  category=c("Low", "Medium", "High"),
  count=c(frequency_counts$Freq)
)
str(chart_frequency_classes)
# Compute percentages
chart_frequency_classes$fraction = chart_frequency_classes$count / sum(chart_frequency_classes$count)
# Compute the cumulative percentages (top of each rectangle)
chart_frequency_classes$ymax = cumsum(chart_frequency_classes$fraction)
# Compute the bottom of each rectangle
chart_frequency_classes$ymin = c(0, head(chart_frequency_classes$ymax, n=-1))

# Make the plot
chart_frequency_classes$label <- paste0(chart_frequency_classes$category, "\n  ", chart_frequency_classes$count)
chart_frequency_classes$labelPosition <- (chart_frequency_classes$ymax + chart_frequency_classes$ymin) / 2

ggplot(chart_frequency_classes, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=5) +
  scale_fill_brewer(palette="Set2") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution Frequency Classes") 


##DONUGHT CHART - MONETARY CLASSES DISTRIBUTION 
# Create test chart_monetary_classes.
chart_monetary_classes <- data.frame(
  category=c("Low", "Medium", "High"),
  count=c(monetary_counts$Freq)
)
str(chart_monetary_classes)
# Compute percentages
chart_monetary_classes$fraction = chart_monetary_classes$count / sum(chart_monetary_classes$count)
# Compute the cumulative percentages (top of each rectangle)
chart_monetary_classes$ymax = cumsum(chart_monetary_classes$fraction)
# Compute the bottom of each rectangle
chart_monetary_classes$ymin = c(0, head(chart_monetary_classes$ymax, n=-1))

# Make the plot
chart_monetary_classes$label <- paste0(chart_monetary_classes$category, "\n  ", chart_monetary_classes$count)
chart_monetary_classes$labelPosition <- (chart_monetary_classes$ymax + chart_monetary_classes$ymin) / 2

ggplot(chart_monetary_classes, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=5) +
  scale_fill_brewer(palette="Accent") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution Monetary Classes") 


##Creazione del dataset rfm 
rfm <- merge(frequency, monetary, by = "ID_CLI")
rfm <- merge(rfm, recency, by = "ID_CLI")
head(rfm)

# Si e' creato il density plot al fine di mettere in evidenza la distribuzione:
r <- ggplot(rfm) +geom_density(aes(x= RECENCY))
f <- ggplot(rfm) +geom_density(aes(x = N_TRANSAZIONI)) 
m <- ggplot(rfm) +geom_density(aes(x = MONETARY_VALUE)) 
grid.arrange(r, f, m, nrow = 3)

summary(rfm)
# Da tale operazione e' possibile notare la presenza di molti outliers che influenzano la distribuzione sia in riferimento alla variabile 
# N_TRANSAZIONI, sia a quella denominata MONETARY_VALUE.

#scores 

analysis_date <- lubridate::as_date("2019-04-30")
colnames(rfm)
rfm_2<- rfm[ -c(3,5,8) ]
head(rfm_2)
colnames(rfm_2)
colnames(rfm_2)[c(1,2,3,5)] <- c("customer_id","number_of_orders","revenue","recency_days","analysis_date")

rfm_result<- rfm_table_customer(rfm_2, customer_id, number_of_orders, recency_days, revenue, analysis_date)
rfm_result

str(rfm_2)
rfm_2$recency_days<-as.numeric(rfm_2$recency_days)
str(rfm_result)

#scatterplot monetary+recency
rfm_rm_plot(rfm_result)
#scatterolot frequency+monetary
rfm_fm_plot(rfm_result)
#scatterplot 
rfm_rf_plot(rfm_result)


#Creazione gruppi customer base
rfm$RF <- NA

for(i in c(1:nrow(rfm))){
  if(rfm$RECENCY_GROUP[i] == "LOW" && rfm$FREQUENCY_GROUP[i] == "LOW") rfm$RF[i] <- "One-Timer"
  if(rfm$RECENCY_GROUP[i] == "MEDIUM" && rfm$FREQUENCY_GROUP[i] == "LOW") rfm$RF[i] <- "One-Timer"
  if(rfm$RECENCY_GROUP[i] == "HIGH" && rfm$FREQUENCY_GROUP[i] == "LOW") rfm$RF[i] <- "Leaving"
  if(rfm$RECENCY_GROUP[i] == "HIGH" && rfm$FREQUENCY_GROUP[i] == "MEDIUM") rfm$RF[i] <- "Leaving"
  if(rfm$RECENCY_GROUP[i] == "LOW" && rfm$FREQUENCY_GROUP[i] == "MEDIUM") rfm$RF[i] <- "Engaged"
  if(rfm$RECENCY_GROUP[i] == "MEDIUM" && rfm$FREQUENCY_GROUP[i] == "MEDIUM") rfm$RF[i] <- "Engaged"
  if(rfm$RECENCY_GROUP[i] == "LOW" && rfm$FREQUENCY_GROUP[i] == "HIGH") rfm$RF[i] <- "Top"
  if(rfm$RECENCY_GROUP[i] == "MEDIUM" && rfm$FREQUENCY_GROUP[i] == "HIGH") rfm$RF[i] <- "Top"
  if(rfm$RECENCY_GROUP[i] == "HIGH" && rfm$FREQUENCY_GROUP[i] == "HIGH") rfm$RF[i] <- "Leaving Top"
}

#Distribuzione gruppi recency e frequency 
rf_counts = rfm %>%
  count(RECENCY_GROUP, FREQUENCY_GROUP, RF)
rf_counts

ggplot(rf_counts, aes(x=FREQUENCY_GROUP, y=RECENCY_GROUP, fill=n)) + 
  geom_tile() +
  geom_text(aes(label = n)) +
  scale_fill_distiller(direction=1) +
  theme_minimal()

rf <- as.data.frame(table(rfm$RF)) %>%
  arrange(Freq)
rf

ggplot(data = rf, aes(x=Var1, y=Freq, fill=Var1)) +
  geom_bar(stat = "identity") +
  scale_colour_brewer(palette = "Set2") +
  labs(title = "RF Classes Distribution", x = "RF Classes", y = "N Customers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = FALSE)

#RFM Classes 
rfm$RFM <- NA

for(i in c(1:nrow(rfm))) {
  if(rfm$RF[i] == "One-Timer" && rfm$MONETARY_GROUP[i] == "LOW") rfm$RFM[i] <- "Cheap"
  if(rfm$RF[i] == "Top" && rfm$MONETARY_GROUP[i] == "HIGH") rfm$RFM[i] <- "Diamond"
  
  if(rfm$RF[i] == "Leaving" && rfm$MONETARY_GROUP[i] == "LOW") rfm$RFM[i] <- "Tin"
  if(rfm$RF[i] == "One-Timer" && rfm$MONETARY_GROUP[i] == "MEDIUM") rfm$RFM[i] <- "Tin"
  
  if(rfm$RF[i] == "Engaged" && rfm$MONETARY_GROUP[i] == "LOW") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "Leaving" && rfm$MONETARY_GROUP[i] == "MEDIUM") rfm$RFM[i] <- "Copper"
  if(rfm$RF[i] == "One-Timer" && rfm$MONETARY_GROUP[i] == "HIGH") rfm$RFM[i] <- "Copper"
  
  if(rfm$RF[i] == "Leaving Top" && rfm$MONETARY_GROUP[i] == "LOW") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Engaged" && rfm$MONETARY_GROUP[i] == "MEDIUM") rfm$RFM[i] <- "Bronze"
  if(rfm$RF[i] == "Leaving" && rfm$MONETARY_GROUP[i] == "HIGH") rfm$RFM[i] <- "Bronze"
  
  if(rfm$RF[i] == "Top" && rfm$MONETARY_GROUP[i] == "LOW") rfm$RFM[i] <- "Silver"
  if(rfm$RF[i] == "Leaving Top" && rfm$MONETARY_GROUP[i] == "MEDIUM") rfm$RFM[i] <- "Silver"
  if(rfm$RF[i] == "Engaged" && rfm$MONETARY_GROUP[i] == "HIGH") rfm$RFM[i] <- "Silver"

  if(rfm$RF[i] == "Top" && rfm$MONETARY_GROUP[i] == "MEDIUM") rfm$RFM[i] <- "Gold"
  if(rfm$RF[i] == "Leaving Top" && rfm$MONETARY_GROUP[i] == "HIGH") rfm$RFM[i] <- "Gold"
  } 

#Distribuzione gruppi rf e monetary 
rfm_counts = rfm %>%
  count(RF, MONETARY_GROUP, RFM)

rfm_counts

ggplot(rfm_counts, aes(x = RF, y = MONETARY_GROUP, fill = n)) + 
  geom_tile() +
  geom_text(aes(label = n)) +
  scale_fill_distiller(direction=1) +
  theme_minimal()


rfm_distrib <- as.data.frame(table(rfm$RFM))
rfm_distrib

#RFM Classes Distribution - BarPlot  
ggplot(data = rfm_distrib, aes(x = Var1, y = Freq, fill=Var1)) +
  geom_bar(stat = "identity")+
  scale_colour_brewer() +
  labs(title = "RFM Classes Distribution", x = "RFM Classes", y = "N. Customers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = FALSE)+
  scale_fill_manual("legend", values = c("Bronze" = "yellow", "Cheap" = "black", "Copper" = "#FF6700", "Diamond"="#1E90FF", "Gold"="#3EA055", "Silver"="#77DD77", "Tin"="red"))

# I consumatori "Tin" sono quelli piu' frequenti, seguiti da quelli di tipo "Gold", mentre la classe 
# meno frequente e' quella dei clienti "Silver".

#Per riassumere, sono stati effettuati i seguenti passaggi: 
# Dapprima si e' provveduto alla creazione del dataframe su cui lavorare a partire dal dataset relativo agli scontrini (df7),
# di cui sono stati selezionati i dati relativi al 2019, e da cui sono state create le tre variabili del modello 
# (Recency, Frequency e Monetary).
# Sono state poi create le 3 classi in cui suddividere la customer base (low-medium-high),
# Utilizzando i percentili delle tre distribuzioni, sono stati creati dei donught chart che rappresentasseo la proporzione delle 
# tre classi al fine di visualizzare i risultati ottenuti. 
# Successivamente, e' stato realizzato il dataset rfm, effettuando un merge tra i 3 dataset relativi alle variabili del modello, 
# sulla base della chiave id_cli. Utilizzando tale dataset, sono state realizzate le classi in cui suddividere la customer base. 
# Inoltre, sono stati poi creati una serie di grafici: density plot per visualizzare la distribuzione dei valori delle variabili 
# singolarmente e scatterplot per visualizzare quest'ultima congiuntamente, ls matrice RF e infine la distribuzione 
# dei clienti per ogni classe del modello RFM (bronze, cheap, copper, diamond, gold, silver, tin).



