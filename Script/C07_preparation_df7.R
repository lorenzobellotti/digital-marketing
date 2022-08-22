#### FIRST LOOK of df_7 ####
# df_7_tic ha al suo interno gli acquisti e i resi per ciascun cliente.


str(df_7_tic)
# abbiamo 9 campi: ID SCONTRINO (id transazione), ID_CLI (id cliente),
#ID_NEG (id del negozio), ID_ARTICOLO (id acquistato o restituito), 
#COD_REPARTO (id del reparto relativo all'articolo), -DIREZIONE(variabile binaria con acquisto(1) e non acquisto(-1)),
#IMPORTO_LORDO (definito come la somma di importo netto e sconto applicato), SCONTO (sconto effettuato),
#DATETIME: data della transazione.

summary(df_7_tic)
#breve descrizione della composizione delle variabili presenti. 

#### START CLEANING df_7 ####

df_7_tic_clean <- df_7_tic

#### CLEANING DATA TYPES in df_7 ####

## formatting dates and times ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(TIC_DATETIME = as.POSIXct(DATETIME, format="%Y-%m-%dT%H%M%S")) %>%
  mutate(TIC_HOUR = hour(TIC_DATETIME)) %>%
  mutate(TIC_DATE = as.Date(TIC_DATETIME)) %>%
  select(-DATETIME)

## formatting boolean as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(DIREZIONE = as.factor(DIREZIONE))

## formatting numerical categories as factor ##
df_7_tic_clean <- df_7_tic_clean %>%
  mutate(COD_REPARTO = as.factor(COD_REPARTO))

#### CONSISTENCY CHECK ID_CLI in df_1/df_7 ####

cons_idcli_df1_df7 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_7_tic_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_7 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_7) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df7

#!!! NOTE: all ID_CLI in df_7 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_7 !!!#  
# non tutti i clienti dell'azienda hanno fatto un acquisto nella finestra temporale considerata.

#### RESHAPING df_7 ####

df_7_tic_clean_final <- df_7_tic_clean %>%
  ## adding day characterization ##
  mutate(TIC_DATE_WEEKDAY = wday(TIC_DATE)) %>%
  mutate(TIC_DATE_HOLIDAY = isHoliday("Italy", TIC_DATE)) %>%
  mutate(TIC_DATE_TYP = case_when(
    (TIC_DATE_WEEKDAY %in% c(6,7)) ~ "weekend"
    , (TIC_DATE_HOLIDAY == TRUE) ~ "holiday"
    , (TIC_DATE_WEEKDAY < 7) ~ "weekday"
    , TRUE ~ "other"
    )
  )

#### EXPLORE VARIABLES in df_7 ####

### GENERAL OVERVIEW ###

## compute aggregate
df7_overview <- df_7_tic_clean_final %>% 
  summarize(MIN_DATE = min(TIC_DATE)
            , MAX_DATE = max(TIC_DATE)
            , TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI))

df7_overview
# All'interno di df7_overview abbiamo 998035 diversi ID_SCONTRINO con 212124 clienti unici.

### Variable DIREZIONE ###

## compute aggregate
df7_dist_direction <- df_7_tic_clean_final %>%
  group_by(DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_TICs = TOT_TICs/df7_overview$TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/df7_overview$TOT_CLIs)

df7_dist_direction
# il 9% delle transazioni è costituito da restituzioni, il resto è costituito da acquisti.

### Variable TIC_HOURS ###

## compute aggregate
df7_dist_hour <- df_7_tic_clean_final %>%
  group_by(TIC_HOUR, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_hour

## plot aggregate
plot_df7_dist_hour <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_hour
# i clienti concentrano i loro acquisti nella fascia oraria compresatra le 18.00 e le 19.00

## plot aggregate percent
plot_df7_dist_hour_percent <- (
  ggplot(data=df7_dist_hour
         , aes(fill=DIREZIONE, x=TIC_HOUR, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_hour_percent


### Variable COD_REPARTO ###

## compute aggregate
df7_dist_dep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
            ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
    select(-ALL_TOT_TICs, -ALL_TOT_CLIs)
    
df7_dist_dep

## plot aggregate
plot_df7_dist_dep <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_dep
# il reparto 3 e il 10 sono quelli dai quali proviene il maggior numero di resi

## plot aggregate percent
plot_df7_dist_dep_percent <- (
  ggplot(data=df7_dist_dep
         , aes(fill=DIREZIONE, x=COD_REPARTO, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_dep_percent

### Variable TIC_DATE_TYP ###

## compute aggregate
df7_dist_datetyp <- df_7_tic_clean_final %>%
  group_by(TIC_DATE_TYP, DIREZIONE) %>%
  summarize(TOT_TICs = n_distinct(ID_SCONTRINO)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df7_dist_direction %>%
              select(DIREZIONE
                     , ALL_TOT_TICs = TOT_TICs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by = 'DIREZIONE'
  ) %>%
  mutate(PERCENT_TICs = TOT_TICs/ALL_TOT_TICs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(-ALL_TOT_TICs, -ALL_TOT_CLIs)

df7_dist_datetyp

## plot aggregate
plot_df7_dist_datetyp <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df7_dist_datetyp
# il maggior numero di acquisti viene effettuato durante giorni infrasettimanali

## plot aggregate percent
plot_df7_dist_datetyp_percent <- (
  ggplot(data=df7_dist_datetyp
         , aes(fill=DIREZIONE, x=TIC_DATE_TYP, y=TOT_TICs)) +
    geom_bar(stat="identity", position="fill" ) +
    theme_minimal()
)

plot_df7_dist_datetyp_percent

### Variable average IMPORTO_LORDO and average SCONTO per TICKET ###

## compute aggregate
df7_dist_importosconto <- df_7_tic_clean_final %>%
  group_by(ID_SCONTRINO, DIREZIONE) %>%
  summarize(IMPORTO_LORDO = sum(IMPORTO_LORDO)
            , SCONTO = sum(SCONTO)) %>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto <- df7_dist_importosconto %>%
  group_by(DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))

df7_dist_avgimportosconto
# L'importo lordo medio degli acquisti è pari a 164, quello dei resi è di 110. per quanto riguarda gli sconti
# abbiamo uno sconto medio per gli acquisti di 11.8 mentre per i resi tale sconto è di 8.29


## plot aggregate
plot_df7_dist_importo <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((IMPORTO_LORDO > -1000) & (IMPORTO_LORDO < 1000))
         , aes(color=DIREZIONE, x=IMPORTO_LORDO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_importo
# la gran parte dei clienti spende meno di 500 euro per operazione

## plot aggregate
plot_df7_dist_sconto <- (
  ggplot(data=df7_dist_importosconto %>%
           filter((SCONTO > -250) & (IMPORTO_LORDO < 50))
         , aes(color=DIREZIONE, x=SCONTO)) +
    geom_histogram(binwidth=10, fill="white", alpha=0.5) +
    theme_minimal()
)

plot_df7_dist_sconto
#come da attese, la maggior parte dei clienti non ha grossi sconti

#### ???? TO DO df_7 ???? ####
# EXPLORE average IMPORTO_LORDO and average SCONTO by COD_REPARTO

df7_dist_avgimportosconto_cod_rep <- df_7_tic_clean_final %>%
  group_by(COD_REPARTO, DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))%>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_cod_rep

#plot Importo Lordo by Cod_Reparto
plot_df7_importolordo_codrep <- (
  ggplot(data=df7_dist_avgimportosconto_cod_rep, aes(fill=DIREZIONE, x=COD_REPARTO, y=AVG_IMPORTO_LORDO)) +
    geom_bar(stat="identity") +
    theme_minimal()
)
plot_df7_importolordo_codrep 
# Gli acquisti con l'importo maggiore provengono dal reparto 2 e 6 con i resi maggiori che provengono dal reparto 2


#plot Sconto by Cod_Reparto
plot_df7_sconto_codrep <- (
  ggplot(data=df7_dist_avgimportosconto_cod_rep, aes(fill=DIREZIONE, x=COD_REPARTO, y=AVG_SCONTO)) +
    geom_bar(stat="identity") +
    theme_minimal()
)
plot_df7_sconto_codrep

# ID_ARTICOLO DISTRIBUTIONS (i.e. num TICs by ID_ARTICOLO)
head(df_7_tic_clean_final)

df7_dist_idarticolo <-  df_7_tic_clean_final %>%
  group_by(ID_ARTICOLO) %>%
  summarize(NUM_VENDITE = n_distinct(ID_SCONTRINO)) %>%
  mutate(PERCENT = NUM_VENDITE/sum(NUM_VENDITE))%>%
  arrange(desc(PERCENT)) %>%
  filter(row_number() <= 10)%>%
  as.data.frame()
head(df7_dist_idarticolo)
str(df7_dist_idarticolo)

df7_dist_idarticolo$ID_ARTICOLO<- as.factor(df7_dist_idarticolo$ID_ARTICOLO)

plot_df7_dist_idarticolo<- (
  ggplot(data = df7_dist_idarticolo,
         aes(x = ID_ARTICOLO, y = NUM_VENDITE)) +
         geom_bar(stat = "identity", color="blue", fill="steelblue")
) 
plot_df7_dist_idarticolo
# Il grafico mostra la top 10 degli articoli maggiormente venduti, l'aticolo maggiormente venduto è con ID 33700716
# con un totale di un totale di 58010 vendite


# EXPLORE average IMPORTO_LORDO and average SCONTO per ID_CLI

df7_dist_avgimportosconto_idcli<- df_7_tic_clean_final %>%
  group_by(ID_CLI, DIREZIONE) %>%
  summarize(AVG_IMPORTO_LORDO = mean(IMPORTO_LORDO)
            , AVG_SCONTO = mean(SCONTO))%>%
  ungroup() %>%
  as.data.frame()

df7_dist_avgimportosconto_idcli


#IMPORTO_LORDO
ggplot(data = df7_dist_avgimportosconto_idcli %>% filter((AVG_IMPORTO_LORDO > -1000) & (AVG_IMPORTO_LORDO < 1000)), aes(fill = DIREZIONE, x= AVG_IMPORTO_LORDO)) +
  geom_histogram(binwidth=20) +
  theme_minimal()
#solo una parte marginale dei clienti spende per un importo lordo maggiore di 500 euro

#SCONTO
ggplot(data = df7_dist_avgimportosconto_idcli %>% filter((AVG_SCONTO > -250) & (AVG_SCONTO < 250)), aes(fill = DIREZIONE, x = AVG_SCONTO)) +
  geom_histogram(binwidth = 10) +
  theme_minimal()
#La maggior parte degli sconti applicati è minore di 50 euro 

# compute the distribution of customers by number of purchases (as described in the slides)
df7_dist_customers_purchases <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(NUM_PURCHASES = n_distinct(ID_SCONTRINO)) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(desc(NUM_PURCHASES))

ggplot(data = df7_dist_customers_purchases %>% filter(NUM_PURCHASES <= 20), aes(x= NUM_PURCHASES)) +
  geom_histogram(binwidth=1) +
  theme_minimal()

# il grafico mostra che la maggior parte dei clienti ha effettuato un solo acquisto.

# compute the days for next purchase curve (as described in the slides)
# Daysfor Next Purchase Curve: Distribution of customers by the average difference in days between a purchase and the next

##NEXT PURCHASE CURVE 
# compute the days for next purchase curve (as described in the slides)
data_for_next_purchase <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>% 
  select(ID_CLI,
         ID_ARTICOLO,
         ID_SCONTRINO,
         TIC_DATE,
         DIREZIONE)      %>% 
  arrange(ID_CLI)


head(data_for_next_purchase,10)

df_np <- data_for_next_purchase %>%
  group_by(ID_CLI) %>%
  mutate(lagged = lag(TIC_DATE , 1 ))

df_np <- df_np %>%
  mutate(lagged = if_else(is.na(lagged), TIC_DATE, lagged))

df_np <- df_np %>%
  mutate(Diff = as.numeric(TIC_DATE) - as.numeric(lagged))
head(df_np)
# otteniamo i giorni che intercorrono tra un acquisto e l'altro.

dummy=df_np %>%
  group_by(ID_CLI,TIC_DATE) %>%
  summarize(days_between=sum(Diff)) %>%
  slice(-1)
head(dummy,20)

# Media dei giorni trascorsi tra un acquisto e l'altro per ciascun cliente
dummy=dummy %>%
  group_by(ID_CLI) %>%
  summarize(avg_days_between=round(mean(days_between),0))
head(dummy)


x <- as.data.frame(table(dummy$avg_days_between))
x$Perc <- x$Freq/sum(x$Freq)
ggplot(x, 
       aes(x = as.numeric(Var1),
           y = cumsum(Perc))) +
  labs(title = "Next Purchase Curve",
       x = "Last Purchase Date (in Days)",
       y = "Cumulative Percent") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +    
  scale_x_continuous(limits=c(0,185), breaks = seq(0, 185, 25)) +     
  geom_vline(xintercept = 95, linetype = "dotted") +
  geom_line(size = 1)

max(dummy$avg_days_between)

x$cum <- (cumsum(x$Perc))
View(x)     
# Il 90% dei clienti effettua un acquisto nei 95 giorni successivi ad un altro acquisto..


# distribuzione scontrini per mese
dist_scontrini <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1) %>%
  group_by(Mese = paste(year(TIC_DATETIME), month(TIC_DATETIME), sep="-")) %>%
  summarise(n = n_distinct(ID_SCONTRINO))

d2 <- as.data.frame(rbind(
  cbind("Maggio 2018", 63601),
  cbind("Giugno 2018", 65651),
  cbind("Luglio 2018", 69052),
  cbind("Agosto 2018", 70829),
  cbind("Settembre 2018", 73766),
  cbind("Ottobre 2018", 79970),
  cbind("Novembre 2018", 92383),
  cbind("Dicembre 2018", 82476),
  cbind("Gennaio 2019", 73830),
  cbind("Febbraio 2019", 72410),
  cbind("Marzo 2019", 89348),
  cbind("Aprile 2019", 74530)
))

d2$V1 <- factor(d2$V1, levels = d2$V1)

ggplot(data=d2, aes(x=V1, y=V2)) +
  geom_bar(stat="identity", fill="darkblue", colour="black") +
  labs(title = "Distribuzione Scontrini per Mese", x = "Mese", y = "# Scontrini") +
  theme_minimal()

# Novembre 2018 e Marzo 2019 sono i mesi caratterizzati dal maggior numero di acquisti

#### FINAL REVIEW df_7_clean ####

str(df_7_tic_clean_final)
summary(df_7_tic_clean_final)
# nel seguente script vengono effettuate diverse operazioni esplorative al fine di individuare
# diverse caratteristiche descrittive delle transazioni presenti.

