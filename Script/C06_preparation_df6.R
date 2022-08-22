#### FIRST LOOK of df_6 ####
# df_6_camp_event ha al suo interno informazioni relative alla comunicazione marketing inviata dall'azienda ai 
# clienti tramite email. 

str(df_6_camp_event)
# campi presenti in df_6_camp_event: ID_EVENT (risposta all'evento), ID_CLI (id cliente)
# ID_CAMP (id campagna), ID_DELIVERY (identifica consegna), TYP_EVENT (identifica azioni successive all'invio della mail, 
# S= mail inviata, V= mail aperta, C= click sulla mail per visualizzare contenuto,
# B= se host del cliente non ha permesso la consegna del messaggio, E= errore
# recapito della mail), EVENT_DATE (data di invio della comunicazione marketing). 

summary(df_6_camp_event)
#breve descrizione della composizione delle variabili presenti. 

#### START CLEANING df_6 ####

df_6_camp_event_clean <- df_6_camp_event

#### CLEANING DATA TYPES in df_6 ####

## formatting dates and times ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(EVENT_DATETIME = as.POSIXct(EVENT_DATE, format="%Y-%m-%dT%H:%M:%S")) %>%
  mutate(EVENT_HOUR = hour(EVENT_DATETIME)) %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATETIME))

#### CONSISTENCY CHECK ID_CLI in df_1/df_6 ####

cons_idcli_df1_df6 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_6) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df6

#!!! NOTE: all ID_CLI in df_6 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_6 !!!#
# Non tutti i clienti sono stati soggetti a campagna marketing. 

#### CONSISTENCY CHECK ID_CAMP in df_5/df_6 ####

cons_idcamp_df5_df6 <- df_5_camp_cat_clean %>%
  select(ID_CAMP) %>%
  distinct() %>%
  mutate(is_in_df_5 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CAMP) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CAMP"
  ) %>%
  group_by(is_in_df_5, is_in_df_6) %>%
  summarize(NUM_ID_CAMPs = n_distinct(ID_CAMP)) %>%
  as.data.frame()

cons_idcamp_df5_df6

#!!! NOTE: all ID_CAMP in df_6 are mapped in df_5, but not all ID_CAMP in df_5 are mapped in df_6 !!!#
# Non tutti i clienti che hanno accettato di ricevere campagne marketing hanno effettivamente ricevuto tale campagna.

#### RESHAPING df_6 ####

## remapping TYPE_EVENT values "E" [ERROR] and "B" [BOUNCE] into a level "F" [FAILURE] ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(TYP_EVENT = as.factor(if_else(TYP_EVENT == "E" | TYP_EVENT == "B", "F", as.character(TYP_EVENT))))

## adding type from df_5 ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP")

## organize the data adding to each sending event the corresponding opens/clicks/fails

# sends
df_sends <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "S") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_S = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , SEND_DATE = EVENT_DATE) %>%
  as.data.frame()

# opens
# there could be multiple opens of the same communication
# 1- count the open events
# 2- consider explicitely only the first open

df_opens_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "V") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_O = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , OPEN_DATETIME = EVENT_DATETIME
         , OPEN_DATE = EVENT_DATE)

total_opens <- df_opens_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarize(NUM_OPENs = n_distinct(ID_EVENT_O))
  
df_opens <- df_opens_prep %>%
  left_join(total_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(OPEN_DATETIME == min(OPEN_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

head(df_opens)

# clicks
# there could be multiple clicks of the same communication
# 1- count the click events
# 2- consider explicitely only the first click

df_clicks_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "C") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_C = ID_EVENT
       , ID_CLI
       , ID_CAMP
       , TYP_CAMP
       , ID_DELIVERY
       , CLICK_DATETIME = EVENT_DATETIME
       , CLICK_DATE = EVENT_DATE)

total_clicks <- df_clicks_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarize(NUM_CLICKs = n_distinct(ID_EVENT_C))

df_clicks <- df_clicks_prep %>%
  left_join(total_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(CLICK_DATETIME == min(CLICK_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# fails
df_fails <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "F") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_F = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , FAIL_DATETIME = EVENT_DATETIME
         , FAIL_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(FAIL_DATETIME == min(FAIL_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

# combine sends opens clicks and fails
df_6_camp_event_clean_final <- df_sends %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  left_join(df_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(CLICK_DATE) | OPEN_DATE <= CLICK_DATE) %>%
  left_join(df_fails
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(FAIL_DATE) | SEND_DATE <= FAIL_DATE) %>%
  mutate(OPENED = !is.na(ID_EVENT_O)) %>%
  mutate(CLICKED = !is.na(ID_EVENT_C)) %>%
  mutate(FAILED = !is.na(ID_EVENT_F)) %>%
  mutate(DAYS_TO_OPEN = as.integer(OPEN_DATE - SEND_DATE)) %>%
  select(ID_EVENT_S
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , SEND_DATE
         
         , OPENED
         , OPEN_DATE
         , DAYS_TO_OPEN
         , NUM_OPENs
         
         , CLICKED
         , CLICK_DATE
         , NUM_CLICKs
         
         , FAILED
         )
# In df_6_camp_event_clean_final andiamo ad unire i dataframe precedentemente creati.


#### EXPLORE VARIABLES in df_6 ####

### GENERAL OVERVIEW ###

## compute aggregate
df6_overview <- df_6_camp_event_clean_final %>% 
  summarize(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overview
# Il periodo della campagna è compreso tra 3/1/19 e 30/4/2019. il feeback ricevuto è pari a 1556646 e 190427 clienti
# sono stato soggetti a una campagna marketing.

### GENERAL OVERVIEW by TYP_CAMP ###

## compute aggregate
df6_overviewbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP) %>%
  summarize(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overviewbytyp

# "National"(177153) è la campagna che ha raggiunto il maggior numero di clienti con 833085 feedback

## plot aggregate
plot_df6_overviewbytyp <- (
  ggplot(data=df6_overviewbytyp
         , aes(x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_overviewbytyp

### Variable OPENED ###

## compute aggregate
df6_dist_opened <- df_6_camp_event_clean_final %>%
  group_by(OPENED) %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview$TOT_CLIs)

df6_dist_opened
# Il 17,9% delle email è stato aperto(278.382), 83420 clienti su 261.798 hanno aperto la mail. 

## plot aggregate
plot_df6_dist_opened <- (
  ggplot(data=df6_dist_opened
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    theme_minimal()
)

plot_df6_dist_opened


### Variable OPENED by TYP_CAMP ###

## compute aggregate
df6_dist_openedbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, OPENED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , OPENED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_openedbytyp
#la campagna National è quella con il maggior numero di aperture e anche con il maggior numero di non aperture

## plot aggregate
plot_df6_dist_openedbytyp <- (
  ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df6_dist_openedbytyp

## plot aggregate percent
plot_df6_dist_openedbytyp_percent <- (
  ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
)

plot_df6_dist_openedbytyp_percent

# le campagne "Product" e "Personalized" sono quelle migliori sotto l'aspetto delle percentuali sulle aperture
#in relazione agli invii. 

### Variable DAYS_TO_OPEN

## compute aggregate
df6_dist_daystoopen <- df_6_camp_event_clean_final %>%
  filter(OPENED) %>%
  group_by(ID_CLI) %>%
  summarize(AVG_DAYS_TO_OPEN = floor(mean(DAYS_TO_OPEN))) %>%
  ungroup() %>%
  group_by(AVG_DAYS_TO_OPEN) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))

df6_dist_daystoopen
# 12221 clienti aprono la mail dopo un giorno che gli è stata inviata, mentre la maggior parte di loro
# la apre subito.

## plot aggregate
plot_df6_dist_daystoopen <- (
  ggplot(data=df6_dist_daystoopen %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_dist_daystoopen

### DAYS_TO_OPEN vs CUMULATE PERCENT ###

## compute aggregate
df6_dist_daystoopen_vs_cumulate <- df6_dist_daystoopen %>%
  arrange(AVG_DAYS_TO_OPEN) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

## plot aggregate
plot_df6_dist_daystoopen_vs_cumulate <- (
  ggplot(data=df6_dist_daystoopen_vs_cumulate %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=PERCENT_COVERED)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14) +
    theme_minimal()
)

plot_df6_dist_daystoopen_vs_cumulate

#l'80% dei clienti apre la mail entro un giorno.

#### ???? TO DO df_6 ???? ####
# EXPLORE the following relevant variables in df_6_camp_event_clean_final:

# - CLICKED/CLICKED by TYP_CAMP
## compute aggregate
df6_dist_clickedbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, CLICKED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , CLICKED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_clickedbytyp


## plot aggregate
plot_df6_dist_clickedbytyp <- (
  ggplot(data=df6_dist_clickedbytyp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)

plot_df6_dist_clickedbytyp

## plot aggregate percent
plot_df6_dist_clickedbytyp_percent <- (
  ggplot(data=df6_dist_clickedbytyp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
)

plot_df6_dist_clickedbytyp_percent

# "Product" è la campagna con il maggior numero di click in percentuale. Questi risultati
# sono simili a quelli precedenti, da ciò si può desumere una sorta di correlazione.


# - FAILED/FAILED by TYP_CAP
## compute aggregate
df6_dist_failedbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, FAILED)  %>%
  summarize(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , FAILED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_failedbytyp

#National presenta il maggior numero in valor assoluto per quanto riguarda le mail "fallite"

##plot aggregate 
plot_df6_dist_failedbytyp <- (
  ggplot(data=df6_dist_failedbytyp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()
)
plot_df6_dist_failedbytyp 

## plot aggregate percent
plot_df6_dist_failedbytyp_percent <- (
  ggplot(data=df6_dist_failedbytyp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    theme_minimal()
)

plot_df6_dist_failedbytyp_percent
#Personalized è la campagna che ottiene i migliori risultati in termini percentuali.

# - NUM_OPENs
## compute aggregate
df6_dist_numopens <- df_6_camp_event_clean_final %>%
  filter(OPENED) %>%
  group_by(ID_CLI) %>%
  summarize(AVG_NUM_OPENs = floor(mean(NUM_OPENs))) %>%
  ungroup() %>%
  group_by(AVG_NUM_OPENs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI))

df6_dist_numopens
# 73265 clienti aprono la mail una sola volta.

## plot aggregate
plot_df6_dist_numopens <- (
  ggplot(data=df6_dist_numopens %>%
           filter(AVG_NUM_OPENs < 6)
         , aes(x=AVG_NUM_OPENs, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal()
)

plot_df6_dist_numopens

### NUM_OPENs vs CUMULATE PERCENT ###

## compute aggregate
df6_dist_numopens_vs_cumulate <- df6_dist_numopens %>%
  arrange(AVG_NUM_OPENs) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

## plot aggregate
plot_df6_dist_numopens_vs_cumulate <- (
  ggplot(data=df6_dist_numopens_vs_cumulate %>%
           filter(AVG_NUM_OPENs < 10)
         , aes(x=AVG_NUM_OPENs, y=PERCENT_COVERED)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks=seq(0,10,1), minor_breaks=0:10) +
    theme_minimal()
)

plot_df6_dist_numopens_vs_cumulate
# Dal presente grafico, è possibile notare che quasi il 97,5% dei clienti apre le mail al massimo due volte,
# non di più.

# - NUM_CLICKs

df6_dist_numclicks <-  df_6_camp_event_clean_final         %>%
  group_by(NUM_CLICKs)                        %>% 
  summarize(TOT_ID = n_distinct(ID_EVENT_S)) %>% 
  mutate(PERCENT = TOT_ID/sum(TOT_ID))       %>% 
  arrange(desc(PERCENT))                         

df6_dist_numclicks

ggplot(data = df6_dist_numclicks,
       aes(x = NUM_CLICKs,
           y = TOT_ID)) +       
  geom_bar(stat = "identity",
           fill = "steelblue") + 
  xlim(0, 10) +                  
  theme_minimal()  

#### FINAL REVIEW df_6_clean ####

str(df_6_camp_event_clean_final)

summary(df_6_camp_event_clean_final)