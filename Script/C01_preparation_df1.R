#### FIRST LOOK of df_1 ####
#Il dataset df_1_cli_fid ha al suo interno informazioni ruardanti le sottoscrizioni al programma fedeltà 

str(df_1_cli_fid)
#Sono presenti 7 campi: ID_CLI (id cliente), ID_FID (id programma fedelta), 
#ID_NEG (id negozio di riferimento), TYP_CLI_FID (variabile binaria che mostra se si tratta di account principale), 
#COD_FID (tipo di programma fedeltà), STATUS_FID (variabile binaria che mostra se si tratta di account attivo o meno), 
#DT_ACTIVE (data attivazione del programma fedeltà)


summary(df_1_cli_fid)
# breve descrizione della composizione delle variabili presenti

#### START CLEANING df_1 ####

df_1_cli_fid_clean <- df_1_cli_fid

#### CLEANING DUPLICATE VALUES in df_1 ####

## check for duplicates
df_1_cli_fid_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ID_FIDs = n_distinct(ID_FID)
            , TOT_ID_CLIFIDs = n_distinct(paste0(as.character(ID_CLI),"-",as.character(ID_FID)))
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates for combination CLI-FID !!!#

#### CLEANING DATA TYPES in df_1 ####

## formatting dates ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

## formatting boolean as factor ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

#### CONSISTENCY CHECK on df1: number of fidelity subscriptions per client ####

## count the subscriptions for each client
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarize(NUM_FIDs =  n_distinct(ID_FID)
            , NUM_DATEs = n_distinct(DT_ACTIVE)
            )

tot_id_cli <- n_distinct(num_fid_x_cli$ID_CLI)
# Il dataset contiene 369472 clienti distinti.

## compute the distribution of number of subscriptions
dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_CLIs = TOT_CLIs/tot_id_cli)

dist_num_fid_x_cli

#!!! NOTE: there are clients with multiple fidelity subscriptions !!!#
# Lo 0,2% dei clienti ha un 2 o più carte fedeltà 

## let examine in details clients with multiple subscriptions

num_fid_x_cli %>% filter(NUM_FIDs == 3)

# each subscription can have different dates
df_1_cli_fid %>% filter(ID_CLI == 621814)
# there could be subscriptions at the same dates [possibly for technical reasons]
df_1_cli_fid %>% filter(ID_CLI == 320880)

#### RESHAPING df_1 ####

## combining information

# from first subscription  --> registration date, store for registration
# from last subscription   --> type of fidelity, status
# from subscriptions count --> number of subscriptions made

df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()

df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  select(ID_CLI
         , ID_FID
         , LAST_COD_FID = COD_FID
         , LAST_TYP_CLI_FID = TYP_CLI_FID
         , LAST_STATUS_FID = STATUS_FID
         , LAST_DT_ACTIVE = DT_ACTIVE) %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI
                     , FIRST_ID_NEG = ID_NEG
                     , FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI
                     , NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')

#### EXPLORE COLUMNS of df_1 ####

### variable LAST_COD_FID ###

## compute distribution
df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(LAST_COD_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_codfid
# il 78,4% del totale dei clienti ha sottoscritto un programma di tipo "STANDARD". 

## plot distribution
plot_df1_dist_codfid <- (
  ggplot(data=df1_dist_codfid
         , aes(x=LAST_COD_FID, y=TOT_CLIs)
         ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_codfid 

#### ???? TO DO df_1 ???? ####
# EXPLORE the remaining df_1_cli_fid_clean relevant variables

# 1. Variabile LAST_STATUS_FID
df1_dist_statusfid <- df_1_cli_fid_clean %>%
  group_by(LAST_STATUS_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_statusfid
#il 99,2% dei clienti ha un account attivo (1)

## plot distribution
plot_df1_dist_statusfid <- (
  ggplot(data=df1_dist_statusfid
         , aes(x=LAST_STATUS_FID, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_statusfid

# 2. Variabile NUM_FIDS
df1_dist_numfids <- df_1_cli_fid_clean %>%
  group_by(NUM_FIDs) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_numfids
#il 99,8% dei clienti ha una sola carta fedelta' 

## plot distribution
plot_df1_dist_numfids <- (
  ggplot(data=df1_dist_numfids
         , aes(x=NUM_FIDs, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_numfids

# 3. Variabile FIRST_ID_NEG
df1_dist_firstidneg <- df_1_cli_fid_clean %>%
  group_by(FIRST_ID_NEG) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_firstidneg
#il negozio con il maggior numero di sottoscrizioni e' ID_NEG 1 

## plot distribution
plot_df1_dist_firstidneg <- (
  ggplot(data=df1_dist_firstidneg
         , aes(x= FIRST_ID_NEG, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_firstidneg

# 4. Variabile LAST_TYP_CLI_FID
df1_dist_lasttypclifid <- df_1_cli_fid_clean %>%
  group_by(LAST_TYP_CLI_FID) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_lasttypclifid
#il 98,5% dei clienti ha un account principale

## plot distribution
plot_df1_dist_lasttypclifid <- (
  ggplot(data=df1_dist_lasttypclifid
         , aes(x= LAST_TYP_CLI_FID, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df1_dist_lasttypclifid
#### FINAL REVIEW df_1_clean ####

str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)

#si sono svolti i seguenti passaggi:
#si è verificata la presenza di eventuali duplicati all'interno del dataset
#trasformazione in date time e fattorizzazione 
#controllo della distribuzione delle carte fedeltà per cliente, si nota come alcuni clienti abbiano più carte fedeltà
#si è effettuato un reshape del dataset per ricombinare alcune informazioni
#plot contenenti la distribuzione delle variabili di interesse

