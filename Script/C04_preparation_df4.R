#### FIRST LOOK of df_4 ####
# df_4_cli_privacy ha al suo interno dati sulle condizioni di privacy per ciascun cliente.

str(df_4_cli_privacy)
# df_4_cli_privacy contiene 4 campi: ID_CLI (identifica il cliente), FLAG_PRIVACY_1 (variabile binaria che riguarda
#la possibilità di essere contattato dall'azienda), FLAG_PRIVACY_2 (variabile binaria che riguarda
#la possibilità di essere profilato dall'azienda), FLAG_DIRECT_MKT (variabile binaria che riguarda
#la possibilità di essere soggetto a campagne marketing dell'azienda).

summary(df_4_cli_privacy)
# breve descrizione della composizione delle variabili presenti

#### START CLEANING df_4 ####

df_4_cli_privacy_clean <- df_4_cli_privacy

#### CLEANING DUPLICATE VALUES in df_4 ####

## check for duplicates
df_4_cli_privacy_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_4 ####

## formatting boolean as factor ##
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

#### CONSISTENCY CHECK ID_CLI in df_1/df_4 ####

cons_idcli_df1_df4 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_4_cli_privacy_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_4 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_4) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df4

#!!! NOTE: all ID_CLI in df_1 are also in df_4 and vice-versa !!!#

#### EXPLORE COLUMNS of df_4 ####

#### ???? TO DO df_4 ???? ####
# EXPLORE the df_4_cli_privacy_clean relevant variables

# 1. variabile FLAG_PRIVACY_1
df4_dist_flagpriv1 <- df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df4_dist_flagpriv1

## plot distribution
plot_df4_dist_flagpriv1 <- (
  ggplot(data=df4_dist_flagpriv1
         , aes(x=FLAG_PRIVACY_1, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df4_dist_flagpriv1
# La società ha ottenuto la possibilità di contattare 242251 
# clienti, 127221 clienti si sono rifiutati di essere contattati. 

# 2. variabile FLAG_PRIVACY_2
df4_dist_flagpriv2 <- df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_2) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df4_dist_flagpriv2


## plot distribution
plot_df4_dist_flagpriv2 <- (
  ggplot(data=df4_dist_flagpriv2
         , aes(x=FLAG_PRIVACY_2, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df4_dist_flagpriv2
# la società può profilare il 93,6% del totale clienti, 

# 3. variabile FLAG_DIRECT_MKT
df4_dist_flagdirectmkt <- df_4_cli_privacy_clean %>%
  group_by(FLAG_DIRECT_MKT) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df4_dist_flagdirectmkt 


## plot distribution
plot_df4_dist_flagdirectmkt  <- (
  ggplot(data=df4_dist_flagdirectmkt 
         , aes(x=FLAG_DIRECT_MKT, y=TOT_CLIs)
  ) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df4_dist_flagdirectmkt 
# I clienti che accettano di essere soggetti a campagne marketing sono 247790 ,
# mentre quelli che non accettano sono 121682 (il 33% circa del totale clienti).

#### FINAL REVIEW df_4_clean ####

str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)
#Per quanto riguarda le analisi delle singole variabili si veda il commento alle singole variabili.
