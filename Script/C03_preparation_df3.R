#### FIRST LOOK of df_3 ####
#all'interno di df_3_cli_address troviamo informazioni concesse dal cliente in fase di sottoscrizione
#riguardanti l'ubicazione del cliente stesso.

str(df_3_cli_address)
# df_3_cli_address è composto da 4 campi ID_ADDRESS (indirizzo di ogni cliente), CAP(indirizzo postale), 
# PVR (provincia) e REGION (regione).

summary(df_3_cli_address)
# breve descrizione della composizione delle variabili presenti

#### START CLEANING df_3 ####

df_3_cli_address_clean <- df_3_cli_address

#### CLEANING DUPLICATE VALUES in df_3 ####

## check for duplicates
df_3_cli_address_clean %>%
  summarize(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS)
            , TOT_ROWs = n())

#!!! NOTE:  there are duplicates !!!#
# si provvede ad eliminare i duplicati come segue

df_3_cli_address_clean <- df_3_cli_address_clean %>%
  distinct()

#### CLEANING DATA TYPES in df_3 ####

## format string as factors ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(CAP = as.character(CAP))

#### CLEANING MISSING VALUES in df_3 ####
View(df_3_cli_address)
df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP)
           , w_PRV = !is.na(PRV)
           , w_REGION = !is.na(REGION)) %>%
  summarize(TOT_ADDs = n_distinct(ID_ADDRESS))
#possiamo verificare come 337466 righe siano popolate e prive di valore nullo per
#tutti i campi

## let examine in details some of these missing cases
df_3_cli_address_clean %>% filter(!is.na(PRV) & is.na(REGION))

## MISSING VALUES rows are removed ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))

#### CONSISTENCY CHECK ID_ADDRESS in df_2/df_3 ####

cons_idaddress_df2_df3 <- df_2_cli_account_clean %>%
  select(ID_ADDRESS) %>%
  mutate(is_in_df_2 = 1) %>%
  distinct() %>%
  full_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS) %>%
              mutate(is_in_df_3 = 1) %>%
              distinct()
            , by = "ID_ADDRESS"
  ) %>%
  group_by(is_in_df_2, is_in_df_3) %>%
  summarize(NUM_ID_ADDRESSes = n_distinct(ID_ADDRESS)) %>%
  as.data.frame()

cons_idaddress_df2_df3

#!!! NOTE:  there are ID_ADDRESSes actually not mapped in df_3 !!!#
#!!!        this issue should be taken into account in joining these two tables !!!#

#### ???? TO DO df_3 ???? 
# EXPLORE the df_3_cli_address_clean relevant variables

# 1. variabile CAP 

nrow(df_3_cli_address_clean %>% distinct(CAP))
# 4784 different CAPs

df3_dist_capclean<- df_3_cli_address_clean %>%
  group_by(CAP) %>%
  summarize(TOT_CLIs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df3_dist_capclean

## plot distribution
plot_df3_dist_capclean <- (
  ggplot(data=df3_dist_capclean
         , aes(x=CAP, y=TOT_CLIs)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df3_dist_capclean

# 2. variabile PRV 

nrow(df_3_cli_address_clean %>% distinct(PRV)) 
# 110 different province

df3_dist_prvclean<- df_3_cli_address_clean %>%
  group_by(PRV) %>%
  summarize(TOT_CLIs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df3_dist_prvclean

## plot distribution
plot_df3_dist_prvclean <- (
  ggplot(data=df3_dist_prvclean
         , aes(x=TOT_CLIs, y=PRV)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df3_dist_prvclean

# 3. variabile REGION

nrow(df_3_cli_address_clean %>% distinct(REGION)) 
# 20 different region

df3_dist_regionclean<- df_3_cli_address_clean %>%
  group_by(REGION) %>%
  summarize(TOT_CLIs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df3_dist_regionclean

## plot distribution
plot_df3_dist_regionclean <- (
  ggplot(data=df3_dist_regionclean
         , aes(x=TOT_CLIs, y=REGION)) +
    geom_bar(stat="identity"
             , fill="steelblue") +
    theme_minimal()
)

plot_df3_dist_regionclean

#### FINAL REVIEW df_3_clean ####

str(df_3_cli_address_clean)

summary(df_3_cli_address_clean)
#dall'analisi effettuata emergono molti fattori significativi come ad esempio la presenza della maggior
#parte dei clienti all'interno della regione Lombardia.Con circa 40000 clienti che risiedono nella provincia di milano.

