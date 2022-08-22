#### FIRST LOOK of df_5 ####
# df_5_camp_cat contiene informazioni riguardanti le campagne marketing.


str(df_5_camp_cat)
# df_5_camp_cat ha 3 attributi: ID_CAMP (id campagna marketing inviata per email ai clientiche hanno accettato), 
# TYP_CAMP(tipo di campagna marketing), CHANNEL_CAMP(tipo di canale di comunicazione utilizzato). 

summary(df_5_camp_cat)
# breve descrizione della composizione delle variabili presenti

#### START CLEANING df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat

#### CLEANING LOW VARIANCE in df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)
# data la poca varianza del campo si è deciso di eliminare CHANNEL_CAMP

#### FINAL REVIEW df_5_clean ####

str(df_5_camp_cat_clean)
summary(df_5_camp_cat_clean)
# df_5_camp_cat_clean contiene gli attributi ID_CAMP e TYP_CAMP 
