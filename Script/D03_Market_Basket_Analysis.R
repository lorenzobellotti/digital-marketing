library(arules)
library(rattle)
library(graphlayouts)
library(plyr)
library(Rcpp)

##MARKET BASKET ANALYSIS
# E' un' analisi di affinita' che analizza le abitudini di acquisto dei clienti, trovando associazioni tra i diversi prodotti 
# acquistati (prodotti complementari).

##Breve analisi esplorativa della top-10 degli articoli più venduti
plot_df7_dist_idarticolo<- (
  ggplot(data = df7_dist_idarticolo,
         aes(x = ID_ARTICOLO, y = NUM_VENDITE)) +
    geom_bar(stat = "identity", color="blue", fill="steelblue")
) 
plot_df7_dist_idarticolo
# Il grafico mette in evidenza che la prima posizione e' occupata dall'articolo con ID 33700716, il quale ha registrato un totale di 
# 58010 vendite.


#Regole di associazione
retail_sorted <- df_7_tic_clean[order(df_7_tic_clean$ID_CLI),]

itemList <- ddply(df_7_tic_clean,c("ID_CLI","TIC_DATETIME"), 
                  function(df1)paste(df1$ID_ARTICOLO, 
                                     collapse = ","))
itemList$ID_CLI <- NULL
itemList$TIC_DATETIME <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

tr <- arules::read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

itemFrequencyPlot(tr, topN=10, type='absolute')

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))

#Ordinamento delle regole trovate in base al supporto, che fornisce la misura 
#in cui una regola e' applicata all'intero dataset, in percentuale. 
#E' anche utile ad eliminare regole che occorrono per puro caso, 
#e quindi non rilevanti nel mondo reale.
rules.sorted <- sort(rules, by='support', decreasing = TRUE) 
summary(rules)

#Identificazione delle prime 10 regole che sembrano essere quelle piu' rilevanti
topRules <- inspect(rules.sorted[1:10])

#Individuazione delle regole ridondanti
subset.matrix <- is.subset(rules,rules)
subset.matrix<-is.subset(rules,rules,sparse=FALSE)
subset.matrix[lower.tri(subset.matrix,diag=TRUE)]<- NA
redundant <- colSums(subset.matrix,na.rm = TRUE) >=1
which(redundant)

#Rimozione delle regole ridondanti
rules.pruned<-rules.sorted[!redundant]
rules.pruned<-sort(rules.pruned,by="lift")
inspect(rules.pruned)

#Identificazione delle prime 10 regole avendo escluso quelle ridondanti
topRules2 <- inspect(rules.pruned[1:10])

#Visualizzazioni 
plot(rules.pruned[1:10])
# Scatterplot dei valori di confidence, support e lift delle prime 10 regole di associazione.

plot(rules.pruned[1:10],method="grouped")
# In questo grafico e' possibile notare gli items con maggiore correlazione: ad esempio, l'articolo 36298381 e' un prodotto complementare
# dell'articolo 36298353 con un Supporto del 0.17% e un Lift di 350

plot(rules.pruned, method = "graph",
     control = list(
       edges = ggraph::geom_edge_link(
         end_cap = ggraph::circle(4, "mm"),
         start_cap = ggraph::circle(4, "mm"),
         color = "black",
         arrow = arrow(length = unit(2, "mm"), angle = 20, type = "closed"), alpha = .2
       ),
       nodes = ggraph::geom_node_point(aes_string(size = "support", color = "lift")), nodetext = ggraph::geom_node_label(aes_string(label = "label"), alpha = .8, repel = TRUE)
     ),
     limit = 10
)+
  scale_color_gradient(low = "yellow", high = "red") + scale_size(range = c(2, 10))
# Tale visualizzazione mostra la rappresentazione tramite grafo delle regole di associazione piu' significative. 

