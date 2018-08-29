library(tidyverse)
library(readr)

# data from cars, updated at 31/10/2017: http://dati.mit.gov.it/catalog/dataset/parco-circolante-dei-veicoli

# data from year 2017 tax returns: http://www1.finanze.gov.it/finanze3/analisi_stat/index.php?tree=2017

files <- list.files(path="D:/blog/poverta-autodilusso/dati", pattern="parco*.*")
files
setwd("D:/blog/poverta-autodilusso/dati")
tbl = lapply(files, read_csv) %>% bind_rows()

# solo auto di lusso, esclusi noleggi
# auto, kw>200, cil >3k, uso= no noleggi, destinazione = trasporto persone, qualsiasi anno


tbl_luxury<-tbl%>%
  filter(tipo_veicolo=="A" & kw>=200 & cilindrata>=3000 & destinazione=="AUTOVETTURA PER TRASPORTO DI PERSONE" & uso =="PROPRIO") %>%
  select(tipo_veicolo, comune_residenza, provincia_residenza, regione_residenza, marca, cilindrata, kw, destinazione, uso, data_immatricolazione)

tbl_luxury_group<-tbl_luxury %>% 
  group_by(comune_residenza, provincia_residenza, regione_residenza) %>% 
  summarise(auto_lusso=n())



# leggi dichiarazioni
dichiarazioni_2016 <- read_delim("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2016.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# poveri= reddito <=0 e <=10k
# rinomino le colonne che mi interessano
dichiarazioni_2016<-rename(dichiarazioni_2016, "numero_contribuenti"=`Numero contribuenti`)

dichiarazioni_2016<-rename(dichiarazioni_2016, "reddito_minore_uguale_zero_frequenza"=`Reddito complessivo minore o uguale a zero euro - Frequenza`)

dichiarazioni_2016<-rename(dichiarazioni_2016, "reddito_0_10000_frequenza"=`Reddito complessivo da 0 a 10000 euro - Frequenza`)

dichiarazioni_2016<-rename(dichiarazioni_2016, "comune"=`Denominazione Comune`)
dichiarazioni_2016<-rename(dichiarazioni_2016, "sigla_provincia"=`Sigla Provincia`)

dichiarazioni_2016<-dichiarazioni_2016 %>% 
  select(comune, sigla_provincia, numero_contribuenti, reddito_minore_uguale_zero_frequenza,reddito_0_10000_frequenza)

# gestione NA (solo reddito<0)
dichiarazioni_2016 <- mutate(dichiarazioni_2016, reddito_minore_uguale_zero_frequenza = ifelse(is.na(reddito_minore_uguale_zero_frequenza), 0, reddito_minore_uguale_zero_frequenza))

# ora somma reddito <0 e <10k
dichiarazioni_2016<-dichiarazioni_2016 %>% mutate(tot_redditi_bassi= reddito_minore_uguale_zero_frequenza+ reddito_0_10000_frequenza, pct_redditi_bassi = tot_redditi_bassi/numero_contribuenti*100)

# # uso percentile per categorie redditi bassi: bassa/media/alta
qp<-quantile(dichiarazioni_2016$pct_redditi_bassi,c(0,1/3, 2/3,1), na.rm = TRUE) 
dichiarazioni_2016<-dichiarazioni_2016 %>% 
  mutate(cat_redditi_bassi=cut(pct_redditi_bassi, breaks=qp, labels=c("bassa","media", "alta")))



# comuni che hanno auto di lusso ma non hanno dichiarazioni
test_mancanti<-tbl_luxury_group %>% anti_join(dichiarazioni_2016, by = c("comune_residenza"="comune"))

# calcolo metrica: auto lusso pro capite
test<-tbl_luxury_group %>% inner_join(dichiarazioni_2016, by = c("comune_residenza"="comune")) %>%   mutate(auto_lusso_procapite=auto_lusso/numero_contribuenti)

# percentile auto
qa<-quantile(test$auto_lusso_procapite,c(0,1/3, 2/3,1), na.rm = TRUE) 

# uso percentile per categorie auto lusso:bassa/media/alta
test<-test %>% 
  mutate(cat_auto_lusso=cut(auto_lusso_procapite, breaks=qa, labels=c("bassa","media", "alta")))

test_alti<-test %>%
  filter(cat_redditi_bassi=="alta" & cat_auto_lusso=="alta") %>% 
  select(comune_residenza,provincia_residenza, regione_residenza,numero_contribuenti,tot_redditi_bassi,pct_redditi_bassi, auto_lusso,auto_lusso_procapite)


library(patchwork) 
 
# plotting delle due variabili
p1<-test %>% ggplot(aes(x=auto_lusso_procapite, y= pct_redditi_bassi))+
  geom_point()+
  theme_light()+
  labs(caption = "Auto di lusso e redditi bassi. Tutte le categorie ")+
  theme(plot.caption = element_text(hjust = 0.5))


# plotting delle due variabili solo cat "alta"
p2<-test_alti %>% ggplot(aes(x=auto_lusso_procapite, y= pct_redditi_bassi))+
  geom_point()+
  theme_light()+
  labs(caption = "Auto di lusso e redditi bassi. Frequenze elevate")+
  theme(plot.caption = element_text(hjust = 0.5))

p1+p2
png("fig3_2.png")
p1+p2
dev.off()
# scrivo il file per tableau
write.csv2(test, "test.csv")


# visualizzazione e geolocation
#library(ggmap)
#latlon<-geocode(rank_finale_alta$comune_residenza)
