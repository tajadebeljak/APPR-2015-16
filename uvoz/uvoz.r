# 2. faza: Uvoz podatkov

library(dplyr)
library(gsubfn)
library(ggplot2)
library(rvest)


#tabela csv

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.potovanja <- function() {
  return(read.csv2("podatki/potovanja-slovencev.csv",
                    sep = ";", header = FALSE,
                    as.is = TRUE, na.strings = 'N',
                    col.names = c("Destinacija", "Starost", "Povprecno.stevilo.nocitev.in.izdatkov", "Četrtletje", "Meritve"),
                    fileEncoding = "UTF-8"))
}

# Zapišimo podatke v razpredelnico potovanja.
potovanja <- uvozi.potovanja()

uredi <- function(tabela, x, y, z, max = nrow(tabela), brisi = TRUE) { 
     s <- seq(x, max, z+1) 
     tabela[t(matrix(x:max, ncol=length(s))), y] <- tabela[s, y] 
     if (brisi) {
      tabela <- tabela[-s,]
     }
     return(tabela) 
} 


potovanja <- uredi(potovanja, 1, 1, 332) 
potovanja <- uredi(potovanja, 1, 2, 82) 
potovanja <- uredi(potovanja, 1, 3, 40) 

potovanja$Meritve <- gsub("M", "", potovanja$Meritve) %>% as.numeric()





##povprečni izdatki za potovanja po starostnih skupinah
starost <- c("15-24","25-44","45-64", "65+")

#izdatki po starostih za Slovenijo
vsote_slo <- c(filter(potovanja, Destinacija=="Slovenija",Starost=="15-24", 
                  `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(na.rm=TRUE)/40,
               filter(potovanja, Destinacija=="Slovenija",Starost=="25-44", 
                  `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(na.rm=TRUE)/40,
               filter(potovanja, Destinacija=="Slovenija",Starost=="45-64", 
                  `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(na.rm=TRUE)/40,
               filter(potovanja, Destinacija=="Slovenija",Starost=="65 +", 
                  `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(na.rm=TRUE)/40)

#izdatki po starostih za tujino
vsote_tujina <- c(filter(potovanja, Destinacija=="Tujina",Starost=="15-24", 
                         `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(na.rm=TRUE)/39,
                  filter(potovanja, Destinacija=="Tujina",Starost=="25-44", 
                         `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(na.rm=TRUE)/40,
                  filter(potovanja, Destinacija=="Tujina",Starost=="45-64", 
                         `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(na.rm=TRUE)/40,
                  filter(potovanja, Destinacija=="Tujina",Starost=="65 +", 
                         `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(na.rm=TRUE)/37)

delez_potovanj_slo<-round(c(round(vsote_slo,2)/(round(vsote_slo,2)+round(vsote_tujina,2))),2)

tabela_starosti <- data.frame(starost, round(vsote_slo,2), round(vsote_tujina,2), delez_potovanj_slo) 

colnames(tabela_starosti)<-c("Starost", "Povprecni_izdatki_slo", "Povprecni_izdatki_tujina", "Delez_izdatkov_slo")

ggplot(data= tabela_starosti, aes(x=Starost, y=Delez_izdatkov_slo)) + geom_point()




##povprečno število prenočitev po staorstnih skupinah
prenocitve_slo <- c(filter(potovanja, Destinacija=="Slovenija",Starost=="15-24", 
                      `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečno število prenočitev")$Meritve%>% sum(na.rm=TRUE)/40,
               filter(potovanja, Destinacija=="Slovenija",Starost=="25-44", 
                      `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečno število prenočitev")$Meritve%>% sum(na.rm=TRUE)/40,
               filter(potovanja, Destinacija=="Slovenija",Starost=="45-64", 
                      `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečno število prenočitev")$Meritve%>% sum(na.rm=TRUE)/40,
               filter(potovanja, Destinacija=="Slovenija",Starost=="65 +", 
                      `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečno število prenočitev")$Meritve%>% sum(na.rm=TRUE)/40)

prenocitve_tujina <- c(filter(potovanja, Destinacija=="Tujina",Starost=="15-24", 
                         `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečno število prenočitev")$Meritve%>% sum(na.rm=TRUE)/39,
                  filter(potovanja, Destinacija=="Tujina",Starost=="25-44", 
                         `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečno število prenočitev")$Meritve%>% sum(na.rm=TRUE)/40,
                  filter(potovanja, Destinacija=="Tujina",Starost=="45-64", 
                         `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečno število prenočitev")$Meritve%>% sum(na.rm=TRUE)/40,
                  filter(potovanja, Destinacija=="Tujina",Starost=="65 +", 
                         `Povprecno.stevilo.nocitev.in.izdatkov`=="Povprečno število prenočitev")$Meritve%>% sum(na.rm=TRUE)/37)


tabela_prenocitve <- data.frame(starost, round(prenocitve_slo, 2), round(prenocitve_tujina, 2))
                  




#HTML TABELA


html <- file("podatki/potovanja-slovencev.html") %>% read_html()

tabela2 <- html %>% html_nodes(xpath="//table[1]") %>% .[[1]] %>% html_table(fill = TRUE)
Encoding(tabela2[[1]]) <- "UTF-8"
tabela2 <- t(apply(tabela2, 1, function(x) c(rep(NA, sum(is.na(x))), x[!is.na(x)]))) %>% data.frame()
tabela2 <- tabela2[-nrow(tabela2),]


tabela2 <- uredi(tabela2, 1, 1, 384, brisi=FALSE)
tabela2 <- uredi(tabela2, 1, 2, 95, brisi=FALSE)
tabela2 <- uredi(tabela2, 1, 3, 47, brisi=FALSE)
tabela2 <- uredi(tabela2, 1, 4, 11, brisi=FALSE)

colnames(tabela2)<-c("Povprecni_izdatki", "Vrsta_potovanja", "Destinacija", "Izdatki", "Cetrtletlje", "Meritve")
tabela2$Meritve <- gsub("[MN]", "", as.character(tabela2$Meritve)) %>% as.numeric()



#izdatki za zasebna in poslovna potovanja
vrsta_potovanja <-c("Poslovno", "Zasebno")

izdatki_zasebno<-c(filter(tabela2, Povprecni_izdatki="Povprečni izdatki na turista na prenočitev (EUR)", Vrsta_potovanja="Zasebna potovanja",
                          Destinacija="Slovenija", Izdatki="Izdatki za nastanitev")$Meritve%>% sum(na.rm=TRUE),
                   filter(tabela2, Povprecni_izdatki="Povprečni izdatki na turista na prenočitev (EUR)", Vrsta_potovanja="Zasebna potovanja",
                          Destinacija="Slovenija", Izdatki="Izdatki za prevoz")$Meritve%>% sum(na.rm=TRUE),
                   filter(tabela2, Povprecni_izdatki="Povprečni izdatki na turista na prenočitev (EUR)", Vrsta_potovanja="Zasebna potovanja",
                          Destinacija="Tujina", Izdatki="Izdatki za nastanitev")$Meritve%>% sum(na.rm=TRUE),
                   filter(tabela2, Povprecni_izdatki="Povprečni izdatki na turista na prenočitev (EUR)", Vrsta_potovanja="Zasebna potovanja",
                          Destinacija="Tujina", Izdatki="Izdatki za prevoz")$Meritve%>% sum(na.rm=TRUE))



# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
