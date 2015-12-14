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
                    col.names = c("Destinacija", "Starost", "Povprečno število nočitev in izdatkov", "Četrtletje", "Meritve"),
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


starost <- c("15-24","25-44","45-64", "65+")

#izdatki po starostih za Slovenijo
vsote_slo <- c(filter(potovanja, Destinacija=="Slovenija",Starost=="15-24", 
                  Povprečno število nočitev in izdatkov=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(),
               filter(potovanja, Destinacija=="Slovenija",Starost=="25-44", 
                  Povprečno število nočitev in izdatkov=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(),
               filter(potovanja, Destinacija=="Slovenija",Starost=="45-64", 
                  Povprečno število nočitev in izdatkov=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(),
               filter(potovanja, Destinacija=="Slovenija",Starost=="65+", 
                  Povprečno število nočitev in izdatkov=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum())

#izdatki po starostih za tujino
vsote_tujina <- c(filter(potovanja, Destinacija=="Tujina",Starost=="15-24", 
                         Povprečno število nočitev in izdatkov=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(),
                  filter(potovanja, Destinacija=="Tujina",Starost=="25-44", 
                         Povprečno število nočitev in izdatkov=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(),
                  filter(potovanja, Destinacija=="Tujina",Starost=="45-64", 
                         Povprečno število nočitev in izdatkov=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(),
                  filter(potovanja, Destinacija=="Tujina",Starost=="65+", 
                         Povprečno število nočitev in izdatkov=="Povprečni izdatki na turista na prenočitev (EUR)")$Meritve%>% sum(),
odstotki <- round(vsote_slo/vsote_tujina)

tabela_starosti <- data.frame(starost, vsote_slo, vsote_tujina, odstotki) 

ggplot(tabela_starosti, aes(x=starost, y=odstotki)) + geom_point()
                  
#HTML TABELA


html <- file("podatki/potovanja-slovencev.html") %>% read_html()

tabela2 <- html %>% html_nodes(xpath="//table[1]") %>% .[[1]] %>% html_table(fill = TRUE)
Encoding(tabela2[[1]]) <- "UTF-8"
tabela2 <- t(apply(tabela2, 1, function(x) c(rep(NA, sum(is.na(x))), x[!is.na(x)])))
tabela2 <- tabela2[-nrow(tabela2),]


tabela2 <- uredi(tabela2, 1, 1, 384, brisi=FALSE)
tabela2 <- uredi(tabela2, 1, 2, 95, brisi=FALSE)
tabela2 <- uredi(tabela2, 1, 3, 47, brisi=FALSE)
tabela2 <- uredi(tabela2, 1, 4, 11, brisi=FALSE)






# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.