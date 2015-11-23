# Analiza podatkov s programom R, 2015/16

Avtor: Taja Debeljak

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2015/16.

## Tematika

V svojem projektu bom analizirala potovanja prebivalcev Slovenije. V prvem delu me bo zanimalo predvsem v katerem starostnem obdobju Slovenci največ potujemo(zasebno); v sklopu tega dela bom analizirala povprečno število prenočitev in povprečne izdatke na turista na prenočitev glede na različne starostne skupine. Primerjala bom podatke desetih let, od leta 2005 do leta 2014. (podatkovni vir: http://pxweb.stat.si/pxweb/Dialog/varval.asp?ma=2170110S&ti=&path=../Database/Ekonomsko/21_gostinstvo_turizem/06_potovanja/25_21701_znacilnosti_cetrt/&lang=2)
V drugem delu pa bom analizirala le za leto 2014, in sicer kakšni so na potovanjih izdatki za prevoz, nastanitve, hrano in druge aktivnosti v Sloveniji in v tujini (tako za zasebna, kot za poslovna potovanja). (podatkovni vir:http://pxweb.stat.si/pxweb/Dialog/varval.asp?ma=2170105S&ti=&path=../Database/Ekonomsko/21_gostinstvo_turizem/06_potovanja/25_21701_znacilnosti_cetrt/&lang=2)


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Spletni vmesnik

Spletni vmesnik se nahaja v datotekah v mapi `shiny/`. Poženemo ga tako, da v
RStudiu odpremo datoteko `server.R` ali `ui.R` ter kliknemo na gumb *Run App*.
Alternativno ga lahko poženemo tudi tako, da poženemo program `shiny.r`.

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `httr` - za pobiranje spletnih strani
* `XML` - za branje spletnih strani
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
