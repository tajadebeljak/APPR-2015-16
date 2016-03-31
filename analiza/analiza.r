# 4. faza: Analiza podatkov


potovanja2 <- potovanja %>% filter (Povprecno.stevilo.nocitev.in.izdatkov=="Povprečni izdatki na turista na prenočitev (EUR)")
#potovanja2$Četrtletje <- potovanja2$Četrtletje %>%
#  strapplyc("([0-9]+)Q([0-9])") %>% lapply(as.numeric) %>%
#  sapply(. %>% {.[[1]] + (.[[2]]-1)/4})

potovanja2 <- potovanja2 %>%
  mutate(datum = Četrtletje %>% strapplyc("([0-9]+)") %>%
           sapply(. %>% {paste(.[[1]], 3*as.numeric(.[[2]])-2, 1,
                               sep = "-")}) %>% as.Date())

graf_potovanj <- ggplot(potovanja2, aes(y=Meritve, x= datum)) + geom_point()
graf_potovanj1 <- graf_potovanj + geom_smooth(method = "lm", formula =y ~ x)

potovanja2_slovenija <- potovanja2 %>% filter (Destinacija=="Slovenija")
graf_potovanj_slo <- ggplot(potovanja2_slovenija, aes(y=Meritve, x= datum)) + geom_point()
graf_potovanj2 <- graf_potovanj_slo + geom_smooth(method = "lm", formula =y ~ x) + labs(list(title="Povprečni izdatki (Slovenija)", x="Datum", y="Meritve"))

potovanja2_tujina <- potovanja2 %>% filter (Destinacija=="Tujina")
graf_potovanj_tuj <- ggplot(potovanja2_tujina, aes(y=Meritve, x= datum)) + geom_point()
graf_potovanj3 <- graf_potovanj_tuj + geom_smooth(method = "lm", formula =y ~ x)


št_prenočitev_slo <- potovanja %>% filter (Povprecno.stevilo.nocitev.in.izdatkov=="Povprečno število prenočitev", Destinacija=="Slovenija")
št_prenočitev_slo <- št_prenočitev_slo %>%
  mutate(datum = Četrtletje %>% strapplyc("([0-9]+)") %>%
           sapply(. %>% {paste(.[[1]], 3*as.numeric(.[[2]])-2, 1,
                               sep = "-")}) %>% as.Date())


graf_prenočitve_slo <- ggplot(št_prenočitev_slo, aes(y=Meritve, x= datum)) + geom_point()
graf_prenočitve1 <- graf_prenočitve_slo + geom_smooth(method = "lm", formula =y ~ x) + labs(list(title="Število prenočitev (Slovenija)", x="Datum", y="Meritve"))

št_prenočitev_tuj <- potovanja %>% filter (Povprecno.stevilo.nocitev.in.izdatkov=="Povprečno število prenočitev", Destinacija=="Tujina")
št_prenočitev_tuj <- št_prenočitev_tuj %>%
  mutate(datum = Četrtletje %>% strapplyc("([0-9]+)") %>%
           sapply(. %>% {paste(.[[1]], 3*as.numeric(.[[2]])-2, 1,
                               sep = "-")}) %>% as.Date())


graf_prenočitve_tuj <- ggplot(št_prenočitev_tuj, aes(y=Meritve, x= datum)) + geom_point()
graf_prenočitve2 <- graf_prenočitve_slo + geom_smooth(method = "lm", formula =y ~ x)
