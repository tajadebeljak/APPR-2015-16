# 4. faza: Analiza podatkov


potovanja2 <- potovanja %>% filter (Povprecno.stevilo.nocitev.in.izdatkov=="Povprečni izdatki na turista na prenočitev (EUR)")
potovanja2$Četrtletje <- potovanja2$Četrtletje %>%
  strapplyc("([0-9]+)Q([0-9])") %>% lapply(as.numeric) %>%
  sapply(. %>% {.[[1]] + (.[[2]]-1)/4})
graf_potovanj <- ggplot(potovanja2, aes(y=Meritve, x= Četrtletje)) + geom_point()
graf_potovanj1 <- graf_potovanj + geom_smooth(method = "lm", formula =y ~ x)

potovanja2_slovenija <- potovanja2 %>% filter (Destinacija=="Slovenija")
graf_potovanj_slo <- ggplot(potovanja2_slovenija, aes(y=Meritve, x= Četrtletje)) + geom_point()
graf_potovanj2 <- graf_potovanj_slo + geom_smooth(method = "lm", formula =y ~ x)

potovanja2_tujina <- potovanja2 %>% filter (Destinacija=="Tujina")
graf_potovanj_tuj <- ggplot(potovanja2_tujina, aes(y=Meritve, x= Četrtletje)) + geom_point()
graf_potovanj3 <- graf_potovanj_tuj + geom_smooth(method = "lm", formula =y ~ x)


št_prenočitev_slo <- potovanja %>% filter (Povprecno.stevilo.nocitev.in.izdatkov=="Povprečno število prenočitev", Destinacija=="Slovenija")
graf_prenočitve_slo <- ggplot(št_prenočitev_slo, aes(y=Meritve, x= Četrtletje)) + geom_point()
graf_prenočitve1 <- graf_prenočitve_slo + geom_smooth(method = "lm", formula =y ~ x)
#barve <- rainbow(length(levels(obcine[[7]])))
#names(barve) <- levels(obcine[[7]])
