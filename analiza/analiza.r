# 4. faza: Analiza podatkov


potovanja2 <- potovanja %>% filter (Povprecno.stevilo.nocitev.in.izdatkov=="Povprečni izdatki na turista na prenočitev (EUR)")
potovanja2$Četrtletje <- potovanja2$Četrtletje %>%
  strapplyc("([0-9]+)Q([0-9])") %>% lapply(as.numeric) %>%
  sapply(. %>% {.[[1]] + (.[[2]]-1)/4})

graf_potovanj <- ggplot(potovanja2, aes(y=Meritve, x= Četrtletje)) + geom_point()

graf_potovanj1 <- graf_potovanj + geom_smooth(method = "lm", formula =y ~ x)

#barve <- rainbow(length(levels(obcine[[7]])))
#names(barve) <- levels(obcine[[7]])
