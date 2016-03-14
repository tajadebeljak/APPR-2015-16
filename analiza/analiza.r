# 4. faza: Analiza podatkov


potovanja2 <- potovanja %>% filter (Povprecno.stevilo.nocitev.in.izdatkov=="Povprečni izdatki na turista na prenočitev (EUR)")
potovanja2$Četrtletje <- gsub("Q1", ".00", potovanja2$Četrtletje) %>% as.numeric()
potovanja2$Četrtletje <- gsub("Q2", ".25", potovanja2$Četrtletje) %>% as.numeric()
potovanja2$Četrtletje <- gsub("Q3", ".50", potovanja2$Četrtletje) %>% as.numeric()
potovanja2$Četrtletje <- gsub("Q4", ".75", potovanja2$Četrtletje) %>% as.numeric()

graf_potovanj <- ggplot(potovanja2, aes(y=Meritve, x= Četrtletje)) + geom_point()

graf_potovanj1 <- graf_potovanj + geom_smooth(method = "lm", formula =y ~ x)

#barve <- rainbow(length(levels(obcine[[7]])))
#names(barve) <- levels(obcine[[7]])
