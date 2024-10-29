library(dplyr)
library(broom)
library(readxl)

daneLogistyczna <- read_excel("C:/Users/wiola/OneDrive/Pulpit/R/Regresja_logistyczna.xlsx")

# Obliczenie mediany zmiennej ceny ubezpieczenia zdrowotnego 

mediana_Cena_ubezpieczenia_zdrowotnego <- median(daneLogistyczna$Cena_ubezpieczenia_zdrowotnego, na.rm = TRUE)

# Dyskretyzacja zmiennej ceny ubezpieczenia zdrowotnego

dyskretyzacja <- daneLogistyczna %>%
  mutate(Cena_ubezpieczenia_zdrowotnego_binary = ifelse(Cena_ubezpieczenia_zdrowotnego > mediana_Cena_ubezpieczenia_zdrowotnego, 1, 0))
View(dyskretyzacja)

# Przeprowadzenie regresji logistyczne

model <- glm(Cena_ubezpieczenia_zdrowotnego_binary ~ Wiek + Wskaźnik_masy_ciała, data = dyskretyzacja, family = binomial)

# Podsumowanie wyników modelu

summary(model)




