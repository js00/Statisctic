library(readxl)
library(ggplot2)
library(psych)
library(clusterSim)

#Załadowanie danych
Składowe <- read_excel("C:/Users/Kuba/Desktop/infa w biz/Semestr 2/Statystyka/Projket statystyka/4.xlsx")

#Obcięcie danych
Składowe1 <- Składowe[2:6]

#Przeprowadzenie normalizacji
norma = data.Normalization(as.matrix(Składowe1), type="n1")
print(norma)

#Stworzenie analizy pca
pca <- prcomp(Składowe1, scale = T, center = T)
print(pca)

#Podsumowanie pca
summary(pca)

#Wykresy
plot(pca, type = "l", main = "Wykres osypiska")

#Wariancja dla każdej składowej
pca1 <- pca$sdev^2

#Udział procentowy
pca2 <- round(pca1 / sum(pca1) * 100, 1)

#Tworzenie wykresu słupkowego 
barplot(pca2, main = "wariancja wyjaśniająca")

#Tworzenie wykresu słupkowego 
barplot(pca1, main = "Wartości własne")

#Tworzenie wykresu mapowego
biplot(pca)

#Liczymy współczynnik KMO
wsp_kmo <- KMO(Składowe1)
print(wsp_kmo)
