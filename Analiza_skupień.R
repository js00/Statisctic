library(readxl)
library(factoextra)
library(cluster)
library(NbClust)

#Zaczytanie danych
DaneUnijne <- read_excel("C:/Users/Kuba/Desktop/infa w biz/Semestr 2/Statystyka/Projket statystyka/3.xlsx")

#Okrojenie danych do odczytu z excela od 2 do 6 kolumny
DaneUnijne1 <- DaneUnijne[2:6]

#Przeskalowanie danych
dane <- scale(DaneUnijne1)

#Zbadanie optymalnej liczby klastrów metodą łokcia
fviz_nbclust(dane, kmeans, method = "wss", nstart = 10, iter.max = 100) + labs(subtitle = "Metoda łokcia")
#4 klastry

#Metoda Davies-Bouldin Index 
fviz_db <- function(daneDavis) {
  #Liczba rozpatrywanych klastrów
  k <- c(2:10)
  nb <- NbClust(daneDavis, min.nc = 2, max.nc =  10, index = "db", method =  "kmeans")
  db <- as.vector(nb$All.index)
  plot(k, db, xlab = "Liczba klastrów",
       ylab = "Metoda Davies-Bouldin Index",
       col = "red", cex = 1,
       lty = 1, type = "o", lwd = 2, pch = 1)
}

#wywołanei funkcji metody Davies-Bouldin Index dla naszych danych 
fviz_db(dane)
#Wybieramy 6 klastrów. Najlepsze będzie 10, ale 6 jest bliskie, a potrzebujemy mniejszej liczby

#Przeprwadzamy dla 4 klastrów według metody Łokcia
km_wynik <- kmeans(dane, centers = 4, nstart = 100)
print(km_wynik)

#Struktura zmiennej km_wynik
str(km_wynik)

#Podział na klastry z etykietami wojewóztw
rownames(dane) <- paste(DaneUnijne$Województwo)
fviz_cluster(list(data = dane, cluster = km_wynik$cluster))

heat <- get_dist(dane)
fviz_dist(heat,
          show_labels = T,
          gradient = list(low = "white", mid = "yellow", high = "red"))




#Przeprwadzamy dla 6 klastrów według metody Davies-Bouldin Index 
km_wynik1 <- kmeans(dane, centers = 6, nstart = 100)
print(km_wynik1)

#Struktura zmiennej km_wynik1
str(km_wynik1)

#Podział na klastry z etykietami wojewóztw
rownames(dane) <- paste(DaneUnijne$Województwo)
fviz_cluster(list(data = dane, cluster = km_wynik1$cluster))

heat1 <- get_dist(dane)
fviz_dist(heat1,
          show_labels = T,
          gradient = list(low = "white", mid = "yellow", high = "red"))

