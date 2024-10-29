
library(readxl)

#Wczytujemy dane z excela, które zostały pobrane z GUS

dane <- read_excel("C:/Users/wiola/OneDrive/Pulpit/R/Analiza_liniowa.xlsx")


#Tworzymy wykres zależności liczby osób bezrobotnych a liczby osób w wieku produkcyjnym

plot(Liczba_osób_bezrobotnych ~ Liczba_osób_w_wieku_produkcyjnym, data = dane)


#Tworzenie modelu regresji liniowej. Ustalenie relacji liniowej między liczbą osób bezrobotnych a liczbą osób w wieku produkcyjnym.

model_bezrobotni_produkcyjni <- lm(Liczba_osób_bezrobotnych ~ Liczba_osób_w_wieku_produkcyjnym, data = dane)

#Dodanie lini regresjii do wykresu

abline(model_bezrobotni_produkcyjni, col = "purple")

#Podsumowane modelu

summary(model_bezrobotni_produkcyjni)

        
#Tworzenie wykresu zależności między liczbą osób bezrobotnych, a ilością wolnych miejsc pracy i dodanie lini regresji

plot(Liczba_osób_bezrobotnych ~ Ilość_wolnych_miejsc_pracy, data = dane)


#Tworzenie modelu regresji liniowej. Ustalenie relacji liniowej między liczbą osób bezrobotnych a ilością wolnych miejsc pracy

model_bezrobtni_miejsca_pracy <- lm(Liczba_osób_bezrobotnych ~ Ilość_wolnych_miejsc_pracy, data = dane)
summary(model_bezrobtni_miejsca_pracy)

#Dodanie lini regresjii do wykresu

abline(model_bezrobtni_miejsca_pracy, col = "pink")


#Tworzymy model regresjii liniowej z jedną zmienną objaśnianą oraz dwoma objaśniającymi

model_finalny <- lm(Liczba_osób_bezrobotnych ~ Liczba_osób_w_wieku_produkcyjnym
 + Ilość_wolnych_miejsc_pracy, data = dane)

#Podsumowanie modelu
summary(model_finalny)
