# Mateusz Cypcer Wariant 3


# a)
a = 5/4^3
b = 2 * a
min(a,b)


# b)
help(min)


# c)
a = c(50:75)
a
mean(a^2)


# d)
apropos("min", mode = "function")


# e)
getwd()
setwd("D:/R")
a = "aparat z wymienną optyką"
save(a, file = "data")
remove(a)
a
load("data")
a


# f)
#install.packages("gridExtra")
library(gridExtra)
help(package="gridExtra")
seatbelts_subset <- head(Seatbelts, 10)
table_grob <- tableGrob(seatbelts_subset)
grid.arrange(table_grob)


# g)
c = seq(1000,800,-5)


#h)
a = 29:5
b = 21:33
d = c(b, a)
d


# i)
nazwa = c("Canon EOS 250D", "Pentax KF", "Canon EOS 850D", "Pentax K-70", 
          "Nikon D3500", "Canon EOS 90D", "Canon EOS 6D Mark II", 
          "Canon PowerShot SX70", "Sony Alpha a7 III", " Sony A6100 ")
rozdzielczosc = c("24,1 Mpix", "24 Mpix", "24,1 Mpix", "24 Mpix", "24,2 Mpix",
                  "32,5 Mpix", "26,2 Mpix", "20,3 Mpix", "24,2 Mpix", "24,2 Mpix")
zakres_czulosci = c("100 - 25600", "100-102400", "100 - 25600", "100 - 102400 ",
                    "100 - 25600 ", "100 - 25600 ", "100 - 40000 ", 
                    "100 - 25600 ", "50 - 204800", "100 - 32000")
cena = c(3299.00, 3928.00, 3999.00, 4099.00, 2590.00, 4999.00, 5999.00, 2679.00,
         8199.00, 3999.00)
liczba_opinii = c(1, 0, 0, 1, 22, 10, 63, 23, 12, 7)

aparaty = data.frame(nazwa, rozdzielczosc, zakres_czulosci, cena, liczba_opinii)
aparaty

srednia_cena = mean(aparaty$cena)
srednia_cena


# j)
nowy_aparat = data.frame(nazwa = "Panasonic Lumix S5", rozdzielczosc = "24,2 Mpix",
                          zakres_czulosci = "100 - 204800", cena = 8190.00, liczba_opinii = 1)

aparaty = rbind(aparaty, nowy_aparat)
aparaty

srednia_cena_po_dodaniu = mean(aparaty$cena)
srednia_cena_po_dodaniu


# k)
ocena = c(5, 0, 0, 5, 4.5, 4, 4.5, 4.5, 4, 5, 5)
aparaty = cbind(aparaty, ocena)
aparaty
srednie_ceny_na_ocene <- tapply(aparaty$cena, aparaty$ocena, mean, na.rm = TRUE)
srednie_ceny_na_ocene


# l)
nowe_aparaty = data.frame(
  nazwa = c("Nikon Z30", "Sony Alpha 6400", "Canon EOS R10", " Panasonic Lumix DMC-G7"),
  rozdzielczosc = c("21,51 Mpix ", "24,2 Mpix ", "24,2 Mpix", "16 Mpix "),
  zakres_czulosci = c("100 -51200", "100 - 102400", "100-32000", "200 - 25600"),
  cena = c(3849.00, 3949.00, 4699.00, 2090.00),
  liczba_opinii = c(0, 9, 3, 17),
  ocena = c(0, 4.5, 5, 5)
)

aparaty = rbind(aparaty, nowe_aparaty)
aparaty

count = table(aparaty$ocena)

barplot(count,
        main = "Liczebność reprezentantów dla każdej oceny",
        ylim = c(0, max(count) + 5),
        xlab = "Ocena klientów",
        ylab = "Liczebność",
        col = "skyblue")


# m)
procentowy_udzial = prop.table(table(aparaty$ocena)) * 100
pie(procentowy_udzial, 
    main = "Procentowy udział każdej oceny",
    col = rainbow(length(procentowy_udzial)),
    labels = paste(names(procentowy_udzial), ": ", round(procentowy_udzial, 1), "%"))

#install.packages("plotrix")
library(plotrix)

fan.plot(procentowy_udzial,
         main = "Procentowy udział każdej oceny",
         col = rainbow(length(procentowy_udzial)),
         labels = paste(names(procentowy_udzial), ": ", round(procentowy_udzial, 1), "%"))


# n)
aparaty$status_opinii = cut(aparaty$liczba_opinii,
                             breaks = c(-Inf, 0, 10, 30, Inf),
                             labels = c("nie ma", "mniej niż 10 opinii",
                                        "10-30 opinii", "więcej niż 30 opinii"))

aparaty$status_opinii = as.factor(aparaty$status_opinii)
aparaty

procentowy_udzial_statusu_opinii <- prop.table(table(aparaty$status_opinii)) * 100

pie(procentowy_udzial_statusu_opinii,
    main = "Procentowy udział aparatów o konkretnym statusie opinii",
    col = rainbow(length(procentowy_udzial_statusu_opinii)),
    labels = paste(names(procentowy_udzial_statusu_opinii), ": ",
                   round(procentowy_udzial_statusu_opinii, 1), "%"))


# o)
zdania_aparatow <- paste(aparaty$nazwa, " ma ocenę klientów ", aparaty$ocena,
                         " bo ma liczbę opinii ", aparaty$liczba_opinii)

cat(zdania_aparatow, sep = "\n")


# p)
write.csv(aparaty, file = "aparaty.csv")
wczytane_aparaty <- read.csv("aparaty.csv")
wczytane_aparaty

