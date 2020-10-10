"1"
#opis: https://www.kaggle.com/abcsds/pokemon
download.file('http://staff.ii.pw.edu.pl/~rbembeni/dane/Pokemon.csv','Pokemon.csv')
pokemon <- read.csv("Pokemon.csv", header = TRUE, na.strings=c("","NA"))

library(dbscan)
library(fpc)
library(cluster)
library(factoextra)

set.seed(7876)


summary(pokemon)
head(pokemon)
str(pokemon)
View(pokemon)
col <-names(pokemon)
pokemon <- as.data.frame(pokemon)


for (c in col) {
  print(c)
  print(table(pokemon[c], useNA = "ifany"))
}
#usunięcie atrybutów id oraz telstowych
basePokemon = pokemon

#weentualnei parametry w okół których mogłóby nastąpic grupowanie to Typr 1 ale
#rozkład typów jest 
#dosyć rozdrobniony w type 2 mamy 386 wartośi nieznanych natomast 
#type 1 ma 6 licznych tpów a reszta jest dosyć rozdrobniona
#poniważ mamy 6 wartości ale pozostałych jest 342 to jest prawie 40% całej próby 
# mamy tez parametry o duzych wrtościach tj. HP,Attack,Defense,Sp..AtkSp.Def,Speed
# oraz 2 o małych Generation,Legendary 
"
 Min.   :  1.0   Abomasnow              :  1   Water  :112   Flying  : 97   Min.   :180.0  
 1st Qu.:184.8   AbomasnowMega Abomasnow:  1   Normal : 98   Ground  : 35   1st Qu.:330.0  
 Median :364.5   Abra                   :  1   Grass  : 70   Poison  : 34   Median :450.0  
 Mean   :362.8   Absol                  :  1   Bug    : 69   Psychic : 33   Mean   :435.1  
 3rd Qu.:539.2   AbsolMega Absol        :  1   Psychic: 57   Fighting: 26   3rd Qu.:515.0  
 Max.   :721.0   Accelgor               :  1   Fire   : 52   (Other) :189   Max.   :780.0  
                 (Other)                :794   (Other):342   NA's    :386         
"
#====Założenia=====
#Chciałbym osiągnąć dobre rezultaty grupowania dla zmiennej Legendary

summary(basePokemon)
pokemon$Type.2 <- NULL
pokemon$Name <- NULL
pokemon$X. <- NULL
pokemon$Type.1 <- NULL
pokemon$Total <- NULL
#Usuwamy dane wejsciowe tekstowe oraz ID 
typeof(pokemon)
pokemon$Legendary <- NULL




# wobu przypdakach otrzymaliśmy 2 grupy
fviz_nbclust(pokemon, pam, method = "silhouette")+theme_classic()
elbowmethod <-function (data) {
  wss <- 0
  # Od 1 do 15 grup
  for (i in 1:15) 
  {
    km.out <- kmeans(pokemon, centers = i, nstart=20)
    # Zapisz całkowita sume bledu kwadratowego do zmiennej wss
    wss[i] <- km.out$tot.withinss
  }
  
  print(km.out$centers)
  # Narysuj całkowita sume bledu kwadratowego wzgledem liczby grup
  plot(1:15, wss, type = "b",  xlab = "Liczba grup", ylab = "Suma bledu kwadratowego wewnatrz grup")
  
}

#for (i in 1:5) {
rindex <- sample(1:nrow(pokemon), nrow(pokemon) * 0.25) 
samplesPokemon <- pokemon[rindex,] 
 # }

elbowmethod(samplesPokemon)


#for (i in 1:5) {
rindex <- sample(1:nrow(pokemon), nrow(pokemon) * 0.25, replace = TRUE) 
samplesPokemon <- pokemon[rindex,] 
# }


elbowmethod(samplesPokemon)

#for (i in 1:5) {
rindex <- sample(1:nrow(pokemon), nrow(pokemon) * 0.25, replace = TRUE) 
samplesPokemon <- pokemon[rindex,] 
# }

elbowmethod(samplesPokemon)
# niezależnie od wylowango zbioru zawsze metoda łoksciowa wskazuje 2 grupy jako 
# najlepszy podział dla danych wskazywał by to na grupowanie narzucone parametrm legendary




km_alt<-eclust(pokemon, "kmeans", k=2, graph=FALSE)
print(km_alt)
print(km_alt$iter)
print(km_alt$centers)
print(km_alt$cluster)

km_alt
" (between_SS / total_SS =  31.9 %)"
#bardzo słabe dopasowaie
km_alt2<-eclust(pokemon, "kmeans", k=3, graph=FALSE )
km_alt2

km_alt4<-eclust(pokemon, "kmeans", k=4, graph=FALSE )
km_alt2





table(basePokemon$Legendary,km_alt$cluster)

fviz_silhouette(km_alt, palette="jco")


pokemonScale <- scale(pokemon)

kmv1 <- kmeans(pokemonScale,2, iter.max = 20, nstart=1) 
table(basePokemon$Legendary,kmv1$cluster)



fviz_nbclust(pokemon, pam, method = "silhouette")+theme_classic()
kmpure = kmeans(pokemonScale,40, iter.max = 20, nstart=20)
kmpure
" (between_SS / total_SS =  77.1 %)"
#Uzyskałem indeks skupiena na poziomie 77.1
summary(pokemonScale)
elbowmethod(pokemonScale)

km_alt3<-eclust(pokemonScale, "kmeans", k=3, graph=FALSE )
km_alt3
"(between_SS / total_SS =  34.6 %)"
km_alt4<-eclust(pokemonScale, "kmeans", k=25, graph=FALSE )
km_alt4
" (between_SS / total_SS =  71.0 %)
"


km_alt50<-eclust(pokemonScale, "kmeans", k=50, graph=FALSE )
km_alt50
" (between_SS / total_SS =  79.0 %)
"
#K menas daje lessze rezultaty dla duzej liczby zadanych grup



#po porzekalowaniu dalej sugrowane są dwie grupy


algs = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")

par(mfrow = c(2, 2))
#set.seed(1)
for(i in 1:4)
{
  pok.kmeansS = kmeans(pokemon,2,nstart = 1, algorithm = algs[i] )
  #drukuj wykres
  plot(iris2[1:2], col = pok.kmeansS$cluster, 
       main = paste(pok.kmeansS$tot.withinss, algs[i]), 
       xlab = "", ylab = "")
  points(pok.kmeansS$centers[,1:2], col = 1:3, pch = 8, cex=2)
}
#ustawienie siatki rysowania wykresów 1x1
par(mfrow = c(1, 1))

km_alt<-eclust(pokemonScale, "kmeans", k=2, graph=FALSE)

par(mfrow = c(2, 2))
#set.seed(1)
for(i in 1:4)
{
  pok.kmeansS = kmeans(pokemonScale,3,nstart = 1, algorithm = algs[i] )
  #drukuj wykres
  plot(iris2[1:2], col = pok.kmeansS$cluster, 
       main = paste(pok.kmeansS$tot.withinss, algs[i]), 
       xlab = "", ylab = "")
  points(pok.kmeansS$centers[,1:2], col = 1:3, pch = 8, cex=2)
}
#ustawienie siatki rysowania wykresów 1x1
par(mfrow = c(1, 1))

basePokemon$Legendary <- as.integer(basePokemon$Legendary)
str(basePokemon)
table(basePokemon$Legendary,km_alt$cluster)
nrow(basePokemon[basePokemon$Legendary == 1,])
nrow(basePokemon[basePokemon$Legendary == 2,])

#dane przeskalowane dają duzo lepszy rezultat podziału. Ale nie jest dobrze skalsfkowana 
#liczność grup gdyż mamy więcej 


pok_km = kmeans(pokemonScale,2, algorithm = c("MacQueen"))
table(basePokemon$Legendary,pok_km$cluster)
"   1   2
  1 377 358
  2   0  65"

km_alt<-eclust(pokemonScale, "kmeans", k=2, graph=FALSE)
table(basePokemon$Legendary,km_alt$cluster)
"     1   2
  1 359 376
  2  65   0"

pam_alt<-pam(pokemonScale, k=2, diss=FALSE, metric=c("euclidean"))
pam_alt
table(basePokemon$Legendary,pam_alt$cluster)
"  1   2
  1 295 440
  2   0  65"

pamMan_alt<-pam(pokemonScale, k=2, metric="manhattan")
table(basePokemon$Legendary,pamMan_alt$cluster)
"
     1   2
  1 321 414
  2   2  63
"
#Manhatan podzileił zbiór bardziej równomernie

#hierachiczne
# próbka danych w celu demonstracyjnym
idx <- sample(1:nrow(pokemonScale), 200)

?hclust
#wyliczenie macierzy odległości/niepodobieństwa
?dist
distM = dist(pokemonScale[idx], method='euclidean')
distT = as.matrix(distM)
dim(distT)
distT[1:5,1:5]

#wykonanie grupowania hierarchicznego dla różnych metod wyliczenia
hc_complete <- hclust(distM, method="complete")
hc_single <- hclust(distM, method="single")
hc <- hclust(distM, method="average")
hc_centroid <- hclust(distM, method="centroid")

#wydruk dendogramów dla różnych grupowań
?plot
par(mfrow=c(2,2))
plot(hc, hang = -1)
rect.hclust(hc, k=2, border=2:4)

plot(hc_complete, hang = -1)
rect.hclust(hc_complete, k=2, border=2:4)

plot(hc_single, hang = -1)
rect.hclust(hc_single, k=2, border=2:4)

plot(hc_centroid, hang = -1)
rect.hclust(hc_centroid, k=2, border=2:4)
par(mfrow=c(1,1))
#utworzenie grup
?cutree
groups <- cutree(hc, k=2)
groupsSimple <- cutree(hc_single, k=2)
groupsComplite <- cutree(hc_complete, k=2)
#iris2.hc.groups

#porównianie uzykanego grupowania z grupowaniem wzorcowym wyznaczonym przez atrybut klasy
table(basePokemon$Legendary[idx], groups)
"
  groups
      1   2
  1 183   2
  2  15   0
"
#Większośc zostala zakwalifikowana do grupy w tym prawie wszystkie legendarne
table(basePokemon$Legendary[idx], groupsSimple)
"
     1   2
  1 184   1
  2  15   0
"

table(basePokemon$Legendary[idx], groupsComplite)
" groupsComplite
      1   2
  1 168  17
  2  13   2"
#róznież niezbyt dobre grupowanie 17 nie legendarnch jako legendarne

#DBSCAN

dbscan::kNNdistplot(pokemonScale, k=2)
abline(h=0.5, lty="dashed")
#otrzymalismy 0.5 jako sugerowaną watość epsilona




dbscan_eps0.5 <- dbscan::dbscan(pokemonScale, eps=0.5, minPts=7)
table(basePokemon$Legendary, dbscan_eps0.5$cluster)

"dbscan_eps0.5 <- dbscan::dbscan(pokemonScale, eps=0.5, minPts=7)
> table(basePokemon$Legendary, dbscan_eps0.5$cluster)
   
      0
  1 735
  2  65"

#Dla powyzysz parametrów udało mi się uzyskać grupowae względem pramatru legendary

"> dbscan_eps0.5 <- dbscan::dbscan(pokemonScale, eps=0.5, minPts=5)
> table(basePokemon$Legendary, dbscan_eps0.5$cluster)
   
      0   1   2   3
  1 717   5   8   5
  2  65   0   0   0"


"dbscan_eps0.5 <- dbscan::dbscan(pokemonScale, eps=0.5, minPts=6)
> table(basePokemon$Legendary, dbscan_eps0.5$cluster)
   
      0   1
  1 727   8
  2  65   0"


"Dbscan duzo latwiej odnalazł grupowanie przyjęte w ramamch zalożenia natomiast pozostale algorytmy wydją się być
kepsze do szukania gryp ktorych obekty są bliżej siebie. Na próbce w grupowaniu hierachicznym udało się uzyskać zblizone propocje do całego zbioru "
