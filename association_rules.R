#ładowanie bibiliotek
library(arules) #reguły asocjacyjne
library(arulesViz) # wizualizacja reguł

#zbiór danych AdultUCI jest dostępny w bibliotece arules
#https://archive.ics.uci.edu/ml/datasets/Adult
data("AdultUCI")
dim(AdultUCI)

######################################
#przetwarzanie danych




#dyskretyzacja atrybutów ciągłych przy użyciu funkcji cut
#?ordered

AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(15,25,45,65,100)), labels = c("Young", "Middle-aged", "Senior", "Old"))

AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]],c(0,25,40,60,168)),
                                         labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))

AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]], c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),Inf)), labels = c("None", "Low", "High"))

AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],
                                           c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),Inf)),
                                       labels = c("None", "Low", "High"))


discFreq = discretize(AdultUCI$`education-num`, method= 'interval', breaks = 6)
summary(discFreq);
#usunięcie parametów nie istotnych
AdultUCI[["education-num"]] <-  NULL
AdultUCI[["fnlwgt"]] <- NULL


summary(AdultUCI)

adultTR <- as(AdultUCI, "transactions") 

printtRules <-function (data,sup, conf, len, maxsize, minsize) {
   if(missing(sup)){
    sup <-  0.1
   }
   if (missing(conf)) {
     conf <- 0.8
   }
   if (missing(len)) {
    len <- 1
   }
  if (missing(maxsize)) {
    maxsize <- 12
  }

  
  
  
   
  rules.all <- apriori(data, parameter = list(support=sup, confidence = conf, minlen = len, maxlen=maxsize))
  quality(rules.all) <- round(quality(rules.all), digits =  3)
  #?is.redundant
  rules.notRedundant  <- rules.all[!is.redundant(rules.all)]
  
  rulesLift1.2 <- subset(rules.notRedundant, subset =  lift > 1.2)
  
  rules.sorted <- sort(rulesLift1.2, by = "lift")
  print('apriori')
  summary(rules.sorted)
  inspect(head(rules.sorted,10))

  
  return (rules.sorted)

} 

#View(adultTR@itemInfo)


printtRules(AdultUCI,maxsize = 2)
#dla ustawień domyślnych ustawień funkcji i długości 2 znaleziono 10  długości max 2
#przykładowo reguła 5 mówi, że 13% osób kórzy mają duże zarobki, wszyscy brali ślub cywilny


# reguła 1 mówi, że z 10% młodych i pracujących w sektorze prywatnym zarabia mało, jednocześnie mamy 66% szansy, że jeśli zarabia mało to pracuje w prywatnej firmie i jest młody
#tutaj dostaliśmy podnad 9000 reguł
rules <- printtRules(AdultUCI, conf = 0.5)
plot(rules[size(rules)>4], method = "graph")
rulesInGivenConseq <- subset(rules, subset = rhs %in% "income=small")
inspect(head(rulesInGivenConseq,100))

#widać, że przy podniesionym stopniu wsparcia do 0.2 dostaliśmy tylko  niecałe 2000 reguł dla następnika "Mały przychód"
rules <- printtRules(AdultUCI, conf = 0.5, sup=0.2)
rulesInGivenConseq <- subset(rules, subset = rhs %in% "income=small")
inspect(head(rulesInGivenConseq,100))

#co do dużego dochodu 
rules <- printtRules(AdultUCI, conf = 0.1, sup=0.1)
rulesInGivenConseq <- subset(rules, subset = rhs %in% "income=large")
inspect(head(rulesInGivenConseq,100))


plot(rules[size(rules)>4], method = "graph")
#na podstawie samego grafu możemy stwierdzić, że w danych mamy sporą grupę białych mężczyzn będących w związku formalnym.
plot(rulesInGivenConseq[size(rulesInGivenConseq)>1], method = "graph")
plot(rules[size(rules)>4], method = "paracoord", control = list(reorder = TRUE))


#teraz chciałbym zbadać, które grupy dostarczyły przyrostu dochodów "caputal gain"(jeśli takie jest tego znaczenie :)) i w jakim stopniu


#z reguł znalezionych tutaj mogę wnioskować, że dla 10 % danych młodzi niezamężni ludzie nie zwiększają swoich zarobków a jeśli ktoś nie zwiększył swoich dochodów to też jest 
#raczej osobą młodą i nie żonatą
rules <- printtRules(AdultUCI, conf = 0.1, sup=0.1)
rulesInGivenConseq <- subset(rules, subset = rhs %in% c("capital-gain=Low", "capital-gain=High","capital-gain=None"))
summary(rulesInGivenConseq)
inspect(head(rulesInGivenConseq))


rulesInGivenConseq <- subset(rules, subset = rhs %in% c("capital-loss=Low", "capital-loss=High","capital-loss=None"))
summary(rulesInGivenConseq)
inspect(head(rulesInGivenConseq))

#Po obniżeniu liftu do 1 z 1.2 udało się jeszcze wyszukać regułę co do 10 procent mężczyzn w średnim wieku, że pracją na etacie, przy siedemdziesięciu procent wsparcia. 

rules <- printtRules(AdultUCI, conf = 0.7, sup=0.1)
rulesInGivenConseq <- subset(rules, subset = rhs %in% c("hours-per-week=Full-time"))
summary(rulesInGivenConseq)
inspect(head(rulesInGivenConseq))



#teraz poszukam reguł co do długości zatrudnienia #to do




#porównanie zbiorów częstych

? discretizeDF

printtRulesEclat <-function (data,sup, target, len, maxsize, minsize, conf) {

  if(missing(sup)){
    sup <-  0.1
  }
  if (missing(target)) {
    target <- "frequent itemsets"
  }
  if (missing(conf)) {
     conf <- 0.8
  }
  if (missing(len)) {
    len <- 1
  }
  if (missing(maxsize)) {
    maxsize <- 12
  }
  
  
  
  

  freq <- eclat(data, parameter = list(support=sup, target = target, minlen = len, maxlen=maxsize))

  #fequent 

  
  
  rules.all =  ruleInduction(freq, adultTR, confidence = conf,control=list(method ="ptree"))
  
  quality(rules.all) <- round(quality(rules.all), digits =  3)
  #?is.redundant
  rules.notRedundant  <- rules.all[!is.redundant(rules.all)]
  
  rulesLift1.2 <- subset(rules.notRedundant, subset =  lift > 1.2)
  
  rules.sorted <- sort(rulesLift1.2, by = "lift")
  summary(rules.sorted)
  print('eclect')
  inspect(head(rules.sorted,10))
  
  
  return (rules.sorted)
  
} 
# przy użyciu funkcji eclact i apriori pierwsze wyniki porkrywają się
rules <- printtRules(adultTR, conf = 0.8, sup=0.3)
rulesEc <- printtRulesEclat(adultTR, conf = 0.8, sup=0.3)


#Wykorzystanie hierarchii elementów

str(adultTR)
head(itemInfo(adultTR))
View(adultTR@itemInfo)


#Hierarchia jest zapisana w składowej iteminfo 
#variables - pierwszt poziom - zmienne
#levels - wartości  artybutów

#utworzenie transakcji zawierających elementy z pierwszego poziomu hierarchii
?aggregate
adultTRlevel1<- aggregate(adultTR, by = "levels")
summary(adultTRlevel1)
inspect(head(adultTR,100))

## porównanie orygianalnych transkacji z transakcjami po agregacji
for( i in 1:10)
{
  inspect(adultTR[i])
  inspect(adultTRlevel1[i])
  cat('\n')
}

#wykrywanie reguł z danych oryginalnych
rulesBasic <- apriori(adultTR, parameter=list(supp=0.03, conf=0.8))
summary(rulesBasic)
inspect(head(rulesBasic,15))

#agregacja reguł
rulesBasicAgg <- aggregate(rulesBasic, by = "levels")
summary(rulesBasicAgg)
inspect(head(rulesBasicAgg,100))


?addAggregate
#poziom1
grocHier <- addAggregate(adultTR, "levels") 
summary(grocHier)
inspect(head(grocHier,30))
View(grocHier@itemInfo)

#dodanie poziomu nr 2
hLevel2 <-  paste(grocHier@itemInfo$levels, "_l2")  #zmiana nazwy elementów na 2 poziomie hierarchii.
grocHier@itemInfo$myH <- as.factor(hLevel2)

grocTFullHier <- addAggregate(grocHier, "myH") 
View(grocTFullHier@itemInfo)

#odkrywanie reguł
rulesAggFH <- apriori(grocTFullHier, parameter=list(supp=0.1, conf=0.5, minlen=2)) 
rulesLift1.2 <- subset(rulesAggFH, subset =  lift > 1.2)
sorted <- sort(rulesLift1.2, by = "support")
summary(sorted)
sorted <- filterAggregate(sorted)
summary(sorted)
inspect(sorted[100:200])

rulesInGivenConseq <- subset(sorted, subset = rhs %in% "income=large")
#szukam reguł wskazujących na duży dochód
inspect(head(rulesInGivenConseq,100))


rulesAggFH <- apriori(grocTFullHier, parameter=list(supp=0.2, conf=0.5, minlen=2)) 
rulesLift1.2 <- subset(rulesAggFH, subset =  lift > 1.2)
sorted <- sort(rulesLift1.2, by = "support")
summary(sorted)
sorted <- filterAggregate(sorted)
summary(sorted)
inspect(sorted[100:200])

rulesInGivenConseq <- subset(sorted, subset = rhs %in% "income=large")
#szukam reguł wskazujących na duży dochód
inspect(head(rulesInGivenConseq,100))


rulesInGivenConseq <- subset(sorted, subset = rhs %in% "race=black")
#szukam reguł wskazujących na rasę czarną
inspect(head(rulesInGivenConseq,100))


plot(sorted[size(sorted) == 4], method = "graph")
# z obserwacji wynika, że zbiory reguł  są dużo większe i też trudniej znaleść wpierającą duży odsetek badnaych danych.
# zwiększanie wartości wspracia jak i zaufanie zmniejsza ilośc generowanych reguł w każdej z metod











