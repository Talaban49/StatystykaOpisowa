dataFramesConversion <- function(index){
  df <- data.frame()
  start=3+14*(index -1)
  end=16+14*(index -1)
  
  for(i in 1:12)
    df <- rbind(df,data.frame(matrix(unlist(AAAAdata[(start+14*10*(i-1)):(end+14*10*(i-1))]),nrow = 17)))
  
  return(df)
}


v?ivodeshipYearAverage <- function(Frame){
  means <- data.frame()
  for(j in 1:17){
    numbers <- list(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    #numbers <- list(rep(0,14))
    for(i in 1:12){
      numbers <- mapply("+", numbers, Frame[j+17*(i-1),] , SIMPLIFY = FA?SE)
    }
    tmp <- data.frame(matrix(unlist(numbers),nrow=1)/12)
    means <- rbind(means,tmp)
  }

  return (means)
}

barprint <- function(data, args, title){
  barplot(
    unlist(data),
    main = title,
    xlab = "Przedzia³ Obserwacji[rok]",
    yl?b = "Œrednia cena produktu[PLN]",
    names.arg = args,
    col = "darkred"
    
  )
  return()
}

AAAAdata <-(read.csv(file = ".\\CENY_2917_CTAB_20200211231701.csv",header = TRUE,sep = ';', dec = ',', stringsAsFactors = FALSE, encoding = 'UTF-8'))
voivode?hips <- c("Polska","Dolnoslaskie","Kujawsko-Pomorskie","Lubelskie","Lubuskie","Lodzkie","Malopolskie",
                  "Mazowieckie","Opolskie","Podkarpackie","Podlaskie","Pomorskie","Slaskie","Swietokrzyskie",
                  "Warminsko-Mazurskie","Wi?lkopolskie","Zachodniopomorskie")
years <- c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")

Szynka <- dataFramesConversion(1)
SzynkaAvg <- voivodeshipYearAverage(Szynka)
barprint(SzynkaAvg[1,], years, "S?ynka")

Morszczuk <- dataFramesConversion(2)
MorszczukAvg <- voivodeshipYearAverage(Morszczuk)
barprint(MorszczukAvg[1,], years, "Morszczuk")

Pomarancza <- dataFramesConversion(3)
PomaranczaAvg <- voivodeshipYearAverage(Pomarancza)
barprint(PomaranczaAvg[?,][8:14], years[8:14], "Pomarañcza")

Cebula <- dataFramesConversion(4)
CebulaAvg <- voivodeshipYearAverage(Cebula)
barprint(CebulaAvg[1,], years, "Cebula")

Kawa <- dataFramesConversion(5)
KawaAvg <- voivodeshipYearAverage(Kawa)
barprint(KawaAvg[1,], year?, "Kawa")

Piwerko <- dataFramesConversion(6)
PiwerkoAvg <- voivodeshipYearAverage(Piwerko)
barprint(PiwerkoAvg[1,], years, "Piwo jasne")

Garnitur <- dataFramesConversion(7)
GarniturAvg <- voivodeshipYearAverage(Garnitur)
barprint(GarniturAvg[1,][13:14], ?ears[13:14], "Garnitur")

ButyDamskie <- dataFramesConversion(8)
ButyDamskieAvg <- voivodeshipYearAverage(ButyDamskie)
barprint(ButyDamskieAvg[1,], years, "Buty Damskie")

BateriaZmywakowa <- dataFramesConversion(9)
BateriaZmywakowaAvg <- voivodeshipYearAv?rage(BateriaZmywakowa)
barprint(BateriaZmywakowaAvg[1,], years, "Bateria Zmywakowa")

FryzjerMeski <- dataFramesConversion(10)
FryzjerMeskiAvg <- voivodeshipYearAverage(FryzjerMeski)
barprint(FryzjerMeskiAvg[1,], years, "Fryzjer mêski")