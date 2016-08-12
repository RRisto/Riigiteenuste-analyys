#andmed sisse
library(riigiteenused)
source("helpers.R")
andmedLai=andmedSisse()
andmedPikk=andmedPikaks(andmedLai)
asutused=unique(andmedLai$allasutus)
#statameti andmebaas
statAmet=read.csv2("./andmed/StatametiMajAktiivsed01.12.2015.csv")
#hoiame muutujad, mis vajalikud
statAmet=statAmet[,c("REGISTRIKOOD", "NIMI")]

#paneme asutustele regkoodid juurde
AsutRegkood=RegKoodiLeidja(AsutNimed = asutused, 
                           lisaandmedNimed = statAmet$NIMI,
                     lisaandmedRegkood = statAmet$REGISTRIKOOD)
AsutRegkood=data.frame(regkood=AsutRegkood, 
                       nimi=unique(andmedLai$allasutus),
                       stringsAsFactors = F)
#käsitsi juurde neile, kellele ei leidnud
AsutRegkood[AsutRegkood$nimi==
              "C. R. Jakobsoni Talumuuseum", "regkood"]="70002584"
AsutRegkood[AsutRegkood$nimi==
              "Tööstusomandi apellatsioonikomisjon", "regkood"]="70000898"#jumi kood!!!
AsutRegkood[AsutRegkood$nimi==
              "Eesti Sõjamuuseum", "regkood"]="70006139"
