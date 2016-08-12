library(readxl)
kogupalk=read_excel("./andmed/Aasta_kogupalk_2015.a_riik.xlsx", skip=6)
kogupalk=kogupalk[-1,]
#asutuse keskmine ja mediaan
#normaliseerime täistööajale
kogupalk$tasuNormal=kogupalk$`Kokku     (8+9+10)`/
  kogupalk$`Ametniku koormus ametikohal`
kogupalk$pohipalkNormal=kogupalk$`Põhipalk, sh puhkusetasu`/
  kogupalk$`Ametniku koormus ametikohal`
#mitu kuud töötas, normaliseerime ühele kuule
library(lubridate)
temp=strsplit(as.character(kogupalk$Periood), split="-")
kogupalk$kestvusKuudes=c(difftime(dmy(lapply(temp, `[[`, 2)),
                                  dmy(lapply(temp, `[[`, 1)),
                                  units="days")/30)
#ühe lisan käsitsi kuna seal kaks perioodi!!!
#kogupalk$kestvusKuudes[6334]=6.367
kogupalk$tasuNormKuu=kogupalk$tasuNormal/kogupalk$kestvusKuudes
kogupalk$pohipalkNormKuu=kogupalk$pohipalkNormal/kogupalk$kestvusKuudes
#võtame keskminse ja mediaanid
library(dplyr)
pohipalkAsut=kogupalk%>%
  group_by(Asutus)%>%
  summarise(meanTasu=mean(tasuNormKuu),
            medNTasu=median(tasuNormKuu),
            meanPohip=mean(pohipalkNormKuu),
            medPohip=median(pohipalkNormKuu),
            sdTasu=sd(tasuNormKuu),
            sdPohip=sd(pohipalkNormKuu))
#paneme külge ka regkoodid
source("helpers.R")
pohipalkAsut$regkood=RegKoodiLeidja(AsutNimed = pohipalkAsut$Asutus, 
                                    lisaandmedNimed = statAmet$NIMI,
                                    lisaandmedRegkood = statAmet$REGISTRIKOOD)
#salvestame
saveRDS(pohipalkAsut,file="./andmed/pohipalkAsut.RDS")
pohipalkAsut=readRDS("./andmed/pohipalkAsut.RDS")