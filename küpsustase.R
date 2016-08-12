#küpsustase arvutmine
source("helpers.R")
kupsusTase=KupsusTaseAsutus(andmedPikk = andmedPikk, andmedLai = andmedLai)
#paneme ka juurde ka regkoodid
Koond=merge(kupsusTase, AsutRegkood, by.x="allasutus", by.y="nimi")
#paneme juurde Struktuuri toetuste andmed
Koond=merge(Koond, RegkoodSummaIT, by.x="regkood", 
            by.y="toetuse_saaja_registri_kood", all.x=T)
#kus summad puudu paneme 0
Koond[is.na(Koond)]<-0
#plotime asju
library(ggplot2)
#summa küpsustaseme järgi
ggplot(Koond, aes(x=factor(kupsus), y=summa/1000000))+
  geom_boxplot()+
  ylab("Summa (mln €)")
#mediaanide jaotus
ggplot(Koond, aes(x=factor(kupsus), y=mediaan/1000))+
  geom_boxplot()+
  ylab("Mediaan summa (tuh €)")
#keskmine toetus
ggplot(Koond, aes(x=factor(kupsus), y=keskmine/1000))+
  geom_boxplot()+
  ylab("Keskmine summa (tuh €)")
#osakaal kogusummast
ggplot(Koond, aes(x=factor(kupsus), y=OsakaalSummast))+
  geom_boxplot()+
  ylab("Osakaal kogu toetuse summast")
#teenuste arv
ggplot(Koond, aes(x=factor(kupsus), y=teenusteArv))+
  geom_boxplot()+
  ylab("Teenuste arv")

######################paneme igale teenusele juurde kas on omanik ja
#ja statistika ning siis liidame % kokku
andmedLai$OnOmanik=ifelse(andmedLai$omanikunimi==""&
                               andmedLai$omanikuamet==""&
                               andmedLai$omanikuemail==""&
                               andmedLai$omanikutelefon=="", 0, 1)
#kupsus teenuste omanike olemasolu mõistes
library(dplyr)
OmanikAsutusLoikes=andmedLai%>%
  group_by(ministeerium, allasutus)%>%
  summarise(omanikOsakaal = mean(OnOmanik),
            teenusteArv=length(OnOmanik))
#mõõdikute osakaal
moodikAsutusLoikes=andmedPikk %>%
  group_by(ministeerium,allasutus) %>%
  summarise(onMoodik=sum(!is.na(value)),
            moodikArv=n())%>%
  mutate(moodikOsakaal=onMoodik/moodikArv)
#regkoodid juurde
moodikAsutusLoikes=merge(moodikAsutusLoikes, OmanikAsutusLoikes,
                         by.x="allasutus", by.y="allasutus")
moodikAsutusLoikes=merge(moodikAsutusLoikes, AsutRegkood, by.x="allasutus",
                         by.y="nimi")
#hoiame ainult vajaliku alles
moodikAsutusLoikes=moodikAsutusLoikes[, c("regkood", "omanikOsakaal",
                                          "moodikOsakaal"), with = FALSE]
#Koondisse juurde
Koond=merge(Koond, moodikAsutusLoikes, by.x="regkood", by.y="regkood")
Koond$KupsusSkoor=Koond$omanikOsakaal+Koond$moodikOsakaal

#plotime nüüd seoseid
library(ggplot2)
ggplot(Koond, aes(x=KupsusSkoor, y=summa/1000000))+
  geom_point()+
  ylab("Summa (mln €)")
#mediaanide jaotus
ggplot(Koond, aes(x=KupsusSkoor, y=mediaan/1000))+
  geom_point()+
  ylab("Mediaan summa (tuh €)")
#keskmine toetus
ggplot(Koond, aes(x=KupsusSkoor, y=keskmine/1000))+
  geom_point()+
  ylab("Keskmine summa (tuh €)")
#osakaal kogusummast
ggplot(Koond, aes(x=KupsusSkoor, y=OsakaalSummast))+
  geom_point()+
  ylab("Osakaal kogu toetuse summast")
#teenuste arv
ggplot(Koond, aes(x=KupsusSkoor, y=teenusteArv))+
  geom_point()+
  ylab("Teenuste arv")

######################paneme juurde palgainfo
Koond=merge(Koond, pohipalkAsut, by.x="regkood", by.y="regkood", all.x = T)
Koond$Asutus=NULL
#visualiseerime huvitavamaid asju
ggplot(Koond, aes(x=meanTasu,y=KupsusSkoor))+
  geom_point()+
  theme_minimal()+
  geom_smooth()+
  geom_text(aes(label=allasutus),check_overlap = TRUE)+
  facet_wrap(~ministeerium, scales="free")
#mediaan tasu vs kupsus
ggplot(Koond, aes(x=medNTasu,y=KupsusSkoor))+
  geom_point()+
  theme_minimal()+
  geom_smooth()+
  geom_text(aes(label=allasutus),check_overlap = TRUE)+
  facet_wrap(~ministeerium, scales="free")
#sd tasu vs kupsus
ggplot(Koond, aes(x=sdTasu,y=KupsusSkoor))+
  geom_point()+
  theme_minimal()+
  geom_smooth()+
  geom_text(aes(label=allasutus),check_overlap = TRUE)+
  facet_wrap(~ministeerium, scales="free")
#keskmine pohipalk vs kupsus
ggplot(Koond, aes(x=meanPohip,y=KupsusSkoor))+
  geom_point()+
  theme_minimal()+
  geom_smooth()+
  facet_wrap(~ministeerium, scales="free")
#mediaan pohipalk vs kupsus
ggplot(Koond, aes(x=medPohip,y=KupsusSkoor))+
  geom_point()+
  theme_minimal()+
  geom_smooth()+
  facet_wrap(~ministeerium, scales = "free")
#sd pohipalk vs kupsus
ggplot(Koond, aes(x=sdPohip,y=KupsusSkoor))+
  geom_point()+
  theme_minimal()+
  geom_smooth()+
  geom_text(aes(label=allasutus),check_overlap = TRUE)+
  facet_wrap(~ministeerium)

#mean tasu vs kupsustase
ggplot(Koond, aes(x=factor(kupsus),y=meanTasu))+
  geom_boxplot()+
  facet_wrap(~ministeerium)
#mediaan tasu vs kupsustase
ggplot(Koond, aes(x=factor(kupsus),y=medNTasu))+
  geom_boxplot()+
  facet_wrap(~ministeerium)
#mean pohipalk vs kupsustase
ggplot(Koond, aes(x=factor(kupsus),y=meanPohip))+
  geom_boxplot()+
  facet_wrap(~ministeerium)
#median pohip vs kupsustase
ggplot(Koond, aes(x=factor(kupsus),y=medPohip))+
  geom_boxplot()+
  facet_wrap(~ministeerium)
#sd tasu vs kupsustase
ggplot(Koond, aes(x=factor(kupsus),y=sdTasu))+
  geom_boxplot()+
  facet_wrap(~ministeerium)
#sd pohip vs kupsustase
ggplot(Koond, aes(x=factor(kupsus),y=sdPohip))+
  geom_boxplot()+
  facet_wrap(~ministeerium)

########paneme juurde riigihangete teema
rhr=readRDS("./andmed/rhrKoond.RDS")
rhrAsut=rhr%>%
  group_by(Reg..kood)%>%
  summarise(meanEelM=mean(eeldatav_maksumus),
            meanLepM=mean(lepingu_maksumus),
            meanTegM=mean(tegelik_maksumus),
            meanPer=mean(kestvus_kuudes),
            medEelM=median(eeldatav_maksumus),
            medLepM=median(lepingu_maksumus),
            medTegM=median(tegelik_maksumus),
            medPer=median(kestvus_kuudes))

#paneme koondisse juurde
Koond=merge(Koond, rhrAsut, by.x="regkood", by.y="Reg..kood", all.x = T)
#plotime, mediaan eeldatav maksumus vs kupsusskoor
ggplot(Koond[!is.na(Koond$meanEelM),], aes(x=log(meanEelM), y=KupsusSkoor))+
  geom_point()+
  facet_wrap(~ ministeerium, scales = "free")+
  coord_cartesian(ylim=c(0,2))
#mean lepingu maksumus vs kupsuskoor
ggplot(Koond[!is.na(Koond$meanLepM),], aes(x=log(meanLepM), y=KupsusSkoor))+
  geom_point()+
  facet_wrap(~ ministeerium, scales = "free")
#mean tegelik maksumus vs kupsusskoor
ggplot(Koond[!is.na(Koond$meanTegM),], aes(x=log(meanTegM), y=KupsusSkoor))+
  geom_point()+
  facet_wrap(~ ministeerium, scales = "free")+
  coord_cartesian(ylim=c(0,2))
#mean keskmine lepingu periood vs kupsusskoor
ggplot(Koond[!is.na(Koond$meanPer),], aes(x=log(meanPer), y=KupsusSkoor))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ ministeerium, scales = "free")+
  coord_cartesian(ylim=c(0,2))

#median eeldatav maksumus vs kupsusskoor
ggplot(Koond[!is.na(Koond$medEelM),], aes(x=log(medEelM), y=KupsusSkoor))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ ministeerium, scales = "free")+
  coord_cartesian(ylim=c(0,2))
#median lepingu maksumus vs kupsuskoor
ggplot(Koond[!is.na(Koond$medLepM),], aes(x=log(medLepM), y=KupsusSkoor))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ ministeerium, scales = "free")+
  coord_cartesian(ylim=c(0,2))
#median tegelik maksumus vs kupsusskoor
ggplot(Koond[!is.na(Koond$medTegM),], aes(x=log(medTegM), y=KupsusSkoor))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ ministeerium, scales = "free")+
  coord_cartesian(ylim=c(0,2))
#mean keskmine lepingu periood vs kupsusskoor
ggplot(Koond[!is.na(Koond$meanPer),], aes(x=log(meanPer), y=KupsusSkoor))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ ministeerium, scales = "free")+
  coord_cartesian(ylim=c(0,2))
