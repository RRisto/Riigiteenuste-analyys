source("helpers.R")
#enne korrigeerisin andmeid käsitsi, kuna veerud on kohati paigast ära
toetused=read.csv2("./andmed/struktuuritoetused_2016-08-10.csv")
toetused$toetuse_andja=gsub(" \\(muu abi rü\\)| norra rü ","",
                            tolower(toetused$toetuse_andja))
toetused$toetuse_andja=gsub("aktsiaselts","", toetused$toetuse_andja)
#paneme andjatele regkoodid juurde
andjaNimi=unique(toetused$toetuse_andja)
andjaRegkood=RegKoodiLeidja(AsutNimed = andjaNimi, 
                           lisaandmedNimed = statAmet$NIMI,
                           lisaandmedRegkood = statAmet$REGISTRIKOOD)
struktAnjaNimiRegkood=data.frame(andjaNimi, andjaRegkood)
#mergime kokku
toetused=merge(toetused, struktAnjaNimiRegkood, by.x = "toetuse_andja",
               by.y="andjaNimi", all.x = T)
#teeme tabelid erimevate summaarsete asjade kohta
ItMeetmed=c("3.2.12 Info- ja kommunikatsioonitehnoloogiate teadus- ja arendustegevuse toetamine",
            "1.2.5 Kohandumine teadmistepõhise majandusega",
            "3.5.1 Infoühiskonna teadlikkuse tõstmise programm",
            "4.5 Infoühiskonna arendamine",
            "2014-2020.12.3 Avalike teenuste pakkumise arendamine",
            "3.5.2 Infoühiskonna edendamine avatud taotluste kaudu",
            "2014-2020.11.2 Nutika teenuste taristu arendamine",
            "3.5.3 Infoühiskonna edendamine investeeringute kavade kaudu")
#hoiame alles ainult read, mis vastavad neile
toetusedIt=toetused[toetused$meetme_kood_koos_nimetusega%in%ItMeetmed,]
#saaja lõikes summa
library(dplyr)
SaajaSummaIT=toetusedIt%>%
  group_by(toetuse_saaja_nimi)%>%
  summarise(summa=sum(projekti_toetus),
            keskmine=mean(projekti_toetus),
            mediaan=median(projekti_toetus))
#eraldi kus on nimede asemel regkoodid mergimiseks
RegkoodSummaIT=toetusedIt%>%
  group_by(toetuse_saaja_registri_kood)%>%
  summarise(summa=sum(projekti_toetus),
            keskmine=mean(projekti_toetus),
            mediaan=median(projekti_toetus))
#osakaal
SaajaSummaIT$OsakaalSummast=SaajaSummaIT$summa/sum(SaajaSummaIT$summa)
RegkoodSummaIT$OsakaalSummast=RegkoodSummaIT$summa/sum(RegkoodSummaIT$summa)

hist(SaajaSummaIT$Osakaal)
#plotime suuremad saajad
library(ggplot2)
ggplot(SaajaSummaIT[SaajaSummaIT$OsakaalSummast>0.01,], aes(
  x=reorder(toetuse_saaja_nimi, summa),y=summa/1000000, 
  label=paste0(round(OsakaalSummast*100,1),"%")))+
  geom_bar(stat="identity", fill="lightblue")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Summa (mln €)")+
  geom_text(aes(y = summa/1000000+1))

#plotime keskmise toetuse summa
ggplot(SaajaSummaIT[SaajaSummaIT$keskmine>100000,], 
       aes(x=reorder(toetuse_saaja_nimi, keskmine),y=keskmine/1000000,
           label=round(keskmine/1000000,3)))+
  geom_bar(stat="identity", fill="lightblue")+
   theme_minimal()+
  ylab("Summa (mln €)")+
  geom_text(aes(y = keskmine/1000000+0.06))+
  coord_flip()

#plotime kogu summa osakaalu järgi keskmise toetuse
ggplot(SaajaSummaIT, aes(x=OsakaalSummast, y=keskmine/100000))+
  geom_point()+
  theme_minimal()+
  geom_text(aes(label = toetuse_saaja_nimi), check_overlap = T,
            hjust = 0)
  

