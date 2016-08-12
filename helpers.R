#funktsioon, mis leiab similarity skoori kahe stringi vahel ning
#valib välja maksimaalselt sarnase stringi registrikoodi
SimMatch=function(NimedKoodita, lisaandmedNimed, lisaandmedRegkood, 
                  minSim=0.78) {
  #input:
  #-NimedKoodita (char/factor vector)=nimed, millele otsime regkoodi
  #-lisaandmedNimed(char/factor vector)=nimed, millel on regkood olemas, andme-
  #tabel, kust otsime vasteid
  #-lisaandmedRegkood(factor/numeric vector)=regkoodid, mille järjekord on
  #sama kui lisaandmedNimed
  #-minSim (numeric)=minimaalne sarnasus, mis peab olema, et üldse kaaluks 
  #sarnasuse otsimisest kahe stringi vahel. Kui kõik sarnasuse skoorid on
  #alla selle väärtuse tagastab väärtuse NA
  #output: 
  #data.frame järgnevate veergudega: NimedKoodita,nimed 
  #(puhastatud nimede vaste NimedKoodita nimdele),regkoodid, maxSimilarity
  #(sarnasuse skoor, mis kahe stringi vahel on, kui vaste leiti)
  library(stringdist)
  nimed=c()
  maxSimilarity=c()
  regkoodid=c()
  for(i in 1:length(NimedKoodita)) {
    #iga lisanamdete nimele arvutame similarity skoori
    dists=stringsim(as.character(NimedKoodita[i]), 
                    as.character(lisaandmedNimed))
    #kui üle minSimi väärtuse skoore pole, siis return NA
    if(length(dists[dists>=minSim])==0) {
      nimed[i]=NA
      regkoodid[i]=NA
      maxSimilarity[i]=NA
      cat("Sarnase nime otsimine. Töötan juhtumi", i, "kallal", 
          paste0(length(NimedKoodita), "-st \n"))
    } else {
      maxSim=max(dists[dists>=minSim])#maksimaalne sarnasus
      #kui on mitu vastet, siis valin esimese, praegu igatahes töötab
      nimed[i]=as.character(lisaandmedNimed[which(dists==maxSim)])[1]
      regkoodid[i]=as.character(lisaandmedRegkood[which(dists==maxSim)])[1]
      maxSimilarity[i]=dists[dists==maxSim][1]
      cat("Sarnase nime otsimine. Töötan juhtumi", i, "kallal", 
          paste0(length(NimedKoodita), "-st \n"))
    }
  }
  data.frame(NimedKoodita,nimed,regkoodid, maxSimilarity)
}
#funktsioon, mis esialgu leiab sarnasuse otsese matchimise teel, 
#seejärel läheb stringi sarnasusi vaatama
RegKoodiLeidja=function(AsutNimed, lisaandmedNimed, lisaandmedRegkood, 
                        minSim=0.78) {
  #input:
  #-AsutNimed(char/factor vector)=asutuste nimed, millele otsime regkoodi
  #-lisaandmedNimed(char/factor vector)=nimed, millel on regkood olemas, andme-
  #tabel, kust otsime vasteid
  #-lisaandmedRegkood(factor/numeric vector)=regkoodid, mille järjekord on
  #sama kui lisaandmedNimed
  #-minSim (numeric)=minimaalne sarnasus, mis peab olema, et üldse kaaluks 
  #sarnasuse otsimisest kahe stringi vahel. Kui kõik sarnasuse skoorid on
  #alla selle väärtuse tagastab väärtuse NA
  #output:
  #-regkoodid (character vector), mis on samas järjekorrask kui AsutNimed
  AsutNimed=tolower(AsutNimed)
  lisaandmedNimed=tolower(lisaandmedNimed)
  #lihtne match
  tulem=lisaandmedRegkood[match(AsutNimed, lisaandmedNimed)]
  cat("Üks ühele leidsin vaste", sum(!is.na(tulem)),
      "nimele, lähen sarnasusi otsima", sum(is.na(tulem)), "nimele \n")
  #eemaldame andmetest sodi
  lisaandmedNimed=gsub("sihtasutus|^sa | sa$|[[:punct:]]","",lisaandmedNimed)
  lisaandmedNimed=trimws(lisaandmedNimed, which = c("both"))#eemaldame tühikud
  AsutNimedPuht=gsub("sihtasutus|^sa | sa$|[[:punct:]]","",AsutNimed)
  AsutNimedPuht=trimws(AsutNimedPuht, which = c("both"))
  #proovime stringmatchi
  tulem[is.na(tulem)]=SimMatch(
    NimedKoodita =AsutNimedPuht[is.na(tulem)],lisaandmedNimed, 
    lisaandmedRegkood, minSim = minSim)$regkoodid
  as.character(tulem)
}

#######teeme funktsiooni asutuse küpsuse arvutamiseks
KupsusTaseAsutus=function(andmedPikk, andmedLai) {
  #muutujad, mida vajame
  var=c("ministeerium", "allasutus", "omanikunimi", "omanikuamet" ,
        "omanikutelefon", "omanikuemail")
  andmedLaiSub=andmedLai[,var]
  #teeme uue colmni, kuhu paneme väärtuse 1 kui on omanik (e-mail või 
  #telefon või nimi)
  andmedLaiSub$OnOmanik=ifelse(andmedLaiSub$omanikunimi==""&
                                 andmedLaiSub$omanikuamet==""&
                                 andmedLaiSub$omanikuemail==""&
                                 andmedLaiSub$omanikutelefon=="", 0, 1)
  #kupsus teenuste omanike olemasolu mõistes
  library(dplyr)
  OmanikAsutusLoikes=andmedLaiSub%>%
    group_by(ministeerium, allasutus)%>%
    summarise(omanikOsakaal = round(mean(OnOmanik)*100),
              teenusteArv=length(OnOmanik))%>%
    mutate(kupsus=ifelse(omanikOsakaal>=80, 3, 2)) 
  #võtame välja, kus tase 3 olemas
  tase3=OmanikAsutusLoikes$allasutus[OmanikAsutusLoikes$kupsus==3]
  #äkki on neil kõrgem tase e 80% teenuste igal kanalil on mõõdik olemas
  andmedPikkSub=andmedPikk[andmedPikk$allasutus %in% tase3, ]
  moodikAsutusLoikes=andmedPikkSub %>%
    group_by(ministeerium,allasutus) %>%
    summarise(onMoodik=sum(!is.na(value)),
              moodikArv=n())%>%
    mutate(moodikOsakaal=onMoodik/moodikArv)%>%
    mutate(kupsus=ifelse(moodikOsakaal>=0.8, 4, 3))
  #paneme andmed kokku ja näitame välja
  OmanikAsutusLoikes[OmanikAsutusLoikes$kupsus==3,]$kupsus=
    moodikAsutusLoikes$kupsus
  #paneme ministeeriumite kaupa järjekorda
  OmanikAsutusLoikes[order(OmanikAsutusLoikes$ministeerium, 
                           OmanikAsutusLoikes$allasutus),
                     c("ministeerium", "allasutus", "teenusteArv","kupsus")]
}
