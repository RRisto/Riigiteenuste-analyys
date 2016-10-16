#andmed sisse
regulatsioon=andmedLai$regulatsioon
names(regulatsioon)=andmedLai$identifikaator
#dfks
library(reshape2)
regulatsioonDf=melt(regulatsioon)
names(regulatsioonDf)=c("link", "id")
#palju on riigiteataja asju
sum(grepl("riigiteataja", regulatsioonDf$link))/nrow(regulatsioonDf)
#jätame alles riigitetaaj õigusaktid
riigit=regulatsioonDf[grepl("riigiteataja", regulatsioonDf$link),]
#mis on need, kus pole riigitetajat
muu=regulatsioonDf[!grepl("riigiteataja", regulatsioonDf$link),]
#päris paljud on ELi omad, mis on pdfid
riigit$link=tolower(as.character(riigit$link))
#osades on ühele väljale mitu õigusakti pandud, need teeme eraldi ridadeks
library(stringr)
strCount=str_count(riigit$link, "riigiteataja")
mitmekordsed=riigit[strCount>1,]
link=c()
id=c()
for(i in 1:nrow(mitmekordsed)) {
  lingid=strsplit(mitmekordsed$link[i], split=",|;")
  ids=rep(mitmekordsed$id[i], length(lingid[[1]]))
  link=c(link, lingid)
  id=c(id, ids)
}
tulem=data.frame(melt(link), id)
tulem$L1=NULL
names(tulem)=c("link","id")
riigit=riigit[strCount==1,]
riigit=rbind(riigit, tulem)
riigit$link=str_trim(riigit$link, side="both")
#paneme kõigile leiaKehtiv juurde
for(i in 1:nrow(riigit)){
  if(!grepl("leiakehtiv", riigit$link[i])) {
    riigit$link[i]=paste0(riigit$link[i], "?leiakehtiv")
  } else {
    next
  }
}
#vaatame topi
library(dplyr)
topAktid=riigit%>%
  group_by(link)%>%
  summarise(n=n())

topAktid=topAktid[order(-topAktid$n),]

#korjame õigusaktide pealkirjad
topAktid$pealkiri=NA
library(rvest)
for(i in 1:nrow(topAktid)) {
#for(i in 324:nrow(topAktid)) {
  tryget <- try({ #selleks et erroriga seisma ei jääks
    page=read_html(topAktid$link[i])
  pealkiri=page %>% 
    html_node("#article-content > h1:nth-child(1)") %>%
    html_text()
  topAktid$pealkiri[i]=pealkiri
  Sys.sleep(1)
  cat(i, pealkiri, "\n")
  })
  if(class(tryget)=='try-error') { #kui erroro annab teate ja liigub edasi
    cat(i, "pealkirja pole", "\n")
    topAktid$pealkiri[i]=NA
    next
  }
}
#osad lingid viitasid samale seadusele, kuid olid erinevad, uurime neid
topAktid[grepl("Käibemaksuseadus1", topAktid$pealkiri),"link"]
#uurime NAsid
topAktid[ is.na(topAktid$pealkiri),]

#kus on NAd proovime nati teist selectorit
for(i in 1:nrow(topAktid)) {
  if(is.na(topAktid$pealkiri[i])) {
    tryget <- try({
      page=read_html(topAktid$link[i])
      pealkiri=page %>% 
        html_node("#article-content > h1:nth-child(2)") %>%
        html_text()
      topAktid$pealkiri[i]=pealkiri
      Sys.sleep(1)
      cat(i, pealkiri, "\n")
    })
    if(class(tryget)=='try-error') { 
      cat(i, "pealkirja pole", "\n")
      topAktid$pealkiri[i]=NA
      next
    }
  } else {
    next
  }
}
#nüüd on ainult paar NAd (ja neid ei oska ma a otsida)
#kuna osad lingid olid küll eirnevad kuid viitasid samale seadusele (nt eri
#paragrahvidele), siis bindin algsete andmetega kokku ja teen uue grupeeringu
riigit=merge(riigit, topAktid[,c("link", "pealkiri")])
#saveime
saveRDS(riigit,file="./andmed/riigiteatajaOigusaktid.RDS")
#aktide viited
AktideViited=riigit%>%
  group_by(pealkiri)%>%
  summarise(n=n())
#järjekorda
AktideViited=AktideViited[order(-AktideViited$n),]
AktideViited$lyhem=substr(AktideViited$pealkiri,1,50)
#plotime top10
library(ggplot2)
ggplot(AktideViited[1:40,], aes(x=reorder(lyhem,n), y=n))+
  geom_point()+
  theme_minimal()+
 # scale_x_discrete(labels = abbreviate)+
  coord_flip()
#kõik pildil
ggplot(AktideViited, aes(x=reorder(lyhem,n), y=n))+
  geom_point()+
  theme_minimal()+
   scale_x_discrete(labels = abbreviate)
  
#teenused, millel on enim seadusi
#lisame enne teenuse nimed
riigit=merge(riigit, andmedLai[,c("identifikaator","nimetus" )], by.x="id",
              by.y="identifikaator", all.x =T)
#arvutame summad
teenusAkte=riigit%>%
  group_by(nimetus)%>%
  summarise(n=n())
teenusAkte=teenusAkte[order(-teenusAkte$n),]

idAkte=riigit%>%
  group_by(id)%>%
  summarise(n=n())
#plotime
ggplot(teenusAkte[1:30,], aes(x=reorder(nimetus,n), y=n))+
  geom_point()+
  theme_minimal()+
  coord_flip()
#pltoime seose seaduste ja teenuste vahel
library(igraph)
#seadused, millele viidatakse rohkem kui kolm korda

network=riigit[, c("pealkiri", "id")]
#eemaldame kus on vähe viiteid
network=network[network$pealkiri%in% AktideViited$pealkiri[AktideViited$n>30]|network$id%in% idAkte$id[idAkte$n>30],]
#network=network[network$id%in% idAkte$id[idAkte$n>8],]

g=graph_from_data_frame(network,directed = F)
plot(g)