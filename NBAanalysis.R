##??ŠNBA14-15??„è?‡æ?™è?€?€²ä??
if (!require('SportsAnalytics')){
  install.packages("SportsAnalytics")
  library(SportsAnalytics)
}
NBA1415<-fetch_NBAPlayerStatistics("14-15")
NBA1415
install.packages("data.table")
library(data.table)

##??„é?Šæ?€è¾›è‹¦??„ç?ƒå“¡
MaxPlayed<-aggregate(TotalMinutesPlayed~Team,NBA1415,max)
#tapply(NBA1415$TotalMinutesPlayed,NBA1415$Team,max)
NBA1415MaxPlayed<-merge(NBA1415,MaxPlayed)
output<-NBA1415MaxPlayed[order(NBA1415MaxPlayed$TotalMinutesPlayed,decreasing = T),c("Team","Name","TotalMinutesPlayed")]
library(knitr)
kable(output, digits=2)

##??„é?Šå?—å?†ç??
MaxPoint<-aggregate(TotalPoints~Team,NBA1415,max)
#tapply(NBA1415$TotalPoints,NBA1415$Team,max)
NBA1415MaxPoint<-merge(NBA1415,MaxPoint)
output<-NBA1415MaxPoint[order(NBA1415MaxPoint$TotalPoints,decreasing = T),c("Team","Name","TotalPoints")]
library(knitr)
kable(output, digits=2)

##??„é?Šæ?€??‰æ?ˆç?‡ç?„ç?ƒå“¡
NBA1415DT<-data.table(NBA1415)
output<-NBA1415DT[,list(Efficiency=round(sum(TotalPoints)/sum(TotalMinutesPlayed),digits = 2)),
                  by=list(Team,Name)] [order(Efficiency,decreasing = T)]
library(knitr)
kable(output, digits=2)

##??„é?Šä?‰å?†ç?ƒå‡º??‹æ?€æº–ç?„ç?ƒå“¡
NBA1415DT<-data.table(NBA1415)
output<-NBA1415DT[,list(ThreePerc=round(sum(ThreesMade)/sum(ThreesAttempted),digits = 2)),
                  by=list(Team,Name)] [order(ThreePerc,decreasing = T)]
library(knitr)
kable(output, digits=2)