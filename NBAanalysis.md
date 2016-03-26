##把NBA14-15的資料讀進來
```{r results='hide',message=FALSE,cache=T}
if (!require('SportsAnalytics')){
  install.packages("SportsAnalytics")
  library(SportsAnalytics)
}
NBA1415<-fetch_NBAPlayerStatistics("14-15")
NBA1415
install.packages("data.table")
library(data.table)
```
##各隊最辛苦的球員
```{r}
MaxPlayed<-aggregate(TotalMinutesPlayed~Team,NBA1415,max)
#tapply(NBA1415$TotalMinutesPlayed,NBA1415$Team,max)
NBA1415MaxPlayed<-merge(NBA1415,MaxPlayed)
output<-NBA1415MaxPlayed[order(NBA1415MaxPlayed$TotalMinutesPlayed,decreasing = T),c("Team","Name","TotalMinutesPlayed")]
library(knitr)
kable(output, digits=2)
```
|   |Team |Name             | TotalMinutesPlayed|
|:--|:----|:----------------|------------------:|
|11 |HOU  |James Harden     |               2979|
|18 |MIN  |Andrew Wiggins   |               2971|
|25 |POR  |Damian Lillard   |               2928|
|13 |LAC  |Chris Paul       |               2860|
|30 |WAS  |John Wall        |               2841|
|24 |PHO  |Eric Bledsoe     |               2799|
|3  |BRO  |Joe Johnson      |               2787|
|6  |CLE  |Kyrie Irving     |               2735|
|7  |DAL  |Monta Ellis      |               2698|
|19 |NOR  |Tyreke Evans     |               2695|
|15 |MEM  |Marc Gasol       |               2690|
|5  |CHI  |Pau Gasol        |               2682|
|26 |SAC  |Ben Mclemore     |               2674|
|8  |DEN  |Ty Lawson        |               2668|
|16 |MIA  |Goran Dragic     |               2641|
|29 |UTA  |Gordon Hayward   |               2618|
|10 |GSW  |Stephen Curry    |               2613|
|9  |DET  |Ke Caldwell-pope |               2591|
|22 |ORL  |Victor Oladipo   |               2572|
|17 |MIL  |G Antetokounmpo  |               2542|
|2  |BOS  |Avery Bradley    |               2427|
|28 |TOR  |Kyle Lowry       |               2422|
|1  |ATL  |Kyle Korver      |               2418|
|12 |IND  |Solomon Hill     |               2380|
|4  |CHA  |Gerald Henderson |               2323|
|23 |PHI  |Nerlens Noel     |               2311|
|27 |SAN  |Danny Green      |               2311|
|21 |OKL  |Russel Westbrook |               2302|
|14 |LAL  |Wesley Johnson   |               2244|
|20 |NYK  |Shane Larkin     |               1864|
##各隊得分王
```{r}
MaxPoint<-aggregate(TotalPoints~Team,NBA1415,max)
#tapply(NBA1415$TotalPoints,NBA1415$Team,max)
NBA1415MaxPoint<-merge(NBA1415,MaxPoint)
output<-NBA1415MaxPoint[order(NBA1415MaxPoint$TotalPoints,decreasing = T),c("Team","Name","TotalPoints")]
library(knitr)
kable(output, digits=2)
```
|   |Team |Name             | TotalPoints|
|:--|:----|:----------------|-----------:|
|11 |HOU  |James Harden     |        2217|
|10 |GSW  |Stephen Curry    |        1900|
|21 |OKL  |Russel Westbrook |        1886|
|6  |CLE  |Lebron James     |        1740|
|25 |POR  |Damian Lillard   |        1720|
|19 |NOR  |Anthony Davis    |        1656|
|13 |LAC  |Chris Paul       |        1564|
|7  |DAL  |Monta Ellis      |        1513|
|29 |UTA  |Gordon Hayward   |        1463|
|5  |CHI  |Pau Gasol        |        1446|
|26 |SAC  |Rudy Gay         |        1432|
|22 |ORL  |Nikola Vucevic   |        1428|
|15 |MEM  |Marc Gasol       |        1413|
|18 |MIN  |Andrew Wiggins   |        1387|
|30 |WAS  |John Wall        |        1385|
|24 |PHO  |Eric Bledsoe     |        1377|
|16 |MIA  |Dwyane Wade      |        1331|
|28 |TOR  |Kyle Lowry       |        1244|
|3  |BRO  |Brook Lopez      |        1236|
|1  |ATL  |Paul Millsap     |        1218|
|8  |DEN  |Ty Lawson        |        1143|
|9  |DET  |Andre Drummond   |        1130|
|2  |BOS  |Isaiah Thomas    |        1101|
|4  |CHA  |Al Jefferson     |        1080|
|27 |SAN  |Tim Duncan       |        1070|
|17 |MIL  |Khris Middleton  |        1055|
|20 |NYK  |Carmelo Anthony  |         966|
|12 |IND  |C.j. Miles       |         942|
|23 |PHI  |Robert Covington |         927|
|14 |LAL  |Jordan Hill      |         841|
##各隊最有效率的球員
```{r}
NBA1415DT<-data.table(NBA1415)
output<-NBA1415DT[,list(Efficiency=round(sum(TotalPoints)/sum(TotalMinutesPlayed),digits = 2)),
                  by=list(Team,Name)] [order(Efficiency,decreasing = T)]
unique(output$Team)
for(team in unique(output$Team)){
  selectTeam<-subset(output,Team==team)
  print(selectTeam[1])
}
```
Team             Name Efficiency
1:  OKL Russel Westbrook       0.82
   Team         Name Efficiency
1:  HOU James Harden       0.74
   Team          Name Efficiency
1:  GSW Stephen Curry       0.73
   Team             Name Efficiency
1:  SAC Demarcus Cousins       0.71
   Team         Name Efficiency
1:  CLE Lebron James        0.7
   Team            Name Efficiency
1:  NYK Carmelo Anthony       0.68
   Team          Name Efficiency
1:  NOR Anthony Davis       0.67
   Team         Name Efficiency
1:  MEM Tyrus Thomas       0.67
   Team        Name Efficiency
1:  MIA Dwyane Wade       0.67
   Team             Name Efficiency
1:  POR Lamarcu Aldridge       0.66
   Team        Name Efficiency
1:  LAL Kobe Bryant       0.65
   Team          Name Efficiency
1:  BOS Isaiah Thomas       0.64
   Team          Name Efficiency
1:  LAC Blake Griffin       0.62
   Team           Name Efficiency
1:  TOR Louis Williams       0.62
   Team         Name Efficiency
1:  PHO Gerald Green       0.61
   Team         Name Efficiency
1:  MIN Kevin Martin        0.6
   Team        Name Efficiency
1:  BRO Brook Lopez       0.59
   Team         Name Efficiency
1:  CHI Derrick Rose       0.59
   Team             Name Efficiency
1:  DAL Charl Villanueva       0.59
   Team        Name Efficiency
1:  IND Paul George       0.58
   Team        Name Efficiency
1:  PHI Tony Wroten       0.57
   Team           Name Efficiency
1:  UTA Gordon Hayward       0.56
   Team           Name Efficiency
1:  ORL Nikola Vucevic       0.56
   Team          Name Efficiency
1:  CHA Jannero Pargo       0.55
   Team             Name Efficiency
1:  DET Brandon Jennings       0.54
   Team             Name Efficiency
1:  DEN Danilo Gallinari       0.52
   Team          Name Efficiency
1:  SAN Kawhi Leonard       0.52
   Team        Name Efficiency
1:  ATL Jeff Teague       0.52
   Team           Name Efficiency
1:  MIL Ersan Ilyasova       0.51
   Team      Name Efficiency
1:  WAS John Wall       0.49
##各隊三分球出手最準的球員
```{r}
NBA1415DT<-data.table(NBA1415)
output<-NBA1415DT[,list(ThreePerc=round(sum(ThreesMade)/sum(ThreesAttempted),digits = 2)),
                  by=list(Team,Name)] [order(ThreePerc,decreasing = T)]
unique(output$Team)
for(team in unique(output$Team)){
  selectTeam<-subset(output,Team==team)
  print(selectTeam[1])
}
```
