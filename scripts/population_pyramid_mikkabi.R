
setwd("C:\\Users\\pirat\\Documents\\mikkabi_population_analysis\\data\\raw\\Population_By_Town_and_Age")


library(openxlsx)　#xlsxを読み込めるようパッケージを読み込む。
library(pyramid)　#ピラミッドを出せるようパッケージを読み込む。
#令和６年三ヶ日地区読み込み
R0604_Kitakusheet<-read.xlsx("jinkousu_areaage_r06-04-01_hamanaku.xlsx",sheet="三ヶ日地区")



#令和１年北区数値指定。人口を取り出す。行は-1で列は問題ない。


#男性5歳階級別データ
R060401_jinkousu_Mikkabiarea_M_class5<-as.numeric(c(
  
  R0604_Kitakusheet[2+6*0:6,3],
  R0604_Kitakusheet[2+6*0:6,7],
  R0604_Kitakusheet[2+6*0:5,11]))

#女性5歳階級別データ
R060401_jinkousu_Mikkabiarea_W_class5<-as.numeric(c(
  
  R0604_Kitakusheet[2+6*0:6,4],
  R0604_Kitakusheet[2+6*0:6,8],
  R0604_Kitakusheet[2+6*0:5,12]))

R060401_jinkousu_Mikkabiarea_M_class5
R060401_jinkousu_Mikkabiarea_W_class5


p<-rbind(
  "0~4","5~9","10~14","15~19","20~24","25~29","30~34",
  "35~39","40~44","45~49","50~54","55~59","60~64",
  "65~69","70~74","75~79","80~84","85~89","90~94","95~")

pyramids(Left=R060401_jinkousu_Mikkabiarea_M_class5, 
         Right=R060401_jinkousu_Mikkabiarea_W_class5, 
         Lcol=c(rep("#00A0CD",3),rep("#71C7D5",10),rep("#00A0CD",10)),
         Rcol=c(rep("#EE86A7",3),rep("#F6BBC6",10),rep("#EE86A7",10)), 
         Center=p[,1], Laxis=seq(0,600,len=5),
         Clab="年齢（歳）", Llab="男", Rlab="女", Cstep=1,
         main="三ケ日の人口ピラミッド 2024 " )


