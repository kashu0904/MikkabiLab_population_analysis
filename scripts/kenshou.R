setwd("C:/Users/pirat/Documents/mikkabi_population_analysis/data/raw/Population_By_Town_and_Age/kitaku and hamanaku")#ディレクトリ指定
library(gdata)　#xlsxを読み込めるようパッケージを読み込む。
library(readxl)#xlsを読み込めるようパッケージを読み込む。read_excelを使えるようにする。
library(tidyr)
library(ggplot2)

data<-read_excel("jinkousu_areaage_r06-04-01_hamanaku.xlsx",sheet="三ヶ日地区") #2024


total_by_age<-c(as.numeric(unlist(data[3:44,0:1*4+2]))[-6*1:14],
     as.numeric(unlist(data[3:42,10]))[-6*1:5]# 各年齢の人口
    )
sum(total_by_age)# 各年齢の人口を合算した全体の総人口を算出


male_by_age<-c(as.numeric(unlist(data[3:44,0:1*4+3]))[-6*1:14],
     as.numeric(unlist(data[3:42,11]))[-6*1:5]
)# 各年齢における男性人口の合計を算出（男性の総人口）

female_by_age<-c(as.numeric(unlist(data[3:44,0:1*4+4]))[-6*1:14],
     as.numeric(unlist(data[3:42,12]))[-6*1:5]
)# 各年齢における女性人口の合計を算出（男性の総人口）

sum(male_by_age,female_by_age)# 男性と女性の各年齢人口の合計（男女合計の総人口）


identical(male_by_age+female_by_age,total_by_age)


as.numeric(unlist(data[43+45*0:19,10]))#総人口（セルから直接）


identical(sum(male_by_age,female_by_age),as.numeric(unlist(data[43,10])))



as.numeric(unlist(data[43,10]))#総人口
as.numeric(unlist(data[43,11]))#総人口（男性）
as.numeric(unlist(data[43,12]))#総人口（女性）

as.numeric(unlist(data[2,2]))#0~4歳人口
as.numeric(unlist(data[2,3]))#0~4歳人口（男性）
as.numeric(unlist(data[2,4]))#0~4歳人口（女性）

as.numeric(unlist(data[8,2]))#5~9歳人口
as.numeric(unlist(data[8,3]))#5~9歳人口（男性）
as.numeric(unlist(data[8,4]))#5~9歳人口（女性）

as.numeric(unlist(data[14,2]))#10~14歳人口
as.numeric(unlist(data[14,3]))#10~14歳人口（男性）
as.numeric(unlist(data[14,4]))#10~14歳人口（女性）

as.numeric(unlist(data[20,2]))#15~19歳人口
as.numeric(unlist(data[20,3]))#15~19歳人口（男性）
as.numeric(unlist(data[20,4]))#15~19歳人口（女性）

as.numeric(unlist(data[26,2]))#20~24歳人口
as.numeric(unlist(data[26,3]))#20~24歳人口（男性）
as.numeric(unlist(data[26,4]))#20~24歳人口（女性）

as.numeric(unlist(data[32,2]))#25~29歳人口
as.numeric(unlist(data[32,3]))#25~29歳人口（男性）
as.numeric(unlist(data[32,4]))#25~29歳人口（女性）

as.numeric(unlist(data[38,2]))#30~34歳人口
as.numeric(unlist(data[38,3]))#30~34歳人口（男性）
as.numeric(unlist(data[38,4]))#30~34歳人口（女性）

as.numeric(unlist(data[2,6]))#35~49歳人口
as.numeric(unlist(data[2,7]))#35~49歳人口（男性）
as.numeric(unlist(data[2,8]))#35~49歳人口（女性）

as.numeric(unlist(data[8,6]))#50~54歳人口
as.numeric(unlist(data[8,7]))#50~54歳人口（男性）
as.numeric(unlist(data[8,8]))#50~54歳人口（女性）

as.numeric(unlist(data[14,6]))#55~59歳人口
as.numeric(unlist(data[14,7]))#55~59歳人口（男性）
as.numeric(unlist(data[14,8]))#55~59歳人口（女性）

as.numeric(unlist(data[20,6]))#60~64歳人口
as.numeric(unlist(data[20,7]))#60~64歳人口（男性）
as.numeric(unlist(data[20,8]))#60~64歳人口（女性）

as.numeric(unlist(data[26,6]))#65~69歳人口
as.numeric(unlist(data[26,7]))#65~69歳人口（男性）
as.numeric(unlist(data[26,8]))#65~69歳人口（女性）

as.numeric(unlist(data[32,6]))#70~74歳人口
as.numeric(unlist(data[32,7]))#70~74歳人口（男性）
as.numeric(unlist(data[32,8]))#70~74歳人口（女性）

as.numeric(unlist(data[38,6]))#75~79歳人口
as.numeric(unlist(data[38,7]))#75~79歳人口（男性）
as.numeric(unlist(data[38,8]))#75~79歳人口（女性）

as.numeric(unlist(data[2,10]))#80~84歳人口
as.numeric(unlist(data[2,11]))#80~84歳人口（男性）
as.numeric(unlist(data[2,12]))#80~84歳人口（女性）

as.numeric(unlist(data[8,10]))#85~89歳人口
as.numeric(unlist(data[8,11]))#85~89歳人口（男性）
as.numeric(unlist(data[8,12]))#85~89歳人口（女性）


as.numeric(unlist(data[14,10]))#90~94歳人口
as.numeric(unlist(data[14,11]))#90~94歳人口（男性）
as.numeric(unlist(data[14,12]))#90~94歳人口（女性）


as.numeric(unlist(data[20,10]))#95~歳人口
as.numeric(unlist(data[20,11]))#95~歳人口（男性）
as.numeric(unlist(data[20,12]))#95~歳人口（女性）













