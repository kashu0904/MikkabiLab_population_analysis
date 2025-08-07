# 作業ディレクトリとパッケージの設定
setwd("C:/Users/pirat/Documents/三ケ日/町字別・年齢別人口表")
library(openxlsx)   # Excelファイル読み込み用
library(pyramid)    # 人口ピラミッド描画用

# Excelファイルから「引佐地区」シートのデータを読み込み
data <- read.xlsx("jinkousu_areaage_r06-04-01_hamanaku.xlsx", sheet = "引佐地区")

## ① 全体の名称作成
# シート名 "引佐地区" から「地区」を除去し、語尾に「（全体）」を追加
sheet_name <- "引佐地区"
overall_name <- sub("地区", "", sheet_name)         # "引佐"
overall_title <- paste0(overall_name, "（全体）")    # "引佐（全体）"

## ② 各大字名称の抽出
# このシートは大字が25件存在する。各大字の名称は、行番号 45, 90, ..., 45*25 にある
# うち初めの24件は "(引佐町x)" 形式で、xの部分のみ抽出
# 25番目は "(神宮寺町)" となっているので、括弧を除去して名称を取得
big_area_names <- sapply(1:25, function(i) {
  row_index <- 45 * i
  raw_name <- as.character(data[row_index, 11])
  if (i < 25) {
    # 例："（引佐町宇志）チョウウシ" → "宇志" を抽出
    extracted <- sub(".*町(.*)）.*", "\\1", raw_name)
  } else {
    # 25番目の場合：例："（神宮寺町）" → "神宮寺町" にする
    extracted <- gsub("（|）", "", raw_name)
  }
  return(extracted)
})

# 全体タイトルと大字名称を結合して、タイトルベクトルを作成（全体＋大字＝26件）
titles <- c(overall_title, big_area_names)

## ③ 各バージョンの人口割合データ抽出
# 各バージョンのデータは、全体が i=0、その後大字が i=1～25 として算出
male_data <- list()
female_data <- list()

for (i in 0:25) {
  start <- 2 + 45 * i                      # 各バージョンのデータ開始行
  divisor <- as.numeric(data[start + 41, 10])  # 総人口（分母）の取得
  # 男性データ：7行分から先頭6行を取得し、総人口で割って100倍
  male <- head(as.numeric(unlist(data[start + 6 * (0:6), 0:2*4+3])), -1) / divisor * 100
  # 女性データ：同様の処理
  female <- head(as.numeric(unlist(data[start + 6 * (0:6), 0:2*4+4])), -1) / divisor * 100
  male_data[[i + 1]] <- male
  female_data[[i + 1]] <- female
}

## ④ 年齢階級ラベルの定義
age_groups <- c("0~4", "5~9", "10~14", "15~19", "20~24",
                "25~29", "30~34", "35~39", "40~44", "45~49",
                "50~54", "55~59", "60~64", "65~69", "70~74",
                "75~79", "80~84", "85~89", "90~94", "95~")

## ⑤ ピラミッドの描画
# (1) 全体のピラミッドを単体で描画
pyramids(Left = male_data[[1]], 
         Right = female_data[[1]], 
         Lcol = c(rep("#00A0CD", 3), rep("#71C7D5", 10), rep("#00A0CD", 10)),
         Rcol = c(rep("#EE86A7", 3), rep("#F6BBC6", 10), rep("#EE86A7", 10)), 
         Center = age_groups, 
         Laxis = seq(0, 5, length.out = 6), 
         Clab = "年齢（歳）", 
         Llab = "男", 
         Rlab = "女", 
         Cstep = 1,
         main = titles[1])

# (2) 4行×6列（全体＋大字26件）のレイアウトで描画
par(mfrow = c(6, 5), mar = c(0, 0, 2, 0))

for (i in 0:25) {
  pyramids(Left = male_data[[i + 1]], 
           Right = female_data[[i + 1]], 
           Lcol = c(rep("#00A0CD", 3), rep("#71C7D5", 10), rep("#00A0CD", 10)),
           Rcol = c(rep("#EE86A7", 3), rep("#F6BBC6", 10), rep("#EE86A7", 10)), 
           Center = age_groups, 
           Laxis = seq(0, 5, length.out = 6), 
           Clab = "年齢（歳）", 
           Llab = "男", 
           Rlab = "女", 
           Cstep = 1,
           main = titles[i + 1])
}

# レイアウトをデフォルトに戻す
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
