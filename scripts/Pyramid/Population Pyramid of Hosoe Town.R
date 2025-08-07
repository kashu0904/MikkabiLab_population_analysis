# 作業ディレクトリとパッケージの設定
setwd("C:/Users/pirat/Documents/三ケ日/町字別・年齢別人口表")
library(openxlsx)   # Excelファイル読み込み用
library(pyramid)    # 人口ピラミッド描画用

# Excelファイルから「細江地区」シートのデータを読み込み
data <- read.xlsx("jinkousu_areaage_r06-04-01_hamanaku.xlsx", sheet = "細江地区")

## ① 全体の名称作成
# シート名「細江地区」から「地区」を除去し、「（全体）」を追加
sheet_name <- "細江地区"
overall_name <- sub("地区", "", sheet_name)         # "細江" を取得
overall_title <- paste0(overall_name, "（全体）")    # "細江（全体）"

## ② 各大字名称の抽出（5件）
big_area_names <- sapply(1:5, function(i) {
  row_index <- 45 * i              # 対象行は 45, 90, 135, 180, 225
  raw_name <- as.character(data[row_index, 11])
  # 例: "(細江町x)" の形式から "町" 以降 "）" までの部分（x）を抽出
  extracted <- sub(".*町(.*)）.*", "\\1", raw_name)
  return(extracted)
})

# 全体タイトルと大字名称を結合して、全体＋大字のタイトルベクトル（6件）を作成
titles <- c(overall_title, big_area_names)

## ③ 各バージョンの人口割合データ抽出
# 全体（i=0）＋大字5件（i=1～5）の合計6バージョン
male_data <- list()
female_data <- list()

for (i in 0:5) {
  start <- 2 + 45 * i                      # 各バージョンのデータ開始行
  divisor <- as.numeric(data[start + 41, 10])  # 総人口（分母）の取得
  # 男性データ：対象の7行分から先頭6行を使用し割合に変換
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
# (1) 全体のピラミッドを個別に描画
pyramids(Left = male_data[[1]], 
         Right = female_data[[1]], 
         Lcol = c(rep("#00A0CD", 3), rep("#71C7D5", 10), rep("#00A0CD", 10)),
         Rcol = c(rep("#EE86A7", 3), rep("#F6BBC6", 10), rep("#EE86A7", 10)),
         Center = age_groups,
         Laxis = seq(0, 5, length.out = 6),
         Clab = "年齢（歳）", Llab = "男", Rlab = "女", Cstep = 1,
         main = titles[1])

# (2) 2行×3列のレイアウトで、全体＋大字（計6件）のピラミッドを描画
par(mfrow = c(2, 5), mar = c(0, 0, 2, 0))
for (i in 0:5) {
  pyramids(Left = male_data[[i + 1]], 
           Right = female_data[[i + 1]], 
           Lcol = c(rep("#00A0CD", 3), rep("#71C7D5", 10), rep("#00A0CD", 10)),
           Rcol = c(rep("#EE86A7", 3), rep("#F6BBC6", 10), rep("#EE86A7", 10)),
           Center = age_groups,
           Laxis = seq(0, 5, length.out = 6),
           Clab = "年齢（歳）", Llab = "男", Rlab = "女", Cstep = 1,
           main = titles[i + 1])
}

# レイアウトをデフォルトに戻す
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
