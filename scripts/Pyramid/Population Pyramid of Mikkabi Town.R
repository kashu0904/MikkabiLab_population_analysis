# 作業ディレクトリとパッケージの設定
setwd("C:/Users/pirat/Documents/三ケ日/町字別・年齢別人口表")
library(openxlsx)   # Excelファイル読み込み用
library(pyramid)    # 人口ピラミッド描画用

# Excelファイルからデータを読み込み
data <- read.xlsx("jinkousu_areaage_r06-04-01_hamanaku.xlsx", sheet = "三ヶ日地区")

## ① 全体の名称をシート名から作成
# シート名（例："三ヶ日地区"）から「町」または「地区」を除去して全体名称を作成
sheet_name <- "三ヶ日地区"
overall_name <- sub("町|地区", "", sheet_name)  # "三ヶ日地区" -> "三ヶ日"
overall_title <- paste0(overall_name, "（全体）")  # "三ヶ日（全体）"

## ② 各大字の名称をExcelデータから抽出
# 各大字名は行45ごとに配置されているので、行番号は45, 90, 135, ... (1から19まで)
big_area_names <- sapply(1:19, function(i) {
  row_index <- 45 * i  # 対象行
  raw_name <- as.character(data[row_index, 11])
  # 例: "(三ヶ日町宇志）チョウウシ" の形式から「町」以降「）」までの部分を抽出
  extracted <- sub(".*町(.*)）.*", "\\1", raw_name)
  return(extracted)
})

# 全体タイトルと各大字名を結合して、全体＋各大字のタイトルベクトルを作成
titles <- c(overall_title, big_area_names)

## ③ 各バージョンの人口割合データの抽出
# 各バージョン（全体＋各大字計20）の男性・女性の人口データを格納するリストを初期化
male_data <- list()
female_data <- list()

# 各バージョンの開始行は「2 + 45 * i」として算出（i=0:全体、i=1～19:各大字）
for (i in 0:19) {
  start <- 2 + 45 * i                      # データ開始行
  divisor <- as.numeric(data[start + 41, 10])  # 総人口（分母）の取得
  
  # 男性データ: 指定範囲の7行分のデータから、最後の1行を除いて割合に変換
  male <- head(as.numeric(unlist(data[start + 6 * (0:6), 0:2*4+3])), -1) / divisor * 100
  # 女性データ: 同様の処理
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

# (2) 4行×5列のレイアウト設定（全体＋各大字の合計20グラフ）
par(mfrow = c(4, 5), mar = c(0, 0, 2, 0))

# 各バージョン（i=0:全体、i=1～19:各大字）のピラミッドを描画
for (i in 0:19) {
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
