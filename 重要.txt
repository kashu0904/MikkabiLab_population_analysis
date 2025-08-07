#----------------------------------------
# 1. ライブラリ
#----------------------------------------
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(openxlsx)
library(stringr)
#----------------------------------------
# 2. 定数・パラメータ設定
#----------------------------------------

# ★★★地-----------地域指定-------------
area_name <- "mikkabi"  
# hosoe, inasa などに変えるだけ
# ★★★----------------------------------

file_name <- paste0(area_name, "_population_combined.xlsx")
base_dir   <- "C:/Users/pirat/Documents/mikkabi_population_analysis/data/processed"
file_path <- file.path(base_dir, file_name)
# ① シート名を取得し、「YYYY-04」のものだけを年だけに変換
years <- excel_sheets(file_path) %>%
  grep("^[0-9]{4}-04$", ., value = TRUE) %>%  # YYYY-04 形式のみ抽出
  sub("-04$", "", .) %>%                     # 「-04」を外す
  as.integer() %>%                            # 整数化
  sort()                                      # ソート
# ② 読み込むシート名ベクトル
sheets     <- paste0(years, "-04")
#----------------------------------------
# 3. 生データ読み込み＆ヘッダー整合性チェック
#----------------------------------------
# ── 1) Excel ブックを一度だけ開く ─────────────────────
wb <- loadWorkbook(file_path)
# ── 2) “生データ” としてヘッダー行も含めて全シート読み込み ───
rawdata <- lapply(sheets, function(sh) {
  readWorkbook(wb, sheet = sh, colNames = FALSE)})
# ── 3) 各シートの11列目ヘッダー行（1+45*(0:19) 行目）を抽出 ───
headers_list <- lapply(rawdata, function(dat) {
  hdr <- as.character(dat[1 + 45*(0:19), 11])
  str_sub(hdr, 2, -2)})
# ── 4) 先頭シートを基準に、他シートのヘッダーと比較 ─────────
base_header <- headers_list[[1]]
mismatch    <- lapply(headers_list[-1], function(hdr) which(hdr != base_header))
bad_sheets  <- which(sapply(mismatch, length) > 0) + 1  # インデックス補正

# ── 5) 不一致があれば差分を表示して確認 ────────────────────
if (length(bad_sheets) > 0) {
  msg <- sapply(bad_sheets, function(i) {
    diffs <- mismatch[[i - 1]]
    paste0(
      sheets[i], " シートでヘッダー不一致：行 ",
      paste(diffs, collapse = ", "),
      " （基準: ", paste(base_header[diffs], collapse = ", "),
      " → 実際: ", paste(headers_list[[i]][diffs], collapse = ", "), "）")})
  cat(
    "ヘッダー整合性チェックに失敗しました。\n",
    paste(msg, collapse = "\n"), "\n\n",
    sep = "" )
  # askYesNo で対話的に確認
  ans <- utils::askYesNo("ヘッダー不一致を検知しました。処理を続行しますか？")
  # FALSE（No）なら中断、TRUE/NA（Yes or Cancel）は続行
  if (identical(ans, FALSE)) {
    stop("処理を中断しました。")}
  message("ユーザー判断により処理を続行します。")}
#地区名
NAMES_area <- headers_list[[length(headers_list)]]
#----------------------------------------
# 4. 通常データ読み込み（ヘッダー行を列名として）
#----------------------------------------
sdata <- lapply(sheets, function(sh) {
  read_excel(path = file_path, sheet = sh, col_names = TRUE)})
# 以下、ext() を使ってデータ抽出へ進みます
ext <- function(dat, rows, col) {
  as.numeric(dat[rows, col][[1]])}
p0004  <- unlist(lapply(sdata, ext, rows = 2+45*(0:19),   col = 2))
p0014  <- unlist(lapply(sdata, ext, rows = 44+45*(0:19),  col = 2))
p1564  <- unlist(lapply(sdata, ext, rows = 44+45*(0:19),  col = 6))
p6500  <- unlist(lapply(sdata, ext, rows = 44+45*(0:19),  col = 10))
pTotal <- unlist(lapply(sdata, ext, rows = 43+45*(0:19),  col = 10))
pTotal_calc <- p0014 + p1564 + p6500
# 不一致チェック＆停止
mismatch_idx <- which(pTotal != pTotal_calc)
if (length(mismatch_idx) > 0) {
  years_bad <- years[mismatch_idx]
  diffs     <- pTotal_calc[mismatch_idx] - pTotal[mismatch_idx]
  # 年・差分をメッセージにまとめてエラーで停止
  stop(
    sprintf(
      "総人口の整合性チェックに失敗しました。以下の年で差分があります：\n%s",
      paste(
        sprintf("%d年: 差分 %d", years_bad, diffs),
        collapse = "\n")))}
message("総人口の整合性チェック：OK")
#----------------------------------------
# 4. マトリクス化
#----------------------------------------
mat_0004 <- matrix(p0004,  nrow = length(years), byrow = TRUE,
                   dimnames = list(year = as.character(years), area = NAMES_area))
mat_0014 <- matrix(p0014,  nrow = length(years), byrow = TRUE,
                   dimnames = list(year = as.character(years), area = NAMES_area))
mat_1564 <- matrix(p1564,  nrow = length(years), byrow = TRUE,
                   dimnames = list(year = as.character(years), area = NAMES_area))
mat_6500 <- matrix(p6500,  nrow = length(years), byrow = TRUE,
                   dimnames = list(year = as.character(years), area = NAMES_area))
mat_TOT  <- matrix(pTotal, nrow = length(years), byrow = TRUE,
                   dimnames = list(year = as.character(years), area = NAMES_area))
mat_ageing <- mat_6500 / mat_TOT * 100
#----------------------------------------
# 5. Y軸設定：手動カスタムだけ custom_scales にまとめる
#----------------------------------------
custom_scales <- setNames(
  vector("list", length(NAMES_area)),
  NAMES_area
)

custom_scales[["三ヶ日地区"]]$total <- list(
  breaks = seq(0, 16000, length = 9),limits = c(0, 16000))

custom_scales[["三ヶ日町宇志"]]$age0004 <- list(
  breaks = seq(0, 400, length = 9),limits = c(0, 400))

custom_scales[["三ヶ日町鵺代"]]$total <- list(
  breaks = seq(0, 800, length = 9),limits = c(0, 800))

custom_scales[["三ヶ日町下尾奈"]]$total <- list(
  breaks = seq(0, 1100, length = 12),limits = c(0,1100))

#----------------------------------------
# auto scale 用ヘルパー
#----------------------------------------
# auto_scale: データ最大値にパディングをかけ、いい感じの目盛＆上限を返す
# x   : 数値ベクトル（例 df$age0004）
# n   : 目盛の本数目安（大きいほど細かい）
#   pad_brk  - 目盛生成用にかける倍率（例1.05 → データ最大の105%でpretty）
#   pad_lim  - limits用にかける倍率（例0.95 → データ最大の95%を上限に）
auto_scale <- function(x, n = 5, pad_brk = 1.05, pad_lim = 1.05) {
  mx      <- max(x, na.rm = TRUE)
  brk_max <- mx * pad_brk
  brks    <- pretty(c(0, brk_max), n = n)
  lim_max <- mx * pad_lim
  list(breaks = brks,limits = c(0, lim_max))}
#----------------------------------------
# Arate（％）専用 auto scale
#----------------------------------------
auto_rate_scale <- function(x, step = 7, pad = 1.10) {
  # step: ％刻み幅
  # pad : 上限マージン倍率（例1.02）
  mx  <- max(x, na.rm = TRUE)
  top <- ceiling(mx / step) * step * pad
  brks <- seq(0, top, by = step)
  list(breaks = brks, limits = c(15, top))}
#----------------------------------------
# 6. プロット関数定義（サンプルのテーマを共通化）
#----------------------------------------
start_year <- min(years)
end_year   <- max(years)
base_area  <- NAMES_area[1]
plot_for_area <- function(area_name){
  df <- tibble(
    year    = factor(years, levels = years),
    age0004 = mat_0004[, area_name],
    age0014 = mat_0014[, area_name],
    age1564 = mat_1564[, area_name],
    age65up = mat_6500[, area_name]
  ) %>% mutate(
    total = age0004 + age0014 + age1564 + age65up,
    Arate = age65up / total * 100)
  ##----------------------------------------
  ## 共通設定
  ##----------------------------------------
  # ❶グラフデザイン
  width_std     <- 0.70 #バーの太さ
  linewidth_std <- 0.35 #バーの枠線の太さ
  expand_x_std  <- expansion(mult = c(0.04, 0.04)) 
  # X軸（年）方向の余白の割合。左右にどれだけスペースをあけるかを調整。
  expand_y_std  <- expansion(mult = c(0.04, 0.04)) 
  # Y軸（人数や率）方向の余白の割合。上下にどれだけスペースをあけるかを調整。
  theme_std     <- theme(
    panel.background = element_rect(fill = "#CCCCCC"),
    plot.margin      = unit(rep(1,4),"cm"), #
    plot.title       = element_text(size=20, hjust=0.5), #グラフタイトルの大きさ
    axis.title       = element_blank(),
    axis.text.x      = element_text(angle=50, # X軸ラベル（年など）の角度。50度傾けている。0で横書き。
                                    hjust=1, # 水平方向のラベル位置の調整。1で右寄せ、0で左寄せ、0.5で中央。
                                    size=20, # X軸ラベルの文字サイズ。
                                    margin=margin(t=0.1,unit="cm")), # X軸ラベルと軸との間の余白（上方向）。
    axis.text.y      = element_text(size=15,
                                    margin=margin(r=0.2,unit="cm"))) # Y軸ラベルと軸との間の余白（右方向）。
  # ❷数値指標用 auto_scale
  auto_scales <- list(
    age0004 = auto_scale(df$age0004, n = 9), # 0~4歳
    age0014 = auto_scale(df$age0014, n = 9), # 0~14歳（年少者）
    age1564 = auto_scale(df$age1564, n = 9), # 15~64歳（労働生産年齢）
    age65up = auto_scale(df$age65up, n = 9), # 65~歳（高齢者）
    total   = auto_scale(df$total,   n = 9)) # 総人口
                                     #`n=9`は目盛りの数（概ね9本程度）を指定。
  # ❸高齢化率（Arate） 用 auto_rate_scale
  rate_auto <- auto_rate_scale(df$Arate, step = 5, pad = 1.02) #
  # カスタム設定取得
  cs <- custom_scales[[area_name]]
  if (is.null(cs)) cs <- list()
  # auto と custom をマージ
  y_scales <- modifyList(auto_scales, cs)
  # Arate は別途 override
  if (!is.null(cs$Arate)) {y_scales$Arate <- cs$Arate
  } else {y_scales$Arate <- rate_auto}
  
  # 1) 0〜4歳
  p1 <- ggplot(df, aes(x = year, y = age0004)) +
    geom_bar(stat="identity", colour = 1, linewidth = linewidth_std, fill = "#90569C", width = width_std) +
    labs(title = paste0(area_name, "：0〜4歳の人口の推移 ",start_year,"〜",end_year,"（",base_area,"）"), y = "人口（人）") +
    scale_x_discrete(expand = expand_x_std)+      
    scale_y_continuous(breaks = y_scales$age0004$breaks, limits = y_scales$age0004$limits,labels = comma,expand = expand_y_std) +
    theme_std
  # 2) 0〜14歳
  p2 <- ggplot(df, aes(x = year, y = age0014)) +
    geom_bar(stat="identity", colour = 1, linewidth = linewidth_std, fill = "#6BC7F1", width = width_std) +
    labs(title = paste0(area_name, "：0〜14歳の人口の推移 ",start_year,"〜",end_year,"（",base_area,"）"), y = "人口（人）") +
    scale_x_discrete(expand = expand_x_std)+
    scale_y_continuous(breaks = y_scales$age0014$breaks, limits = y_scales$age0014$limits,labels = comma,expand = expand_y_std) +
    theme_std
  # 3) 15〜64歳
  p3 <- ggplot(df, aes(x = year, y = age1564)) +
    geom_bar(stat="identity", colour = 1, linewidth = linewidth_std, fill = "#00AD7A", width = width_std) +
    labs(title = paste0(area_name, "：15〜64歳の人口の推移 ",start_year,"〜",end_year,"（",base_area,"）"), y = "人口（人）") +
    scale_x_discrete(expand = expand_x_std)+
    scale_y_continuous(breaks = y_scales$age1564$breaks, limits = y_scales$age1564$limits,labels = comma,expand = expand_y_std) +
    theme_std
  # 4) 65歳以上
  p4 <- ggplot(df, aes(x = year, y = age65up)) +
    geom_bar(stat="identity", colour = 1, linewidth = linewidth_std, fill = "#006EBB", width = width_std) +
    labs(title = paste0(area_name, "：65歳以上の人口の推移 ",start_year,"〜",end_year,"（",base_area,"）"), y = "人口（人）") +
    scale_x_discrete(expand = expand_x_std)+
    scale_y_continuous(breaks = y_scales$age65up$breaks, limits = y_scales$age65up$limits,labels = comma,expand = expand_y_std) +
    theme_std
  # 5) 総人口（積み上げ）
  df_long <- df %>%
    select(year, age0014, age1564, age65up) %>%
    pivot_longer(-year, names_to = "group", values_to = "value") %>%
    mutate(group = factor(group,
                          levels = c("age0014","age1564","age65up"),
                          labels = c("0〜14歳","15〜64歳","65歳以上")))
  p5 <- ggplot(df_long, aes(x = year, y = value, fill = group)) +
    geom_bar(stat="identity", colour = 1, linewidth = linewidth_std, width = width_std) +
    labs(title = paste0(area_name, "：総人口の推移 ",start_year,"〜",end_year,"（",base_area,"）"), y = "人口（人）") +
    scale_x_discrete(expand = expand_x_std)+
    scale_y_continuous(breaks = y_scales$total$breaks, limits = y_scales$total$limits,labels = comma,expand = expand_y_std) +
    scale_fill_manual(values = c("#6BC7F1", "#00AD7A", "#006EBB")) + 
    theme(legend.position = "none") +theme_std
  # 6) 高齢化率
  p6 <- ggplot(df, aes(x = year, y = Arate, group = 1)) +
    geom_line(linewidth = 1.5, color = "#BB0000") +
    # 高齢化率推移線の太さ
    geom_point(size = 3, color = "#BB0000") +
    # 高齢化率ポイントの大きさ
    labs(title = paste0(area_name, "：高齢化率の推移 ",start_year,"〜",end_year,"（",base_area,"）"), y = "高齢化率（%）") +
    scale_x_discrete(expand = expand_x_std)+
    scale_y_continuous(breaks = y_scales$Arate$breaks, limits = y_scales$Arate$limits,expand = expand_y_std) +
    theme_std + theme(axis.text.x = element_text(size = 20, colour = 1))
  
  list(p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5, p6 = p6)}
#----------------------------------------
# 7. 全地区一括作図例
#----------------------------------------
all_plots <- lapply(NAMES_area, plot_for_area)
names(all_plots) <- NAMES_area
#----------------------------------------
# 三ヶ日地区
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日地区"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日地区"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日地区"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日地区"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日地区"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日地区"]]$p6)

#----------------------------------------
# 宇志
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町宇志"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町宇志"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町宇志"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町宇志"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町宇志"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町宇志"]]$p6)

#----------------------------------------
# 大崎
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町大崎"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町大崎"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町大崎"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町大崎"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町大崎"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町大崎"]]$p6)

#----------------------------------------
# 大谷
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町大谷"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町大谷"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町大谷"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町大谷"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町大谷"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町大谷"]]$p6)

#----------------------------------------
# 岡本
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町岡本"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町岡本"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町岡本"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町岡本"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町岡本"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町岡本"]]$p6)

#----------------------------------------
# 上尾奈
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町上尾奈"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町上尾奈"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町上尾奈"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町上尾奈"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町上尾奈"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町上尾奈"]]$p6)

#----------------------------------------
# 駒場
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町駒場"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町駒場"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町駒場"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町駒場"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町駒場"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町駒場"]]$p6)

#----------------------------------------
# 佐久米
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町佐久米"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町佐久米"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町佐久米"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町佐久米"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町佐久米"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町佐久米"]]$p6)

#----------------------------------------
# 下尾奈
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町下尾奈"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町下尾奈"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町下尾奈"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町下尾奈"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町下尾奈"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町下尾奈"]]$p6)

#----------------------------------------
# 只木
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町只木"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町只木"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町只木"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町只木"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町只木"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町只木"]]$p6)

#----------------------------------------
# 都筑
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町都筑"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町都筑"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町都筑"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町都筑"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町都筑"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町都筑"]]$p6)

#----------------------------------------
# 津々崎
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町津々崎"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町津々崎"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町津々崎"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町津々崎"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町津々崎"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町津々崎"]]$p6)

#----------------------------------------
# 釣
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町釣"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町釣"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町釣"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町釣"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町釣"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町釣"]]$p6)

#----------------------------------------
# 鵺代
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町鵺代"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町鵺代"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町鵺代"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町鵺代"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町鵺代"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町鵺代"]]$p6)

#----------------------------------------
# 日比沢
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町日比沢"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町日比沢"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町日比沢"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町日比沢"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町日比沢"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町日比沢"]]$p6)

#----------------------------------------
# 平山
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町平山"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町平山"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町平山"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町平山"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町平山"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町平山"]]$p6)

#----------------------------------------
# 福長
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町福長"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町福長"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町福長"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町福長"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町福長"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町福長"]]$p6)

#----------------------------------------
# 本坂
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町本坂"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町本坂"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町本坂"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町本坂"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町本坂"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町本坂"]]$p6)

#----------------------------------------
# 摩訶耶
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町摩訶耶"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町摩訶耶"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町摩訶耶"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町摩訶耶"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町摩訶耶"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町摩訶耶"]]$p6)

#----------------------------------------
# 三ヶ日
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[["三ヶ日町三ヶ日"]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[["三ヶ日町三ヶ日"]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[["三ヶ日町三ヶ日"]]$p3)
#「65歳以上」グラフを表示
print(all_plots[["三ヶ日町三ヶ日"]]$p4)
#「総人口」グラフを表示
print(all_plots[["三ヶ日町三ヶ日"]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[["三ヶ日町三ヶ日"]]$p6)
