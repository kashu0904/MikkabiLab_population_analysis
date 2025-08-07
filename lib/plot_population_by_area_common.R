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

file_name <- paste0(area_name, "_population_combined.xlsx")
base_dir   <- "C:/Users/pirat/Documents/mikkabi_population_analysis/data/processed"
file_path <- file.path(base_dir, file_name)
# ① シート名を取得し、「YYYY-04」のものだけを年だけに変換
years <- excel_sheets(file_path) %>%
  grep("^[0-9]{4}-04$", ., value = TRUE) %>%  # YYYY-04 形式のみ抽出
  sub("-04$", "", .) %>%                      #「-04」を外す
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
  readWorkbook(wb, sheet = sh, colNames = FALSE)
})

# ── 3) 最新のシートから動的に地区数＆地区名を取得 ────────────────
last_data <- rawdata[[length(rawdata)]]

# 45行ごとにある地区ヘッダー（列11）を抽出
header_positions <- seq(1, nrow(last_data), by = 45)
header_candidates <- as.character(last_data[header_positions, 11])
header_cleaned <- str_sub(header_candidates, 2, -2)

# 実在する地区名だけに絞る（空白・NA除外）
NAMES_area <- header_cleaned[!is.na(header_cleaned) & header_cleaned != ""]
n_areas    <- length(NAMES_area)

# 全シート共通のヘッダー抽出行位置を再定義
positions <- 1 + 45 * seq(0, n_areas - 1)

# ── 4) 各シートのヘッダー行（列11）を抽出 ─────────
headers_list <- lapply(rawdata, function(dat) {
  str_sub(as.character(dat[positions, 11]), 2, -2)
})

# ── 5) 最新シートを基準に、他シートのヘッダーと比較 ─────────
base_header <- headers_list[[length(headers_list)]]
mismatch    <- lapply(headers_list, function(hdr) which(hdr != base_header))
bad_sheets  <- which(sapply(mismatch, length) > 0)

# ── 6) 不一致があれば差分を表示して確認 ────────────────────
if (length(bad_sheets) > 0) {
  msg <- sapply(bad_sheets, function(i) {
    diffs <- mismatch[[i]]
    paste0(
      sheets[i], " シートでヘッダー不一致：行 ",
      paste(diffs, collapse = ", "),
      " （基準: ", paste(base_header[diffs], collapse = ", "),
      " → 実際: ", paste(headers_list[[i]][diffs], collapse = ", "), "）"
    )
  })
  cat(
    "ヘッダー整合性チェックに失敗しました。\n",
    paste(msg, collapse = "\n"), "\n\n",
    sep = ""
  )
  ans <- utils::askYesNo("ヘッダー不一致を検知しました。処理を続行しますか？")
  if (identical(ans, FALSE)) {
    stop("処理を中断しました。")
  }
  message("ユーザー判断により処理を続行します。")
}

#----------------------------------------
# 4. 通常データ読み込み（ヘッダー行を列名として）
#----------------------------------------
index_vec <- 0:(n_areas - 1)
sdata <- lapply(sheets, function(sh) {
  read_excel(path = file_path, sheet = sh, col_names = TRUE)})
# 以下、ext() を使ってデータ抽出へ進みます
ext <- function(dat, rows, col) {
  as.numeric(dat[rows, col][[1]])}
p0004  <- unlist(lapply(sdata, ext, rows = 2  + 45 * index_vec, col = 2))
p0014  <- unlist(lapply(sdata, ext, rows = 44 + 45 * index_vec, col = 2))
p1564  <- unlist(lapply(sdata, ext, rows = 44 + 45 * index_vec, col = 6))
p6500  <- unlist(lapply(sdata, ext, rows = 44 + 45 * index_vec, col = 10))
pTotal <- unlist(lapply(sdata, ext, rows = 43 + 45 * index_vec, col = 10))
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
# 最新の高齢化率
#----------------------------------------
# ❶グラフデザイン
width_std     <- 0.70 #バーの太さ
linewidth_std <- 0.35 #バーの枠線の太さ
expand_x_std  <- expansion(mult = c(0.04, 0.04)) 
# X軸（年）方向の余白の割合。左右にどれだけスペースをあけるかを調整。
expand_y_std  <- expansion(mult = c(0.04, 0.04)) 
# Y軸（人数や率）方向の余白の割合。上下にどれだけスペースをあけるかを調整。
theme_std     <- theme(
  panel.background = element_rect(fill = "#CCCCCC"),
  plot.title       = element_text(size=20, hjust=0.5), #グラフタイトルの大きさ
  axis.title       = element_blank(),
  axis.text.x      = element_text(angle=50, # X軸ラベル（年など）の角度。50度傾けている。0で横書き。
                                  hjust=1, # 水平方向のラベル位置の調整。1で右寄せ、0で左寄せ、0.5で中央。
                                  size= 20 , # X軸ラベルの文字サイズ。
                                  margin=margin(t=0.1,unit="cm"),
                                  color = "#000000"), # X軸ラベルと軸との間の余白（上方向）。
  axis.text.y      = element_text(size=15,
                                  margin=margin(r=0.2,unit="cm"), # Y軸ラベルと軸との間の余白（右方向）。
                                  color = "#000000"),
  plot.margin = margin(t = 2,  # 上余白 1cm
                       r = 2,  # 右余白 2cm
                       b = 2,  # 下余白 1cm
                       l = 2,  # 左余白 2cm
                       unit = "cm"))

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

  # ❷数値指標用 auto_scale
  auto_scales <- list(
    age0004 = auto_scale(df$age0004, n = 9), # 0~4歳
    age0014 = auto_scale(df$age0014, n = 9), # 0~14歳（年少者）
    age1564 = auto_scale(df$age1564, n = 9), # 15~64歳（労働生産年齢）
    age65up = auto_scale(df$age65up, n = 9), # 65~歳（高齢者）
    total   = auto_scale(df$total,   n = 9)) # 総人口
  #`n=9`は目盛りの数（概ね9本程度）を指定。
  # ❸高齢化率（Arate） 用 auto_rate_scale
  rate_auto <- auto_rate_scale(df$Arate)
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
    theme_std

  
  list(p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5, p6 = p6)}




# ③ 年の数
analysis_period <- length(years)
latest_Arate = mat_ageing[analysis_period, ]

df <- data.frame(
  area = names(latest_Arate),
  Arate = as.numeric(latest_Arate)
)
df$fill_color <- ifelse(df$Arate > 50, "#AD000E", "#0B318F")
# 先頭の地区名を取得（=「三ヶ日地区」の代わり）
special_area <- names(latest_Arate)[1]

# その地区以外をメインにして降順ソート
df_main <- df[df$area != special_area, ]
df_main <- df_main[order(-df_main$Arate), ]

# 特別扱いする先頭の地区を取り出す
df_special <- df[df$area == special_area, ]

# 並び順を再構成
df_ordered <- rbind(df_main, df_special)
area_levels <- df_ordered$area

# 並び順を factor に適用
df$area <- factor(df$area, levels = area_levels)


p7 <- ggplot(df, aes(x = area, y = Arate, fill = fill_color)) + 
  geom_bar(stat = "identity", colour = 1, linewidth = linewidth_std, width = width_std_latest) +
  labs(title = paste0("各町字の高齢化率 ", end_year, "（", base_area, "）"),
       y = "高齢化率（%）") +
  scale_x_discrete(expand = expand_x_std_latest) +
  scale_fill_identity() +
  scale_y_continuous(
    breaks = latest_arate_breaks,
    limits = latest_arate_limits,
    labels = comma,
    expand = expand_y_std_latest
  ) +
  theme_std +
  theme(axis.text.x = element_text(size = latest_area_label_size))


