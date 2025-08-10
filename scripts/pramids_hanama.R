# ============================================================
# build_population_pyramids.R
# 地区別人口ピラミッドPDFをバッチ生成するRスクリプト
# ・Excelは最初に一度だけ読み込む（I/O削減）
# ・地区名は最新シートから動的取得
# ・男女合計100%チェック＆ヘッダー整合性チェック（不一致は確認して続行可）
# ・PDFは日本語フォントを埋め込み（showtext + cairo_pdf, family="jp"）
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(stringr)
  library(here)
  library(pyramid)
  library(showtext)
  library(systemfonts)# ← フォント検出用
  library(sysfonts) 
})

# ---- 日本語フォントを "jp" エイリアスで登録（環境依存を吸収） ----
register_jp_font <- function(prefer = c("Noto Sans JP","Noto Sans CJK JP",
                                        "Yu Gothic","Meiryo","Hiragino Sans",
                                        "Hiragino Kaku Gothic ProN")) {
  showtext::showtext_auto(TRUE)
  db <- systemfonts::system_fonts()
  
  # family名で優先リストからパスを探す（Regular=400, normal）
  ix <- which(tolower(db$family) %in% tolower(prefer) &
                db$weight == 400 & db$style == "normal")
  path <- if (length(ix)) db$path[ix[1]] else NA_character_
  
  # Windows フォールバック（パス直指定）
  if (is.na(path) && .Platform$OS.type == "windows") {
    candidates <- c(
      "C:/Windows/Fonts/NotoSansJP-Regular.ttf",
      "C:/Windows/Fonts/NotoSansCJKjp-Regular.otf",
      "C:/Windows/Fonts/YuGothM.ttc",
      "C:/Windows/Fonts/meiryo.ttc"
    )
    hit <- candidates[file.exists(candidates)]
    path <- if (length(hit)) hit[1] else NA_character_
  }
  
  # macOS フォールバック（ヒラギノ）
  if (is.na(path) && Sys.info()[["sysname"]] == "Darwin") {
    candidates <- c(
      "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc",
      "/System/Library/Fonts/ヒラギノ角ゴ ProN W3.otf",
      "/System/Library/Fonts/Hiragino Sans W3.ttc"
    )
    hit <- candidates[file.exists(candidates)]
    path <- if (length(hit)) hit[1] else NA_character_
  }
  
  if (is.na(path)) {
    warning("日本語フォントが見つかりませんでした。Noto Sans JP の導入を推奨します。")
    return(invisible(FALSE))
  }
  
  sysfonts::font_add(family = "jp", regular = path)
  message(sprintf("日本語フォントを 'jp' として登録: %s", basename(path)))
  invisible(TRUE)
}

# 実行（失敗しても描画は sans フォールバックで継続）
register_jp_font()

# ────────────────────────────────────────────────────────────
# 0) ユーザー設定
# ────────────────────────────────────────────────────────────
# 'mikkabi' / 'hosoe' / 'inasa' など
area_name <- "inasa"

# ヘッダー不一致時の挙動：TRUE=常に続行 / FALSE=常に停止 / NA=対話なら確認、非対話は続行
continue_on_header_mismatch <- NA

# データExcel（プロジェクト直下 data/processed）
file_path <- here::here("data", "processed", paste0(area_name, "_population_combined.xlsx"))
file_path <- as.character(file_path)

# 出力先
output_base <- here::here("figures", "pyramid")
output_dir  <- file.path(output_base, paste0(area_name, "地区"))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 年齢ラベル（20区分）
age_groups <- c(
  "0~4","5~9","10~14","15~19","20~24","25~29","30~34","35~39","40~44","45~49",
  "50~54","55~59","60~64","65~69","70~74","75~79","80~84","85~89","90~94","95~"
)

# ────────────────────────────────────────────────────────────
# 1) Excelを一度だけ開く / 対象シートの決定
# ────────────────────────────────────────────────────────────
if (!file.exists(file_path)) stop("Excelファイルが見つかりません: ", file_path)

wb <- openxlsx::loadWorkbook(file_path)
all_sheets <- readxl::excel_sheets(path = file_path)
sheets <- sort(grep("^[0-9]{4}-(04|10)$", all_sheets, value = TRUE))
if (length(sheets) == 0) stop("対象シート（YYYY-04 / YYYY-10）が見つかりません。")

# ────────────────────────────────────────────────────────────
# 2) ヘッダー整合性チェック（raw読み, 列名なし, 列11の括弧付き名称）
# ────────────────────────────────────────────────────────────
rawdata <- lapply(sheets, function(sh) openxlsx::readWorkbook(wb, sheet = sh, colNames = FALSE))

last_data <- rawdata[[length(rawdata)]]
header_positions  <- seq(1, nrow(last_data), by = 45)              # 45行ごと先頭
header_candidates <- as.character(last_data[header_positions, 11]) # 列11
header_cleaned    <- stringr::str_sub(header_candidates, 2, -2)    # 両端の括弧を除去
NAMES_area        <- header_cleaned[!is.na(header_cleaned) & header_cleaned != ""]
n_areas           <- length(NAMES_area)
if (n_areas == 0) stop("最新シートから地区名が取得できませんでした。")

positions <- 1 + 45 * seq(0, n_areas - 1)
headers_list <- lapply(rawdata, function(dat) {
  stringr::str_sub(as.character(dat[positions, 11]), 2, -2)
})
base_header <- headers_list[[length(headers_list)]]
mismatch    <- lapply(headers_list, function(hdr) which(hdr != base_header))
bad_sheets  <- which(sapply(mismatch, length) > 0)

# 不一致があれば差分を表示し、設定に応じて続行/停止（確認ダイアログ可）
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
  cat("ヘッダー整合性チェックに失敗しました。\n",
      paste(msg, collapse = "\n"), "\n\n", sep = "")
  
  proceed <- FALSE
  if (isTRUE(continue_on_header_mismatch)) {
    proceed <- TRUE
  } else if (identical(continue_on_header_mismatch, FALSE)) {
    proceed <- FALSE
  } else {
    if (interactive()) {
      ans <- utils::askYesNo("ヘッダー不一致を検知しました。処理を続行しますか？")
      proceed <- isTRUE(ans)  # TRUE=続行, FALSE/NA=停止
    } else {
      message("非対話モードのため、ヘッダー不一致でも続行します（continue_on_header_mismatch = NA）。")
      proceed <- TRUE
    }
  }
  if (!proceed) stop("ユーザー判断/設定により処理を中断しました。")
  message("ユーザー判断/設定により処理を続行します。")
} else {
  message("ヘッダー整合性チェック：OK")
}
message("地区数：", n_areas, " / 地区名：", paste(NAMES_area, collapse = ", "))

# ────────────────────────────────────────────────────────────
# 3) 解析用データを「列名あり」で先読み（I/O削減）
# ────────────────────────────────────────────────────────────
sdata <- lapply(sheets, function(sh) readxl::read_excel(path = file_path, sheet = sh, col_names = TRUE))
names(sdata) <- sheets

# ────────────────────────────────────────────────────────────
# 4) 各シートごとに全地区の割合データを作成 → PDF化
#    ・男女合計100%チェック：ズレたら stop
#    ・PDFは cairo_pdf + 日本語フォント family="jp"
# ────────────────────────────────────────────────────────────
for (current_date in sheets) {
  sheet <- sdata[[current_date]]
  male_data   <- vector("list", n_areas)
  female_data <- vector("list", n_areas)
  
  for (i in seq_len(n_areas)) {
    idx0    <- i - 1
    start   <- 2 + 45 * idx0
    divisor <- suppressWarnings(as.numeric(sheet[start + 41, 10][[1]]))  # 総人口
    
    if (is.na(divisor) || divisor == 0) {
      message("⚠️ 人口合計が無効 → ", NAMES_area[i], " / ", current_date)
      male_data[[i]]   <- rep(NA_real_, length(age_groups))
      female_data[[i]] <- rep(NA_real_, length(age_groups))
      next
    }
    
    # 男性/女性の生値（Rmd準拠：列3/7/11 と 4/8/12 の階段取り）
    male_raw   <- suppressWarnings(as.numeric(unlist(sheet[start + 6 * (0:6), c(3, 7, 11)])))
    female_raw <- suppressWarnings(as.numeric(unlist(sheet[start + 6 * (0:6), c(4, 8, 12)])))
    
    if (length(male_raw) < 20 || length(female_raw) < 20) {
      message("⚠️ 生データ不足 → ", NAMES_area[i], " / ", current_date)
      male_data[[i]]   <- rep(NA_real_, length(age_groups))
      female_data[[i]] <- rep(NA_real_, length(age_groups))
      next
    }
    
    # 割合（%）へ変換
    male   <- male_raw[1:20]   / divisor * 100
    female <- female_raw[1:20] / divisor * 100
    
    # 男女合計100%チェック（微小誤差許容）
    total_pct <- sum(male, na.rm = TRUE) + sum(female, na.rm = TRUE)
    if (!isTRUE(all.equal(total_pct, 100, tolerance = 1e-6))) {
      stop(sprintf("男女合計100%%チェックに失敗：シート=%s / 地区=%s / 合計=%.6f%%（差分=%.6f）",
                   current_date, NAMES_area[i], total_pct, total_pct - 100))
    }
    
    male_data[[i]]   <- male
    female_data[[i]] <- female
  }
  
  # PDF出力（Cairoでフォント埋め込み / family = "jp"）
  pdf_path <- file.path(output_dir, paste0(area_name, "_pyramids_", current_date, ".pdf"))
  grDevices::cairo_pdf(filename = pdf_path, width = 5, height = 5.2, family = "jp")
  par(cex.main = 1.5, cex.lab = 0.5, cex.axis = 0.5, cex = 0.5, family = "jp")
  
  for (i0 in 0:(n_areas - 1)) {
    pyramids(
      Left   = male_data[[i0 + 1]],
      Right  = female_data[[i0 + 1]],
      Lcol   = c(rep("#00A0CD", 3), rep("#71C7D5", 10), rep("#00A0CD", 10)),
      Rcol   = c(rep("#EE86A7", 3), rep("#F6BBC6", 10), rep("#EE86A7", 10)),
      Center = age_groups,
      Laxis  = seq(0, 5, length.out = 6),
      Clab   = "Age (years)",
      Llab   = "Male",
      Rlab   = "Female",
      Cstep  = 1,
      main   = paste0(NAMES_area[i0 + 1], "（", current_date, "）"),
      lwd    = 10
    )
  }
  dev.off()
  message("✔ 出力完了：", pdf_path)
}

message("すべてのPDF出力が完了しました。")
