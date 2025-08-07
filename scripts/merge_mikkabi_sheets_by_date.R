# ------------------------------------------------------------
# スクリプト名: merge_mikkabi_sheets_by_date.R
# 概要:
#   「三ヶ日地区」シートを抽出し、
#   各ファイルの年月（元号表記）を西暦に変換したうえで、
#   シート名を "YYYY-MM" に統一して1つのExcelに統合。
#
# 入力ディレクトリ:
#   C:/Users/pirat/Documents/mikkabi_population_analysis/data/raw/Population_By_Town_and_Age/kitaku and hamanaku
#
# 出力ファイル:
#   C:/Users/pirat/Documents/mikkabi_population_analysis/data/processed/mikkabi_population_combined.xlsx
#
# 特記事項:
#   - 元号は h（平成）と r（令和）に対応
#   - ファイル名の区切りはハイフン（-）で統一されている必要あり
#   - 空のシートは出力されないように除外処理済
#   - 将来ファイルが追加されても自動対応
# ------------------------------------------------------------

library(readxl)
library(openxlsx)

# 入力／出力ディレクトリ指定
input_dir  <- "C:/Users/pirat/Documents/mikkabi_population_analysis/data/raw/Population_By_Town_and_Age/kitaku and hamanaku"
output_file <- "C:/Users/pirat/Documents/mikkabi_population_analysis/data/processed/mikkabi_population_combined.xlsx"

# .xls/.xlsx ファイルをすべて取得
files <- list.files(path = input_dir,
                    pattern = "^jinkousu_areaage_.*\\.(xls|xlsx)$",
                    full.names = TRUE)

sheets_list <- list()

for(file in files) {
  # ファイル名から元号・年・月を抽出
  parts <- regexec("([hr])([0-9]+)-([0-9]+)-", basename(file))
  groups <- regmatches(basename(file), parts)[[1]]
  if(length(groups) < 4) {
    cat("スキップ：日付が抽出できません →", file, "\n")
    next
  }
  
  era <- groups[2]
  year_digit <- as.numeric(groups[3])
  month_digit <- as.numeric(groups[4])
  
  # 元号 → 西暦変換
  year_ad <- if (era == "h") year_digit + 1988 else year_digit + 2018
  month_str <- sprintf("%02d", month_digit)
  sheet_name <- paste0(year_ad, "-", month_str)
  
  cat("読み込み：", file, " → シート名：", sheet_name, "\n")
  
  # 三ヶ日地区シートを読み込む
  dat <- read_excel(file, sheet = "三ヶ日地区")
  sheets_list[[sheet_name]] <- dat
}

# 書き出し（統合ファイル）
write.xlsx(sheets_list, file = output_file)
