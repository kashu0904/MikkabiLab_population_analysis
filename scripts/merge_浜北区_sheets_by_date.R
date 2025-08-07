# ------------------------------------------------------------
# スクリプト名: merge_mikkabi_inasa_hosoe_sheets_by_date.R
# 概要:
#   「三ヶ日地区」「引佐地区」「細江地区」の3地区のシートを、
#   各Excelファイルから抽出し、それぞれの地区ごとに1つのExcelファイルに統合する。
#   シート名はすべて "YYYY-MM" の西暦形式に統一され、年月順に並ぶ。
#
# 主な処理内容:
#   - 元号（平成・令和）を西暦に自動変換
#   - シート名を "YYYY-MM" 形式に統一
#   - 各地区の存在を確認し、存在する場合のみ読み込み・格納
#   - 空データや欠損ファイルは自動でスキップ
#   - 処理対象ファイルが追加されても自動で対応可能（拡張性あり）
#
# 入力ディレクトリ:
#   C:/Users/pirat/Documents/mikkabi_population_analysis/data/raw/Population_By_Town_and_Age/hamakitaku and hamanaku
#
# 出力ファイル:
#   - 三ヶ日地区: C:/Users/pirat/Documents/mikkabi_population_analysis/data/processed/mikkabi_population_combined.xlsx
#   - 引佐地区  : C:/Users/pirat/Documents/mikkabi_population_analysis/data/processed/inasa_population_combined.xlsx
#   - 細江地区  : C:/Users/pirat/Documents/mikkabi_population_analysis/data/processed/hosoe_population_combined.xlsx
#
# 備考:
#   - ファイル名には元号・年・月が "h31-04-" のように含まれている必要あり
#   - 将来的に新しい年度のファイルを追加しても再利用可能
# ------------------------------------------------------------

library(readxl)
library(openxlsx)

# 入力／出力パス
input_dir <- "C:/Users/pirat/Documents/mikkabi_population_analysis/data/raw/Population_By_Town_and_Age/hamakitaku and hamanaku"

output_paths <- list(
  "浜北区"   = "C:/Users/pirat/Documents/mikkabi_population_analysis/data/processed/hamakitashi_population_combined.xlsx",
  "浜名地区" = "C:/Users/pirat/Documents/mikkabi_population_analysis/data/processed/hamana_population_combined.xlsx",
  "北浜地区" = "C:/Users/pirat/Documents/mikkabi_population_analysis/data/processed/kitahama_population_combined.xlsx",
  "中瀬地区" = "C:/Users/pirat/Documents/mikkabi_population_analysis/data/processed/nakaze_population_combined.xlsx",
  "赤佐地区" = "C:/Users/pirat/Documents/mikkabi_population_analysis/data/processed/akasa_population_combined.xlsx",
  "麁玉地区" = "C:/Users/pirat/Documents/mikkabi_population_analysis/data/processed/aratama_population_combined.xlsx"
)

# ファイル一覧取得
files <- list.files(path = input_dir, pattern = "^jinkousu_areaage_.*\\.(xls|xlsx)$", full.names = TRUE)

sheets_all <- list(
  "浜北区"   = list(),
  "浜名地区" = list(),
  "北浜地区" = list(),
  "中瀬地区" = list(),
  "赤佐地区" = list(),
  "麁玉地区" = list()
)
  # ファイルごとに処理
  for(file in files) {
    # ファイル名から元号と年月を抽出
    parts <- regexec("([hr])([0-9]+)-([0-9]+)-", basename(file))
    groups <- regmatches(basename(file), parts)[[1]]
    
    if(length(groups) < 4) {
      cat("⚠️ スキップ（日付抽出不可）：", file, "\n")
      next
    }
    
    era <- groups[2]
    year <- as.numeric(groups[3])
    month <- as.numeric(groups[4])
    
    # 年号変換
    year_ad <- if (era == "h") year + 1988 else year + 2018
    sheet_name <- sprintf("%d-%02d", year_ad, month)
    
    for (area in names(sheets_all)) {
      # 対象シートが存在するか確認してから読み込む
      sheet_names <- excel_sheets(file)
      if (area %in% sheet_names) {
        dat <- read_excel(file, sheet = area)
        sheets_all[[area]][[sheet_name]] <- dat
        cat("✅ 読み込み：", basename(file), " → 地区：", area, " → シート名：", sheet_name, "\n")
      } else {
        cat("⚠️ シートなし：", basename(file), " → 地区：", area, "\n")
      }
    }
  }
  
  # 地区ごとに書き出し
  for (area in names(sheets_all)) {
    write.xlsx(sheets_all[[area]], file = output_paths[[area]])
    cat("📤 書き出し完了：", area, " → ", output_paths[[area]], "\n")
  }
  