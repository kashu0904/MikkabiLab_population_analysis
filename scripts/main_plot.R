# ======================================================================
# main_plot.R  — 実行用スクリプト（機能は変更せず、操作を明確化）
# ----------------------------------------------------------------------
# 1) 下の「基本操作」だけ触ればOK。必要なら「詳細設定（任意）」で微調整。
# 2) カスタム目盛は custom_scales で area_name ごとに指定（例あり）。
# ======================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(here)
})

# ----------------------------------------------------------------------
# 基本操作
# ----------------------------------------------------------------------
# ① データ対象（Excelの接頭）を指定： 'mikkabi' / 'hosoe' / 'inasa' など
area_name <- "inasa"

# ② 共通モジュールを読み込み（相対パス）
source(here::here("lib", "plot_population_by_area_common.R"), encoding = "UTF-8")

# ③ 描画したい地区（町字）を選ぶ
#    ※ 一覧は plot_population_by_area_common.R 内で読み込まれる NAMES_area を参照
target_area <- "引佐町四方浄"

# ----------------------------------------------------------------------
# 詳細設定（任意）— 最新年の比較図の見た目（ユーザー要望のまとめ）
# ----------------------------------------------------------------------
width_std_latest       <- 0.5
latest_area_label_size <- 5
latest_arate_breaks    <- seq(0, 60, by = 5)
latest_arate_limits    <- c(0, 61.5)
expand_x_std_latest    <- expansion(mult = c(0.04, 0.04))  # X左右余白
expand_y_std_latest    <- expansion(mult = c(0.04, 0.04))  # Y上下余白

# ----------------------------------------------------------------------
# 【custom_scales HOWTO（main_plot.R 側で設定）】
#  - 面ごと（area_name ごと）に Y軸の breaks/limits を固定したいときに使います。
#  - 指定しない指標は自動スケール（auto_scale/auto_rate_scale）を採用します。
#  - 指標名は: age0004 / age0014 / age1564 / age65up / total / Arate
#    ※ Arate は高齢化率（%）。数値類は「人口（人）」です。
#
#  例: main_plot.R で次のように定義してください。
#  --------------------------------------------------------------------
#  custom_scales <- list(
#    "引佐町四方浄" = list(
#      age0004 = list(breaks = seq(0, 80, by = 10), limits = c(0, 80)),
#      total   = list(breaks = seq(0, 1200, by = 200), limits = c(0, 1200)),
#      Arate   = list(breaks = seq(0, 60, by = 5),   limits = c(0, 61.5))
#    ),
#    "三ヶ日町三ヶ日" = list(
#      age65up = list(breaks = seq(0, 900, by = 100), limits = c(0, 900))
#    )
#  )
#  --------------------------------------------------------------------
#  ※ main_plot.R 側で `custom_scales` を定義しなかった場合は、自動スケールのみで描画します。
# ----------------------------------------------------------------------
custom_scales <- list(
  # 例1: 引佐町四方浄の一部指標を固定
  "引佐町四方浄" = list(
    age0004 = list(breaks = seq(0, 80, by = 10),  limits = c(0, 80)),
    total   = list(breaks = seq(0, 1200, by = 200), limits = c(0, 1200)),
    Arate   = list(breaks = seq(0, 60, by = 5),   limits = c(0, 61.5))
  ),
  # 例2: 別エリアで 65歳以上だけ固定
  "三ヶ日町三ヶ日" = list(
    age65up = list(breaks = seq(0, 900, by = 100), limits = c(0, 900))
  )
)
# ----------------------------------------------------------------------
# 実行
# ----------------------------------------------------------------------
plots <- plot_for_area("引佐町渋川")

# 必要な図だけ print() してください
# p1: 0〜4歳 / p2: 0〜14歳 / p3: 15〜64歳 / p4: 65歳以上 / p5: 総人口 / p6: 高齢化率推移
print(plots$p1)
print(plots$p2)
print(plots$p3)
print(plots$p4)
print(plots$p5)
print(plots$p6)
