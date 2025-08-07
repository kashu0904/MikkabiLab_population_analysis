library(ggplot2)
## main_plot.R (メインスクリプト)
#----------------------------------------
# ★★★ 地域指定 ★★★
area_name <- "inasa"  
# mikkabi, hosoe, inasa などに変えるだけ
#----------------------------------------
width_std_latest <- 0.5
latest_area_label_size <- 5
latest_arate_breaks    <- seq(0, 60, by = 5)
latest_arate_limits    <- c(0, 61.5)

expand_x_std_latest  <- expansion(mult = c(0.04, 0.04)) 
# X軸（年）方向の余白の割合。左右にどれだけスペースをあけるかを調整。
expand_y_std_latest  <- expansion(mult = c(0.04, 0.04)) 
# Y軸（人数や率）方向の余白の割合。上下にどれだけスペースをあけるかを調整。
# ❶ 共通処理読み込み
source("C:/Users/pirat/Documents/mikkabi_population_analysis/lib/plot_population_by_area_common.R")

custom_scales <- setNames(
  vector("list", length(NAMES_area)),
  NAMES_area
)


#----------------------------------------
# Arate（％）専用 auto scale
#----------------------------------------
# auto_scale: データ最大値にパディングをかけ、いい感じの目盛＆上限を返す
# x   : 数値ベクトル（例 df$age0004）
# n   : 目盛の本数目安（大きいほど細かい）
#   pad_brk  - 目盛生成用にかける倍率（例1.05 → データ最大の105%でpretty）
#   pad_lim  - limits用にかける倍率（例0.95 → データ最大の95%を上限に）
auto_rate_scale <- function(x, step = 5, pad = 1.10, min_limit = 0, max_limit = 60) {
  # step: ％刻み幅
  # pad : 自動スケーリング時の倍率
  # min_limit: Y軸の下限（デフォルト0）
  # max_limit: 手動で指定したい上限（NULLなら自動）
  mx <- max(x, na.rm = TRUE)
  # 自動上限候補（手動指定がない場合）
  top_auto <- ceiling(mx / step) * step * pad
  top <- if (!is.null(max_limit)) max_limit else top_auto
  brks <- seq(min_limit, top, by = step)
  list(breaks = brks, limits = c(min_limit, top))
}

all_plots <- lapply(NAMES_area, plot_for_area)
names(all_plots) <- NAMES_area
NAMES_area

p7

#----------------------------------------
area1 <- "引佐町四方浄"　# 全体
#----------------------------------------
#「0〜4歳」グラフを表示
print(all_plots[[area1]]$p1)
#「0〜14歳」グラフを表示
print(all_plots[[area1]]$p2)
#「15〜64歳」グラフを表示
print(all_plots[[area1]]$p3)
#「65歳以上」グラフを表示
print(all_plots[[area1]]$p4)
#「総人口」グラフを表示
print(all_plots[[area1]]$p5)
#「高齢化率の推移」グラフを表示
print(all_plots[[area1]]$p6)
