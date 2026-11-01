# BDA400 Assignment 1 -- Navneet Kaur
library(quantmod)
library(TTR)

load_stock_data <- function(portfolio_file = "portfolio.txt", years_back = 1) {
  symbols <- read.table(portfolio_file, stringsAsFactors = FALSE)[[1]]
  start   <- Sys.Date() - 365 * years_back

  pull <- function(sym) {
    quotes <- getSymbols(sym, src = "yahoo", from = start, auto.assign = FALSE)
    data.frame(Date     = as.Date(index(quotes)),
               Open     = as.numeric(quotes[, 1]),
               High     = as.numeric(quotes[, 2]),
               Low      = as.numeric(quotes[, 3]),
               Close    = as.numeric(quotes[, 4]),
               Volume   = as.numeric(quotes[, 5]),
               Adjusted = as.numeric(quotes[, 6]))
  }

  out <- list()
  for (s in symbols) {
    res <- tryCatch(pull(s), error = function(e) NULL)
    if (!is.null(res)) out[[s]] <- res
  }
  out
}

# Mode of a numeric vector: bucket to the nearest 25 cents, return the busiest bucket
bucket_mode <- function(v) {
  buckets <- round(v * 4) / 4
  tab <- table(buckets)
  as.numeric(names(tab)[tab == max(tab)][1])
}

calculate_statistics <- function(price_df, avg_days = 30) {
  v <- price_df$Close
  list(
    n_days    = length(v),
    mean      = mean(v),
    median    = median(v),
    mode      = bucket_mode(v),
    sd        = sd(v),
    moving_av = SMA(v, n = avg_days)
  )
}

# Tidy table: one row per stock, easy to print or export
stats_table <- function(stats_by_stock) {
  data.frame(
    Stock  = names(stats_by_stock),
    Days   = sapply(stats_by_stock, `[[`, "n_days"),
    Mean   = sapply(stats_by_stock, `[[`, "mean"),
    Median = sapply(stats_by_stock, `[[`, "median"),
    Mode   = sapply(stats_by_stock, `[[`, "mode"),
    SD     = sapply(stats_by_stock, `[[`, "sd"),
    row.names = NULL
  )
}
