# BDA400 Assignment 1 -- Navneet Kaur -- displaying the results
source("stock_functions.R")

stocks <- load_stock_data("portfolio.txt")
stats  <- lapply(stocks, calculate_statistics)

write_report <- function(stocks, stats) {
  cat("PORTFOLIO REPORT\n", rep("-", 40), "\n", sep = "")
  for (s in names(stocks)) {
    d <- stocks[[s]]
    cat(sprintf("\n%s  (%s -> %s)\n", s, format(min(d$Date)), format(max(d$Date))))
    cat(sprintf("  Highest close: %.2f on %s\n", max(d$Close), format(d$Date[which.max(d$Close)])))
    cat(sprintf("  Lowest close : %.2f on %s\n", min(d$Close), format(d$Date[which.min(d$Close)])))
    cat(sprintf("  Mean / median: %.2f / %.2f\n", stats[[s]]$mean, stats[[s]]$median))
  }
}

write_report(stocks, stats)
print(stats_table(stats), digits = 4)

# Price chart with a shaded high-low range and the moving average on top
for (s in names(stocks)) {
  d <- stocks[[s]]
  plot(d$Date, d$Close, type = "n", ylim = range(d$Low, d$High),
       main = paste(s, "- daily range and 30-day average"), xlab = "", ylab = "Price")
  polygon(c(d$Date, rev(d$Date)), c(d$High, rev(d$Low)),
          col = adjustcolor("grey70", 0.5), border = NA)
  lines(d$Date, d$Close, col = "navy")
  lines(d$Date, stats[[s]]$moving_av, col = "red", lwd = 2)
}
