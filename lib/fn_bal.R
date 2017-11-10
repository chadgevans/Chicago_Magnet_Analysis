fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$TRAD_MAGNET <- as.factor(dta$TRAD_MAGNET)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = TRAD_MAGNET)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}
