price_diff <- function (buy, sell, fees = 0.001){
  ((sell/buy * (1 - fees)^2) - 1)
}
