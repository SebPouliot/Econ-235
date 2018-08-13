############################
### Hedging with futures ###
############################

hedging_futures <- function(f0, f1, b0, b1, cash.position){
  #Long
  if(cash.position == "long"){
  tab <- data.frame(futures = c(f0, f1, f0-f1, NA, NA, NA, NA),
                    cash = c(f0 + b0, f1 + b1, (f1 + b1) - (f0 + b0), NA, NA, NA, NA),
                    basis = c(b0, b1, b1-b0, f0 + b0, (f1 + b1) - (f0 + b0), f0-f1, f0 + b1))
  }
  #Short
  if(cash.position == "short"){
    tab <- data.frame(futures = c(f0, f1, f1-f0, NA, NA, NA, NA),
                      cash = c(f0 + b0, f1 + b1, (f0 + b0) - (f1 + b1), NA, NA, NA, NA),
                      basis = c(b0, b1, b1-b0, f0 + b0, (f0 + b0) - (f1 + b1), f1-f0, f0 + b1))
  }
  rownames(tab) <- c("May price", "December price", "Gain/loss", "Cash price at beginning", "Gain from cash", "Gain from futures", "Net price")
  return(tab)  
}

hedging_futures(f0 = 5.5, f1 = 5, b0 = -0.75, b1 = -0.25, cash.position = "short")

############################
### Hedging with options ###
############################

hedging_options <- function(f0, f1, strike, p0, b0, b1, cash.position){
  #Long
  if(cash.position == "long"){
    tab <- data.frame(futures = c(f0, f1, f0-f1, NA, NA, NA, NA),
                      option = c(p0, max(strike - f1, 0), max(strike - f1, 0) - p0, NA, NA, NA, NA),
                      cash = c(f0 + b0, f1 + b1, (f1 + b1) - (f0 + b0), NA, NA, NA, NA),
                      basis = c(b0, b1, b1-b0, f0 + b0, (f1 + b1) - (f0 + b0), max(strike - f1, 0) - p0, f1 + b1 + max(strike - f1, 0) - p0))
  }
  #Short
  if(cash.position == "short"){
    tab <- data.frame(futures = c(f0, f1, f1-f0, NA, NA, NA, NA),
                      option = c(p0, max(f1 - strike, 0), max(f1 - strike, 0) - p0, NA, NA, NA, NA),
                      cash = c(f0 + b0, f1 + b1, (f0 + b0) - (f1 + b1), NA, NA, NA, NA),
                      basis = c(b0, b1, b1-b0, f0 + b0, (f0 + b0) - (f1 + b1), max(f1 - strike, 0) - p0, f1 + b1 - max(f1 - strike, 0) + p0))
  }
  rownames(tab) <- c("May price", "December price", "Gain/loss", "Cash price at beginning", "Gain from cash", "Gain from options", "Net price")
  return(tab)  
}

hedging_options(f0 = 6, f1 = 5.5, strike = 5.5, p0 = 0.05, b0 = -0.25, b1 = -0.25, cash.position = "long")




