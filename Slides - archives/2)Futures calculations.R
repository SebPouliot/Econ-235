

futures_func <- function(dates, prices, maint_margin, n_contracts, position){
  #Catch error 
  try(if(length(dates) != length(prices)) stop("Length of dates does not equal length of prices"))
  #Make data frame
  tab <- data.frame(array(NA, c(length(dates), 7)))
  colnames(tab) <- c("Date", "Price", "Value", "Margin", "daily_gain", "cum_gain", "margin_call")
  tab <- as.data.frame(cbind(dates, prices))
  tab$prices <- as.numeric(as.character(tab$prices))
  tab$value <- n_contracts*5000*tab$prices/100
  #Add columns
  tab$margin <- NA
  tab$margin[1] <- maint_margin*1.1*n_contracts
  tab$daily_gain <- NA 
  tab$cum_gain <- NA
  tab$margin_call <- NA
  #Do calculations  
  if(position == "long"){
    for(i in 2:nrow(tab)){
      tab$daily_gain[i] <- tab$value[i] - tab$value[i-1]
      tab$cum_gain[i] <- tab$value[i] - tab$value[1]
      tab$margin[i] <- ifelse(tab$margin[i-1] + tab$daily_gain[i] > maint_margin*n_contracts, tab$margin[i-1] + tab$daily_gain[i], maint_margin*1.1*n_contracts)
      tab$margin_call[i] <- ifelse(tab$margin[i-1] + tab$daily_gain[i] > maint_margin*n_contracts, 0, maint_margin*1.1*n_contracts - (tab$margin[i-1] + tab$daily_gain[i]))
    }
  }
  if(position == "short"){
    for(i in 2:nrow(tab)){
      tab$daily_gain[i] <- tab$value[i-1] - tab$value[i]
      tab$cum_gain[i] <- tab$value[1] - tab$value[i] 
      tab$margin[i] <- ifelse(tab$margin[i-1] + tab$daily_gain[i] > maint_margin*n_contracts, tab$margin[i-1] + tab$daily_gain[i], maint_margin*1.1*n_contracts)
      tab$margin_call[i] <- ifelse(tab$margin[i-1] + tab$daily_gain[i] > maint_margin*n_contracts, 0, maint_margin*1.1*n_contracts - (tab$margin[i-1] + tab$daily_gain[i]))
    }
  }
  return(tab)
}

#dates <- c("October 8", "October 9", "October 10", "October 11", "October 12")
dates <- c("1", "2", "3", "4", "5")
#prices <- c(320, 312, 311, 305, 308)
prices <- c(800, 792, 794, 790, 790)
maint_margin <- 1500
n_contracts <- 2

futures_func(dates, prices, maint_margin, n_contracts, "long")



