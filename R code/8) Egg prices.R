
#########################
######## Retail #########
#########################

month0 <- "01"
month1 <- "12"
day0 <- "01"
day1 <- "31"

rm(dta)

for (y0 in year0:year1){
  y1 <- y0+1
  link0 <- paste("https://www.marketnews.usda.gov/mnp/py-report-retail?&region=ALL&repDate=", month0, "%2F", day0, "%2F", as.character(y0), "&repType=item&portal=py&state=ALL&class=Retail&endDate=", month1,"%2F", day1, "%2F", as.character(y1), "&type=ALL&dataValue=&operator=%3C&dataType=None+Selected&commodity=EGGS&runReport=true&description=ALL&format=text", sep="")
  dta2 <- read_fwf(link0, fwf_empty(link0), skip=2) %>% data.frame
  for (i in 6:ncol(dta2)){dta2[,i] <- as.numeric(dta2[,i])}
  dta2 <- dta2[ , !apply(dta2 , 2 , function(x) all(is.na(x)) ) ]
  colnames(dta2) <- c("date", "region", "type", "form", "description", "stores", "avg_price", "low_price", "high_price")
  dta2 <- dta2[,colnames(dta2)[!is.na(colnames(dta2))]]
  dta2$date <- as.Date(dta2$date, "%m/%d/%Y")
  dta2 <- dta2[!is.na(dta2$date),]
  #Merge the data
  ifelse("dta" %in% ls(), dta <- rbind(dta, dta2), dta <- dta2)
  #Remove duplicates
  dta <- dta[!duplicated(dta),]
}

dta <- dta %>% arrange(date)

write.xlsx(data.frame(dta), "Data/8) Egg retail prices.xlsx", row.names = FALSE)

############################
######## Wholesale #########
############################

rm(dta)

for (y0 in year0:year1){
  y1 <- y0+1
  link0 <- paste("https://marketnews.usda.gov/mnp/py-report?&endYear=2015&repDate=", month0, "%2F", day0, "%2F", y0, "&repMonth=1&repYear=2015&previouscls=Breaking+Stock&endDate=", month1, "%2F", day1, "%2F", y1, "&frequency=Daily&categoryDesc=Egg&run=Run&category=Egg&_producttypefrom=1&subcategory=Shell+Eggs&runReport=true&_producttype=1&report=NW_PY018&commodityDesc=Shell+Eggs&datatype=None+Selected&regionsDesc=&endMonth=1&producttypefrom=ALL&format=text", sep="")
  dta2 <- read_fwf(link0, fwf_empty(link0), skip=2)
  dta2 <- dta2[ , ! apply(dta2 , 2 , function(x) all(is.na(x)) ) ]
  colnames(dta2) <- c("date", "class", "grade", "market", "delivery", "unit", "low_price", "high_price", "mostly_low", "mostly_high")
  dta2$date <- as.Date(dta2$date, "%m/%d/%Y")
  dta2 <- dta2[!is.na(dta2$date),]
  #Merge the data
  ifelse("dta" %in% ls(), dta <- rbind(dta, dta2), dta <- dta2)
  #Remove duplicates
  dta <- dta[!duplicated(dta),]
}

#Remove duplicates
dta <- dta[!duplicated(dta),]
dta <- dta[,colnames(dta)[!is.na(colnames(dta))]]
dta <- dta %>% arrange(date)

write.xlsx(data.frame(dta), "Data/8) Egg wholesale prices.xlsx", row.names = FALSE)

#########################
######## Stocks #########
#########################

rm(dta)

for (y0 in year0:year1){
  y1 <- y0+1
  link0 <- paste("https://www.marketnews.usda.gov/mnp/py-report?&endYear=", y1, "&repDate=", month0, "%2F", day0, "%2F", y0, "&repMonth=1&repYear=", year0, "&previouscls=Liquid&endDate=", month1, "%2F", day1, "%2F", y1, "&frequency=Weekly&categoryDesc=Egg&run=Run&category=Egg&_producttypefrom=1&subcategory=Breaking+Stock&runReport=true&_producttype=1&report=NW_PY023&commodityDesc=Breaking+Stock&datatype=None+Selected&regionsDesc=&endMonth=1&producttypefrom=&format=text", sep="")
  dta2 <- read_fwf(link0, fwf_empty(link0), skip=3)
  dta2 <- dta2[ , ! apply(dta2 , 2 , function(x) all(is.na(x)) ) ]
  colnames(dta2) <- c("date", "region", "class", "volume", "unit", "diff")
  dta2$date <- as.Date(dta2$date, "%m/%d/%Y")
  dta2 <- dta2[!is.na(dta2$date),]
  #Merge the data
  ifelse("dta" %in% ls(), dta <- rbind(dta, dta2), dta <- dta2)
  #Remove duplicates
  dta <- dta[!duplicated(dta),]
}

dta <- dta %>% arrange(date)

dta$volume <- as.numeric(gsub(",","", dta$volume))

write.xlsx(data.frame(dta), "Data/8) Egg stocks.xlsx", row.names = FALSE)



