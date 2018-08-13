
rm(list = ls())
gc()

library(dplyr)
library(magrittr)
library(ggplot2)
library(reshape2)
library(grid)#for arrow in graph
library(gridExtra)
library(scales)
library(lubridate)
library(readr)
library(stringr)
library(readxl)
library(xlsx)
library(Quandl)
library(viridis)
library(extrafont)
loadfonts() #Load the fonts

source("D:/Box Sync/Graph parameters.R")

setwd("D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Slides")

#########################
######## Retail #########
#########################


year0 <- 2010 #Can go as far back as 2000
year1 <- 2017
month0 <- "01"
month1 <- "12"
day0 <- "01"
day1 <- "31"

for (y0 in year0:year1){
  year0 <- y0
  y1 <- year0+1
  link0 <- paste("https://www.marketnews.usda.gov/mnp/py-report-retail?&region=ALL&repDate=", month0, "%2F", day0, "%2F", as.character(year0), "&repType=item&portal=py&state=ALL&class=Retail&endDate=", month1,"%2F", day1, "%2F", as.character(y1), "&type=ALL&dataValue=&operator=%3C&dataType=None+Selected&commodity=EGGS&runReport=true&description=ALL&format=text", sep="")
  dta2 <- read_fwf(link0, fwf_empty(link0), skip=2)
  dta2 <- dta2[ , ! apply(dta2 , 2 , function(x) all(is.na(x)) ) ]
  colnames(dta2) <- c("date", "region", "type", "form", "description", "stores", "avg_price", "low_price", "high_price")
  dta2$date <- as.Date(dta2$date, "%m/%d/%Y")
  dta2 <- dta2[!is.na(dta2$date),]
  #Merge the data
  ifelse("dta" %in% ls(), dta <- rbind(dta, dta2), dta <- dta2)
  #Remove duplicates
  dta <- dta[!duplicated(dta),]
}

dta <- dta %>% arrange(date)

rm(list=setdiff(ls(), c("dta", "mytheme")))

save.image("egg_data.RData")

load("egg_data.RData")

####################
### Prepare data ###
####################

dta_shell <- dta %>% filter(region == "NATIONAL", type == "CONVENTIONAL", form == "SHELL", description %in% c("WHITE, A, LARGE, DOZ.")) %>% select(-region, -type, -form, -low_price, -high_price, -description, -stores) %>% rename(`White A large shell egg ($/doz)` = avg_price)

dta_liquid <- dta %>% filter(region == "NATIONAL", type == "CONVENTIONAL", form == "LIQUID", description %in% c("LIQUID EGG, SMALL CARTON")) %>% select(-region, -type, -form, -low_price, -high_price, -description, -stores) %>% rename(`Liquid egg ($/small carton)` = avg_price)


dta <- left_join(dta_shell, dta_liquid, by = c("date"))
dta  <- melt(dta, id="date")
  
###################
### Make graphs ###
###################


#######################
### Graph of prices ###
#######################

plot <- ggplot(data=dta %>% filter(date>"2010-01-01"), aes(x=date, y=value, color=variable)) + geom_point(aes(shape=variable), size=1.25) + ylab("Price") + xlab("") + scale_x_date(date_breaks="1 year", date_minor_breaks="1 months", labels=date_format("%Y")) + theme_bw()+ mytheme + theme(legend.position=c(0.25,0.85), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'))

plot <- plot + geom_smooth(se = FALSE, span=0.25)

# Save plot
ggsave(plot, filename = "Egg_prices.png",  width = 6, height = 4, units = "in", dpi = 600)


############################
######## Wholesale #########
############################

rm(list=setdiff(ls(), c("mytheme")))

year0 <- 2010
year1 <- 2017
month0 <- "01"
month1 <- "12"
day0 <- "01"
day1 <- "31"

for (y0 in year0:year1){
  year0 <- y0
  y1 <- year0+1
  link0 <- paste("https://marketnews.usda.gov/mnp/py-report?&endYear=2015&repDate=", month0, "%2F", day0, "%2F", year0, "&repMonth=1&repYear=2015&previouscls=Breaking+Stock&endDate=", month1, "%2F", day1, "%2F", year1, "&frequency=Daily&categoryDesc=Egg&run=Run&category=Egg&_producttypefrom=1&subcategory=Shell+Eggs&runReport=true&_producttype=1&report=NW_PY018&commodityDesc=Shell+Eggs&datatype=None+Selected&regionsDesc=&endMonth=1&producttypefrom=ALL&format=text", sep="")
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
dta <- dta %>% arrange(date)

rm(list=setdiff(ls(), c("dta", "mytheme")))

save.image("egg_wholesale_data.RData")

load("egg_wholesale_data.RData")

####################
### Prepare data ###
####################

dta_w <- dta %>% 
  dplyr::select(1:10) %>%
  dplyr::filter(class == "LARGE",market == "IOWA-MINNESOTA-WISCONSIN", delivery == "PAID TO PRODUCERS") %>% 
  mutate(avg_price = (mostly_low+mostly_high)/200)


###################
### Make graphs ###
###################

#######################
### Graph of prices ###
#######################

plot <- ggplot(data=dta_w, aes(x=date, y=avg_price)) + geom_point(size=1.25) + ylab("Price") + xlab("") + scale_x_date(date_breaks="1 year", date_minor_breaks="1 months", labels=date_format("%Y")) + theme_bw()+ mytheme 

plot <- plot + geom_smooth(se = FALSE, span=0.25)

# Save plot
ggsave(plot, filename = "Egg_wholesale_prices.png",  width = 6, height = 4, units = "in", dpi = 600)


#########################
######## Stocks #########
#########################
rm(list=setdiff(ls(), c("mytheme")))

year0 <- 2000
year1 <- 2017
month0 <- "01"
month1 <- "12"
day0 <- "01"
day1 <- "31"


for (y0 in year0:year1){
  year0 <- y0
  y1 <- year0+1
  link0 <- paste("https://www.marketnews.usda.gov/mnp/py-report?&endYear=", y1, "&repDate=", month0, "%2F", day0, "%2F", year0, "&repMonth=1&repYear=", year0, "&previouscls=Liquid&endDate=", month1, "%2F", day1, "%2F", y1, "&frequency=Weekly&categoryDesc=Egg&run=Run&category=Egg&_producttypefrom=1&subcategory=Breaking+Stock&runReport=true&_producttype=1&report=NW_PY023&commodityDesc=Breaking+Stock&datatype=None+Selected&regionsDesc=&endMonth=1&producttypefrom=&format=text", sep="")
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

rm(list=setdiff(ls(), c("dta", "mytheme")))

dta$volume <- as.numeric(gsub(",","", dta$volume))

save.image("egg_stocks.RData")

load("egg_stocks.RData")

###################
### Make graphs ###
###################

#######################
### Graph of prices ###
#######################

plot <- ggplot(data=dta, aes(x=date, y=volume)) + geom_point(size=0.5) + ylab("Stock") + xlab("") + scale_x_date(date_breaks="1 year", date_minor_breaks="1 months", labels=date_format("%Y")) + theme_bw()+ mytheme 
plot <- plot + geom_smooth(se = FALSE, span=0.25)

# Save plot
ggsave(plot, filename = "Egg_stocks_breakers.png",  width = 6, height = 4, units = "in", dpi = 600)


##################################
######## Combined graphs #########
##################################
rm(list = ls())
gc()

#Retail price data
load("egg_data.RData")

dta_r <- dta %>% 
  filter(region == "MIDWEST", type == "CONVENTIONAL", form == "SHELL", description %in% c("WHITE, A, LARGE, DOZ.")) %>% 
  dplyr::select(-region, -type, -form, -low_price, -high_price, -description, -stores) %>% 
  rename(price = avg_price) %>% 
  dplyr::filter(date>="2010-01-01") %>% 
  mutate(week = 1+ (as.numeric(date) - 14617)/7) %>% mutate(stage = "Retail")

rm(dta)

#Wholesale
load("egg_wholesale_data.RData")

dta_w <- dta %>% 
  dplyr::select(1:10) %>%
  dplyr::filter(class == "LARGE",market == "IOWA-MINNESOTA-WISCONSIN", delivery == "PAID TO PRODUCERS") %>% 
  mutate(avg_price = (mostly_low+mostly_high)/200)

dta_w <- dta_w %>% 
  mutate(week = trunc(1+ (as.numeric(date) - 14617)/7)) %>% 
  group_by(week) %>% summarize(price = mean(avg_price), date = min(date)) %>% 
  mutate(stage = "Wholesale")

#Merge data
dta <- rbind(dta_r, dta_w)


plot <- ggplot(data=dta, aes(x=date, y=price, color = stage, shape = stage)) + 
  geom_point(size=0.5) + 
  ylab("Price ($/dozen)") +
  scale_x_date(date_breaks="1 year", date_minor_breaks="1 months", labels=date_format("%Y")) + 
  scale_color_manual(values = viridis(5)[c(1,4)]) + 
  theme_bw()+ 
  mytheme + 
  theme(legend.position=c(0.25, 0.75), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'), axis.title.x=element_blank())

plot <- plot + geom_smooth(se = FALSE, span=0.4)

plot


ggsave(plot, filename = "Egg_retail_wholesale.png",  width = 6, height = 4, units = "in", dpi = 600)

