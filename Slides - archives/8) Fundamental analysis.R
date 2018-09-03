rm(list = ls())
gc()

library(lubridate)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)
library(readxl)
library(XML)
library(Quandl)
library(xlsx)
library(viridis)

suppressWarnings(try(source("D:/Box Sync/Graph parameters.R"), silent = TRUE))

###########################
### Graphs for corn use ###
###########################

#download.file("https://www.ers.usda.gov/webdocs/DataFiles/50048/Feed%20Grains%20Yearbook%20Tables-All%20Years.xls", destfile = "D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Slides/corn_data.xls", method = "curl")

dta <- read_excel("D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Slides/corn_data.xls", sheet = "FGYearbookTable04-Full", skip = 3) 

dta <- dta %>%
  dplyr::filter(str_detect(X__2, "MY")) 

dta <- dta %>%
  mutate(year = seq(from = 1975, length.out = nrow(dta))) %>%
  dplyr::select(-X__1, -X__2) 

dta_graph <- dta %>%
  dplyr::select(year, `Food, alcohol, and industrial use`:`Feed and residual use`, Exports) %>% 
  gather(var, value, -year) %>%
  mutate(value = value/1000)
  

plot_use <- ggplot(dta_graph, aes(x = year, y = value,  fill = var)) + 
  geom_area(alpha = 0.75) + 
  theme_bw()+ 
  mytheme + 
  scale_x_continuous(breaks = seq(1975, 2017, by = 5), labels = seq(1975, 2017, by = 5), expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,15)) + 
  scale_fill_viridis(discrete = TRUE) + 
  ylab("Billion bushels") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        axis.line = element_line(size = 0.5),
        legend.text=element_text(size=4),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"))

plot_share <- ggplot(dta_graph, aes(x = year, y = value,  fill = var)) + 
  geom_area(alpha = 0.75, position = "fill") + 
  theme_bw()+ 
  mytheme + 
  scale_x_continuous(breaks = seq(1975, 2016, by = 5), labels = seq(1975, 2016, by = 5), expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_viridis(discrete = TRUE)  + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position="bottom", 
        axis.line = element_line(size = 0.5),
        legend.text=element_text(size=4),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"))


##################
### Corn price ###
##################

price <- Quandl("CME/CZ2017")

P0 <- price %>% dplyr::filter(Date >= "2017-05-09", Date < "2017-05-13") %>% summarize(price = mean(Settle)) %>% as.numeric()

#########################################################
### Data from WASDE and futures price by month - 2017 ###
#########################################################

setwd("D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Slides")

#download.file("http://usda.mannlib.cornell.edu/usda/waob/wasde//2010s/2017/wasde-05-10-2017.xls", paste(getwd(), "wasde-05-10-2017.xls", sep="/"), mode="wb", quiet = TRUE)

wasde <- read_excel(paste(getwd(), "wasde-05-10-2017.xls", sep="/"), sheet = "Page 12")

row_1 <- which(wasde[,1] == "CORN") + 1
row_n <- nrow(wasde) - 1

wasde <- wasde[row_1:row_n,]

colnames(wasde) <- c("var", "y2015", "y2016", "april", "may")
wasde <- wasde %>% 
  dplyr::filter(!is.na(var)) %>% 
  mutate(var = str_trim(var, side = "both")) %>%
  as.data.frame()

#Calibration of demand
s0 <- as.numeric(wasde[wasde$var == "Beginning Stocks", "may"])/1000
Q0 <- as.numeric(wasde[wasde$var == "Supply, Total", "may"])/1000
A0 <- as.numeric(str_sub(wasde[wasde$var == "Area Harvested", "may"], 1, 4))
  
#Parameters when eta=-0.2
b_2 <- 0.2*Q0/P0
a_2 <- Q0 + b_2*P0

#Parameters when eta=-0.3
b_3 <- 0.3*Q0/P0
a_3 <- Q0 + b_3*P0

#Parameters when eta=-0.4
b_4 <- 0.4*Q0/P0
a_4 <- Q0 + b_4*P0

#Graph
dta = data.frame(array(NA, c(100,6)))
colnames(dta) <- c("Yield", "Harvest", "Total_supply", "Price_2", "Price_3", "Price_4")

dta$Yield <- seq(150, 190, length.out=100)
dta$Harvest <- dta$Yield*A0/1000
dta$Total_supply <- dta$Harvest + s0
dta$Price_2 <- (-(dta$Total_supply-a_2)/b_2)/100
dta$Price_3 <- (-(dta$Total_supply-a_3)/b_3)/100
dta$Price_4 <- (-(dta$Total_supply-a_4)/b_4)/100

dta$Price_2[dta$Price_2 < 0] <- 0
dta$Price_3[dta$Price_3 < 0] <- 0
dta$Price_4[dta$Price_4 < 0] <- 0

dta_demand <- melt(dta[,c("Price_2", "Price_3", "Price_4", "Total_supply")], id="Total_supply")
colnames(dta_demand) <- c("Total_supply", "var", "price")
dta_demand$var <- as.character(dta_demand$var)
dta_demand$var[dta_demand$var=="Price_2"] <- "Elasticity of demand = -0.2"
dta_demand$var[dta_demand$var=="Price_3"] <- "Elasticity of demand = -0.3"
dta_demand$var[dta_demand$var=="Price_4"] <- "Elasticity of demand = -0.4"

demand_plot <- ggplot(data = dta_demand, aes(x = Total_supply, y = price, color=var)) + geom_line() + scale_color_viridis(discrete = TRUE) + ylab("$ per bushel") + xlab("Total supply (billion bushel)") + theme_bw()  + mytheme + theme(legend.position=c(0.75,0.75), legend.background = element_blank(), axis.line = element_line(size = 0.5)) 

#ggsave(demand_plot, file="2015_demand.png", width=6, height=4)
#rm(demand_plot)

#plot in function of yield
dta$yield <- 1000*(dta$Total_supply - s0)/A0

dta_demand <- melt(dta[,c("Price_2", "Price_3", "Price_4", "yield")], id="yield")
colnames(dta_demand) <- c("yield", "var", "price")
dta_demand$var <- as.character(dta_demand$var)
dta_demand$var[dta_demand$var=="Price_2"] <- "Elasticity of demand = -0.2"
dta_demand$var[dta_demand$var=="Price_3"] <- "Elasticity of demand = -0.3"
dta_demand$var[dta_demand$var=="Price_4"] <- "Elasticity of demand = -0.4"

demand_plot2 <- ggplot() + geom_line(data = dta_demand, aes(x = yield, y = price, color=var)) + scale_color_viridis(discrete = TRUE) + scale_x_continuous(breaks=seq(150, 190, by=5), labels=seq(150, 190, by=5))  + ylab("$ per bushel") + xlab("Yield (bushels per acre)") + ylim(2.25, 5.5) + theme_bw()  + mytheme + theme(legend.position=c(0.75,0.75), legend.background = element_blank(), axis.line = element_line(size = 0.5))

#ggsave(demand_plot2, file="2015_corn_yield.png", width=6, height=4)
#rm(demand_plot2)


##############################
######## Corn yield ##########
##############################
#Download corn yield data from https://data.ers.usda.gov/FEED-GRAINS-custom-query.aspx
corn <- read_excel("Corn yield.xlsx", skip = 1) %>%
  dplyr::select(Year, Amount) %>%
  dplyr::rename(Date = Year, Value = Amount) %>%
  mutate(Date = as.Date(paste(Date, "01", "01", sep = "-"), "%Y-%d-%m"))

corn_yield <- ggplot(data = corn, aes(x = Date, y = Value)) + 
  geom_point(size = 0.25) + 
  theme_bw()+ 
  mytheme + ylab("Bushels per acre") + 
  xlab("") + 
  scale_x_date(breaks = seq(as.Date("1870/1/1"), as.Date("2020/1/1"), "10 years"), 
               labels = date_format("%Y")) + 
  scale_y_continuous(breaks = seq(25, 175, by = 25)) + 
  theme(axis.text.x  = element_text(size=5),
        axis.line = element_line(size = 0.5))
  
#ggsave(corn_yield, file = "Corn_yield.png", width = 6, height = 4, dpi=600)

corn_yield2 <- ggplot(data = corn, aes(x = Date, y = Value)) + 
  geom_point(size = 0.25) + 
  stat_smooth(method = "loess", se=FALSE, size=1, fullrange=T) + 
  theme_bw()+ 
  mytheme + ylab("Bushels per acre") + 
  xlab("") + 
  scale_x_date(breaks = seq(as.Date("1870/1/1"), as.Date("2020/1/1"), "10 years"), 
               labels = date_format("%Y")) + 
  scale_y_continuous(breaks = seq(25, 175, by = 25)) + 
  theme(axis.text.x  = element_text(size=5),
        axis.line = element_line(size = 0.5))
  
#ggsave(corn_yield2, file = "Corn_yield2.png", width = 6, height = 4, dpi=600)


### loess
corn$yield_hat <- predict(loess(corn$Value ~ as.numeric(corn$Date)))
corn$diff_yield <- corn$Value - corn$yield_hat
corn$percent_yield <- corn$diff_yield/corn$yield_hat
corn$diff_sign <- ifelse(corn$diff_yield <= 0, "Negative", "Positive")

corn_percent_plot <- ggplot(data = corn[corn$Date >= "1950-01-01",], aes(x = Date, y = percent_yield, fill = diff_sign)) + geom_bar(stat = "identity", color = "white") + scale_fill_viridis(discrete = TRUE) + geom_hline(aes(yintercept = 0)) + theme_bw()+ mytheme + ylab("Percentage yield shock") + xlab("") + scale_x_date(breaks= as.Date(paste(seq(1950,2020, by=10), "-12-31", sep="")), labels = seq(1950,2020, by=10)) + theme(legend.position = c(0.85, 0.85), axis.line = element_line(size = 0.5))

#ggsave(corn_percent_plot, file = "Corn_percent_yield.png", width = 6, height = 4, dpi=600)
#rm(yield)

#Predicted yield
predict(loess(Value ~ as.numeric(Date), corn, control = loess.control(surface = "direct")), data.frame(Date = as.Date("2017-12-01")))


######################################################################
#Predicting prices

#Yield = 165 bushels per acre

Total_1 <- A0*170/1000 + s0

#eta=-.2
-(Total_1-a_2)/b_2
#eta=-.3
-(Total_1-a_3)/b_3
#eta=-.4
-(Total_1-a_4)/b_4

#Yield = 150 bushels per acre

Total_2 <- A0*150/1000 + s0

#eta=-.2
-(Total_2-a_2)/b_2
#eta=-.3
-(Total_2-a_3)/b_3
#eta=-.4
-(Total_2-a_4)/b_4

#Yield = 175 bushels per acre

Total_2 <- A0*175/1000 + s0

#eta=-.2
-(Total_2-a_2)/b_2
#eta=-.3
-(Total_2-a_3)/b_3
#eta=-.4
-(Total_2-a_4)/b_4

rm(P0, Q0, Total_1, Total_2, Total_2)

#For 2017
Total_2018 <- 175*83/1000 + 2.335

-(Total_2018-a_3)/b_3

##################################
####### Retrospectively ##########
##################################

#download.file("http://usda.mannlib.cornell.edu/usda/waob/wasde//2010s/2017/wasde-09-12-2017.xls", paste(getwd(), "wasde-09-12-2017.xls", sep="/"), mode="wb", quiet = TRUE)

wasde <- read_excel(paste(getwd(), "wasde-09-12-2017.xls", sep="/"), sheet = "Page 12")

row_1 <- which(wasde[,1] == "CORN") + 1
row_n <- nrow(wasde) - 1

wasde <- wasde[row_1:row_n,]

colnames(wasde) <- c("var", "y2015", "y2016", "aug", "sept")
wasde <- wasde %>% 
  dplyr::filter(!is.na(var)) %>% 
  mutate(var = str_trim(var, side = "both")) %>%
  as.data.frame()

#Calibration of demand
dta17 <- data.frame(array(NA, c(1,3)))
colnames(dta17) <- c("supply", "yield", "futures")
dta17$supply <- as.numeric(wasde[wasde$var == "Supply, Total", "sept"])
A1 <- as.numeric(str_sub(wasde[wasde$var == "Area Harvested", "sept"], 1, 4))
s1 <- as.numeric(wasde[wasde$var == "Beginning Stocks", "sept"])/1000

dta17$yield <- as.numeric(wasde[wasde$var == "Yield per Harvested Acre", "sept"])
dta17$futures <- price %>% dplyr::filter(Date >= "2017-09-12") %>% summarize(price = mean(Settle)/100) %>% as.numeric()
dta17$month <- "September 2017"


demand_retro <- demand_plot2 + geom_point(data = dta17[,c("month", "futures", "yield")], aes(x = yield, y = futures)) + 
  annotate("text", x = dta17$yield-5, y=c(2.9,3.1), label = c("169.9 bu/acre", "$3.51/bu"), hjust = 0, size = 3)

#ggsave(demand_retro, file="corn_pred_retro.png", width=6, height=4)


#######################
### Corn price plot ###
#######################

price  <- price %>%
  mutate(Date = as.Date(Date)) %>%
  dplyr::select(Date, Settle) %>%
  dplyr::filter(Date >= "2017-03-01")


corn_plot <- ggplot(data = price, aes(x = Date, y = Settle)) + 
  geom_line() + 
  ylab("Price ($/bu)") + 
  scale_x_date(date_breaks="1 month", date_minor_breaks="1 month", labels=date_format("%b")) + 
  theme_bw()+ 
  mytheme + 
  theme(axis.title.x = element_blank(), axis.line = element_line(color='black'))

########################################
####### Soybean to corn ratio ##########
########################################

#download.file("http://farmdoc.illinois.edu/manage/uspricehistory/excel/USPriceHistory.xls", "Monthly prices.xls", mode="wb", quiet = TRUE)

corn <- read_excel("Monthly prices.xls", sheet = "US-Corn-Time Series", skip = 2, col_names = FALSE)

corn  <- corn %>% 
  dplyr::rename(date = X__1, corn = X__2) %>% 
  mutate(corn = as.numeric(corn)) %>%
  dplyr::filter(!is.na(corn)) %>%
  mutate(date = as.Date(as.numeric(date), origin="1899-12-30")) %>%
  dplyr::filter(!is.na(date))

soybean <- read_excel("Monthly prices.xls", sheet = "US-Soybeans-Time Series", skip = 2, col_names = FALSE)

soybean <- soybean %>% 
  dplyr::rename(date = X__1, soybean = X__2) %>% 
  mutate(soybean = as.numeric(soybean)) %>%
  dplyr::filter(!is.na(soybean)) %>%
  mutate(date = as.Date(as.numeric(date), origin="1899-12-30")) %>%
  dplyr::filter(!is.na(date))

grain <- left_join(corn, soybean)
grain <- grain %>% mutate(ratio = soybean/corn)

ratio_plot <- ggplot(data = grain, aes(x = date, y = ratio)) +geom_line(color="blue", size=1) + ylab("Soybean-to-corn price ratio") + xlab("") + theme_bw()  + mytheme  

#ggsave(ratio_plot, file="corn_soybeans_ratio.png", width=6, height=4)

summary(grain$ratio)
summary(grain$ratio[grain$date>"2000-01-01"])

#################################
######## Soybean yield ##########
##################################

#Download data for soybean yield from https://www.ers.usda.gov/webdocs/DataFiles/52218/Table2.xls
soybean <- read_excel("Soybean yield.xlsx", skip = 1) %>%
  dplyr::filter(!is.na(Year)) %>%
  dplyr::select(Year, Yield) %>%
  mutate(Year = as.Date(paste(Year, "01", "01", sep = "-"), "%Y-%d-%m"), Value = as.numeric(Yield)) %>%
  dplyr::rename(Date = Year) %>%
  dplyr::select(Date, Value)

yield_plot <- ggplot(data = soybean, aes(x = Date, y = Value)) + geom_point() + theme_bw()+ mytheme + ylab("Bushels per acre") + xlab("") + scale_x_date(breaks = seq(as.Date("1870/1/1"), as.Date("2020/1/1"), "10 years"), labels = date_format("%Y")) + scale_y_continuous(breaks = seq(25, 60, by = 5))

#ggsave(yield_plot, file="soybean_yield.png", width=6, height=4)
#rm(yield_plot)

yield_plot2 <- ggplot(data = soybean, aes(x = Date, y = Value)) + geom_point() + stat_smooth(method = "loess", se=FALSE) + theme_bw()+ mytheme + ylab("Bushels per acre") + xlab("") + scale_x_date(breaks = seq(as.Date("1870/1/1"), as.Date("2020/1/1"), "10 years"), labels = date_format("%Y")) + scale_y_continuous(breaks = seq(25, 60, by = 5))

#ggsave(yield_plot2, file="soybean_yield2.png", width=6, height=4)
#rm(yield_plot2)

#yield <- yield[yield$date>"1949-12-31",]

yield_plot3 <- ggplot(data = soybean, aes(x = Date, y = Value)) + geom_point(size=1.25) + stat_smooth(method=loess,se=FALSE, size=1, fullrange=T) + ylab("Bushel per acre") + scale_y_continuous(breaks= seq(10,50, by=5), labels = seq(10,50, by=5)) + scale_x_date(breaks= as.Date(paste(seq(1920,2020, by=10), "-12-31", sep="")), labels = seq(1920,2020, by=10)) + xlab("") + theme_bw()  + mytheme  

#ggsave(yield_plot3, file="soybean_yield3.png", width=6, height=4)
#rm(yield_plot3)

#rm(yield)