rm(list = ls())

library(data.table)
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
setwd("D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Slides/")

#Data from http://ethanolrfa.org/pages/monthly-fuel-ethanol-production-demand
#Data are from http://www.ethanolrfa.org/resources/industry/statistics/

dta0 <- read_excel("9) Ethanol data - old.xlsx", sheet = 1) %>% filter(year < 2015)

#download.file("http://ethanolrfa.org/wp-content/uploads/2016/09/Monthly-Ethanol-Production.xlsx", "09) Ethanol data.xlsx", mode="wb", quiet = TRUE)

dta1 <- read_excel("9) Ethanol data.xlsx", sheet = "2015", skip = 3, col_names = FALSE) %>% 
  dplyr::select(X__1, X__4, X__5, X__7, X__9, X__10, X__12, X__14, X__16, X__18) %>% "["(.,1:12,) %>% 
  mutate(year = 2015, month = lubridate::month(ymd(080101) + months(0:11), label = TRUE) %>% as.character()) %>% dplyr::select(-(X__4:X__18), year, month, everything(), -X__1)
colnames(dta1) <- colnames(dta0)

dta2 <- read_excel("9) Ethanol data.xlsx", sheet = "2016", skip = 3, col_names = FALSE) %>% 
  dplyr::select(X__1, X__4, X__5, X__7, X__9, X__10, X__12, X__14, X__16, X__18) %>% "["(.,1:12,) %>% 
  mutate(year = 2016, month = lubridate::month(ymd(080101) + months(0:11), label = TRUE) %>% as.character()) %>% dplyr::select(-(X__4:X__18), year, month, everything(), -X__1)
colnames(dta2) <- colnames(dta0)

dta3 <- read_excel("9) Ethanol data.xlsx", sheet = "2017", skip = 3, col_names = FALSE) %>% 
  dplyr::select(X__1, X__4, X__5, X__7, X__9, X__10, X__12, X__14, X__16, X__18) %>% "["(.,1:6,) %>% 
  mutate(year = 2017, month = lubridate::month(ymd(080101) + months(0:5), label = TRUE) %>% as.character()) %>% dplyr::select(-(X__4:X__18), year, month, everything(), -X__1)
colnames(dta3) <- colnames(dta0)


dta <- rbind(dta0, dta1) %>% rbind(dta2) %>% rbind(dta3)
rm(dta0, dta1, dta2)

###########################################
### Production, consumption and mandate ###
###########################################

dta_agg <- dta %>% group_by(year) %>% summarize(production = sum(production), consumption = sum(Consumption))

dta_agg$rfs <- c(4, 4.7, 9, 10.5, 12, 12.6, 13.2, 13.80, 13.61, 14.05, 14.50, 15.00)

dta_agg <-  dta_agg %>% mutate(production = production/1000000, consumption = consumption/1000000)

dta_agg2 <- dta_agg %>% gather(variable, volumes, -year) %>% mutate(variable = ifelse(variable == "rfs", "Mandated volumes", ifelse(variable == "production", "Production", "Consumption"))) %>% rename(Year = year)

rfs_plot <- ggplot(dta_agg2, aes(x=Year, y=volumes, fill=variable)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_viridis(discrete = TRUE) + 
  geom_text(aes(x=Year, y=volumes, label=round(volumes,1)), family="Times New Roman", size=2, position = position_dodge(width=1), vjust=-0.1) + 
  ylab("Volumes (billion gallons)") + 
  scale_x_continuous(breaks=seq(2006,2017), labels=seq(2006,2017)) + 
  theme_bw() + 
  mytheme + 
  theme(axis.title.x = element_blank(), axis.line = element_line())
  
#ggsave(rfs_plot, file = "rfs_plot.png", width = 6, height = 4, dpi=600)


#Gasoline consumption

#download.file("https://www.eia.gov/dnav/pet/xls/PET_CONS_PSUP_A_EPM0F_VPP_MBBL_M.xls", destfile = "PET_CONS_PSUP_A_EPM0F_VPP_MBBL_M.xls", mode="wb", quiet = TRUE)


eia <- read_excel("PET_CONS_PSUP_A_EPM0F_VPP_MBBL_M.xls", sheet = "Data 1", skip = 3, col_names = FALSE) %>%
  dplyr::select(X__1, X__2) %>%
  rename(date = X__1, cons = X__2) %>%
  mutate(date = as.Date(date), Year = year(date)) %>%
  group_by(Year) %>%
  summarize(cons = sum(cons)) %>%
  mutate(cons = 42*cons, volumes = 0.1*cons/1000000, variable = "Blend wall") %>%
  dplyr::select(Year, volumes, variable) %>%
  dplyr::filter(Year >= 2006)
  
wall <- dta_agg2 %>%
  dplyr::filter(variable == "Mandated volumes") %>%
  rbind(eia)

wall_plot <- ggplot(wall, aes(x=Year, y=volumes, fill=variable)) + 
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_viridis(discrete = TRUE) + 
  geom_text(aes(x=Year, y=volumes, label=round(volumes,1)), family="Times New Roman", size=2, position = position_dodge(width=1), vjust=-0.1) + 
  ylab("Volumes (billion gallons)") + 
  scale_x_continuous(breaks=seq(2006,2017), labels=seq(2006,2017)) + 
  theme_bw() + 
  mytheme + 
  theme(axis.title.x = element_blank(), axis.line = element_line())


##############
### Stocks ###
##############

dta_stk <- dta %>% 
  dplyr::select(year, month, stocks, `days stocks`) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), "%Y-%b-%d"), stocks = stocks/1000)
  
  
stk1_plot <- ggplot(dta_stk, aes(x=date, y=stocks)) + geom_line() + ylab("Stocks (Million bls)") + scale_x_date(date_breaks= "1 year", date_labels = "%y-%b") + theme_bw() + mytheme + theme(axis.title.x = element_blank())

stk2_plot <- ggplot(dta_stk, aes(x=date, y=`days stocks`)) + geom_line() + ylab("Days in reserve") + scale_x_date(date_breaks= "1 year", date_labels = "%y-%b") + theme_bw() + mytheme + theme(axis.title.x = element_blank())




####################################################################
############################# Data #################################
####################################################################

rm(list = ls()[!(ls() %in% c("rfs_plot", "wall_plot", "mytheme"))])

library(Quandl)
library(data.table)
library(reshape2)
library(ggplot2)
library(grid)#for arrow in graph
library(gridExtra)
library(magrittr)
library(XML)
library(stringr)

setwd("D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Slides/")

#Corn
corn <- Quandl("CHRIS/CME_C1", trim_start="1959-07-01") %>%
  mutate(Date = as.Date(Date), Settle = Settle/100) %>%
  dplyr::select(Date, Settle)

#Ethanol
#Ethanol wholesale prices are from http://www.neo.ne.gov/statshtml/66.html
#Ethanol rack prices, FOB Omaha, Nebraska

Quandl.api_key("2xcmYFJoJKuEDwawuV7J")

eth_futures <- Quandl("CHRIS/CME_EH1", trim_start="2011-10-03")
eth_futures$Date <- as.Date(eth_futures$Date)

#From http://www.neo.ne.gov/statshtml/66.html

url <- "http://www.neo.ne.gov/statshtml/66.html"

tables <- readHTMLTable(url)

eth_whole <- tables[[1]]
eth_whole$V14 <- NULL
colnames(eth_whole) <- c("year", as.character(1:12))

eth_whole <- gather(eth_whole, month, price, 2:13) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")), 
         price = str_replace(price, "\\$",""),
         month = month(date),
         year = year(date)) 

ethanol <- corn
ethanol$Settle <- NULL
ethanol <- left_join(ethanol, eth_futures[,c("Date", "Settle")]) %>%
  mutate(month = month(Date), year = year(Date))

ethanol <- left_join(ethanol, eth_whole) %>% dplyr::select(-date, -month, -year)
  

#Gasoline
gas <- Quandl("CHRIS/CME_RB1", trim_start="2005-10-03") %>% 
  mutate(date = as.Date(Date)) %>% 
  dplyr::select(date, Settle) %>%
  rename(price = Settle) %>%
  mutate(var = "Gasoline ($/gal)")
#gas <- gas[gas$date>=min(gas$date),]

corn <- corn %>% rename(date = Date, price = Settle) %>% 
  mutate(var = "Corn ($/bu)") %>%
  dplyr::filter(date>=min(gas$date))

ethanol_futures <- ethanol[, c("Date", "Settle")]
colnames(ethanol_futures) <- c("date", "price")
ethanol_futures$var <- "Futures"

#ethanol_whole <- ethanol[, c("Date", "Price")]
#colnames(ethanol_whole) <- c("date", "price")
#ethanol_whole$var <- "Ethanol wholesale ($/gal)"

eth_whole <- eth_whole[, c("date", "price")]
eth_whole$var <- "Ethanol ($/gal)"

dta <-  corn  %>% rbind(eth_whole) %>% rbind(gas)

dta <- dta[dta$date>="2005-10-03",]

library(scales)

dta <- dta %>% tbl_df %>% mutate(price = as.numeric(price))

price_plot <- ggplot(data = dta, aes(x = date, y = price)) + 
  geom_line() + 
  facet_grid(var~., scales = "free") + 
  ylab("") + 
  xlab("") + 
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) + 
  theme_bw() + 
  mytheme + 
  theme(axis.text.x = element_text(size = 6, colour = 'black', family = "Times New Roman"), axis.text.y = element_text(size = 6, colour = 'black', family = "Times New Roman"), strip.text.y = element_text(size = 6, colour = 'black', family = "Times New Roman"), axis.line = element_line())

#ggsave(price_plot, file = "corn_gas_eth.png", width = 6, height = 4, dpi=600)

#Plot for ethanol

eth_whole$var <- "Wholesale"

dta_ethanol <-  eth_whole %>% rbind(ethanol_futures)
dta_ethanol <- dta_ethanol[dta_ethanol$date>="2005-10-03",]

dta_ethanol <- dta_ethanol %>% tbl_df %>% mutate(price = as.numeric(price))

ethanol_plot <- ggplot(data = dta_ethanol, aes(x = date, y = price, color=var)) + geom_line(size=1) + ylab("") + xlab("") + theme_bw() + mytheme + theme(legend.position = c(0.90, 0.90))

#ggsave(ethanol_plot, file = "ethanol_prices.png", width = 6, height = 4, dpi=600)


#######################################################
############### Ethanol profitability #################
#######################################################

rm(list = ls()[!(str_detect(ls(), c("plot")) | str_detect(ls(), c("theme")))])

library(data.table)
library(reshape2)
library(ggplot2)
library(grid)#for arrow in graph
library(gridExtra)
library(scales)
library(readxl)
library(magrittr)
library(dplyr)

setwd("D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Slides/")

#http://www.extension.iastate.edu/agdm/energy/xls/agmrcethanolplantprices.xlsx
#dta <- read_excel("http://www.extension.iastate.edu/agdm/energy/xls/agmrcethanolplantprices.xlsx", sheet = 1)

#download.file("http://www.extension.iastate.edu/agdm/energy/xls/agmrcethanolplantprices.xlsx", destfile = "agmrcethanolplantprices.xlsx", mode="wb", quiet = TRUE)

dta <- read_excel("agmrcethanolplantprices.xlsx", sheet = 1, skip = 7, col_names = FALSE) %>% tbl_df

dta <- dta %>% dplyr::select(2,3,8,23) %>% 
  rename(Date = X__2, Corn = X__3, DDGS10 = X__8,Ethanol = X__23) %>%
  mutate(Date = as.Date(Date), Corn_lb = Corn/56, DDGS_lb  = DDGS10/2000) %>% filter(!is.na(Date))


dta_prices <- melt(dta[,c("Corn_lb", "DDGS_lb", "Date")], id="Date")
colnames(dta_prices) <- c("date", "var", "price")

dta_prices$var <- as.character(dta_prices$var)
dta_prices$var[dta_prices$var=="Corn_lb"] <- "Corn"
dta_prices$var[dta_prices$var=="DDGS_lb"] <- "DDGS"


plot_ddgs <- ggplot(data = dta_prices, aes(x = date, y = price, color=var)) + 
  geom_line() + 
  scale_color_viridis(discrete = TRUE) + 
  xlab("") + 
  ylab("Price ($/lb)")  + 
  theme_bw() + 
  mytheme + 
  theme(legend.position=c(0.12,0.85), legend.key = element_blank(), legend.background = element_blank(), axis.line = element_line())

#ggsave(price_plot, file = "corn_DDGS.png", width = 6, height = 4, dpi=600)

dta$month <- month(dta$Date)
dta$year <- year(dta$Date)

##################################
####### Natural gas prices #######
##################################

library(lubridate)

#http://www.eia.gov/dnav/ng/ng_pri_sum_dcu_sia_m.htm
#Measure in $ per thousand cubic feet

#download.file("http://www.eia.gov/dnav/ng/xls/NG_PRI_SUM_DCU_SIA_M.xls", destfile = "NG_PRI_SUM_DCU_SIA_M.xls", mode="wb", quiet = TRUE)

#dta_ngas  <- read.csv("8) Natural_gas.csv", header=TRUE)

dta_ngas <- read_excel("NG_PRI_SUM_DCU_SIA_M.xls", sheet = 2, skip = 3, col_names = FALSE) %>% tbl_df() %>% dplyr::select(1, 5) %>% rename(date = X__1, ngas = X__5) %>% mutate(date = as.Date(date))

#dta_ngas$ngas[dta_ngas$date=="2013-09-01"] <- 9.375

dta_ngas <- dta_ngas %>% mutate(month = month(date), year = year(date))
dta_ngas$date <- NULL

#Merge data
dta <- merge(dta, dta_ngas, by=c("month", "year"))

#Order by date
dta <- dta[order(dta$Date), ]
head(dta[,c("Ethanol", "Corn_lb", "DDGS_lb", "ngas")])


###########################################
####### Calculate revenu per gallon #######
###########################################

dta$revenue <- dta$Ethanol + (16.5/2.8)*dta$DDGS_lb

rev_plot <- ggplot(data = dta, aes(x = Date, y = revenue)) + 
  geom_area(fill="green", alpha=0.5, colour="black") + 
  xlab("") + 
  ylab("Revenue ($/gal)") + 
  scale_x_date(date_breaks=("year"), date_labels="%Y") + 
  theme_bw() + 
  mytheme

#ggsave(rev_plot, file = "rev_ethanol.png", width = 6, height = 4, dpi=600)

dta$rev_eth <- dta$Ethanol 
dta$rev_ddgs <- (16.5/2.8)*dta$DDGS_lb

dta_rev <- melt(dta[complete.cases(dta[,c("rev_ddgs", "rev_eth", "Date")]),c("rev_ddgs", "rev_eth", "Date")], id="Date")
colnames(dta_rev) <- c("date", "revenue", "value")
dta_rev$revenue <- as.character(dta_rev$revenue)
dta_rev$revenue[dta_rev$revenue=="rev_ddgs"] <- "DDGS"
dta_rev$revenue[dta_rev$revenue=="rev_eth"] <- "Ethanol"

rev_plot2 <- ggplot(data = dta_rev, aes(x = date, y = value, fill=revenue)) + 
  geom_area(position="stack") + 
  geom_area(position="stack", colour="black", show.legend=FALSE, alpha=0.5) + 
  xlab("") + 
  ylab("Revenue ($/gal)") + 
  scale_x_date(date_breaks=("year"), date_labels="%Y") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position=c(0.85,0.85), legend.key = element_blank(), legend.background = element_blank(), legend.text = element_text(size = 6, colour = 'black', family = "Times New Roman"), axis.line = element_line())

#ggsave(rev_plot2, file = "rev_ethanol2.png", width = 6, height = 4, dpi=600)

#########################################
####### Calculate cost per gallon #######
#########################################

chemicals <- 11.59/100
otherdirect <- 10.33/100

dta$varcost <- chemicals + otherdirect
dta$fixedcost <- 21.35/100

#2.8 gal per buchel
dta$cost_corn <- dta$Corn/2.8

#30 cubic feet/gal
dta$cost_ngas <- 30*(dta$ngas/1000)

#Total cost
dta$cost <- dta$cost_ngas + dta$cost_corn + dta$fixedcost + dta$varcost
dta$total_varcost <- dta$cost_ngas + dta$cost_corn + dta$varcost

dta_cost <- melt(dta[complete.cases(dta[,c("fixedcost", "varcost", "cost_ngas", "cost_corn", "Date")]),c("fixedcost", "varcost", "cost_ngas", "cost_corn", "Date")], id="Date")
colnames(dta_cost) <- c("date", "cost", "value")
dta_cost$cost <- as.character(dta_cost$cost)
dta_cost$cost[dta_cost$cost=="fixedcost"] <- "Fixed cost"
dta_cost$cost[dta_cost$cost=="varcost"] <- "Chemical and other"
dta_cost$cost[dta_cost$cost=="cost_ngas"] <- "Natural gas"
dta_cost$cost[dta_cost$cost=="cost_corn"] <- "Corn"

cost_plot <- ggplot(data = dta_cost, aes(x = date, y = value, fill=cost)) + 
  geom_area(position="stack", alpha=0.25) + 
  geom_area(position="stack", colour="black", show.legend=FALSE, alpha=0.25) + 
  xlab("") + 
  ylab("Cost ($/gal)") + 
  scale_x_date(date_breaks=("year"), date_labels="%Y") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position=c(0.82,0.82), legend.direction = "vertical", legend.key = element_blank(), legend.background = element_blank(), legend.text = element_text(size = 6, colour = 'black', family = "Times New Roman"), axis.line = element_line())

#ggsave(cost_plot, file = "cost_ethanol.png", width = 6, height = 4, dpi=600)

cost_plot2 <- ggplot(data = dta[ ,c("Date", "cost")], aes(x = Date, y = cost)) + 
  geom_area(fill="red", alpha=0.5, colour="black") + xlab("") + 
  ylab("Cost ($/gal)") + 
  scale_x_date(date_breaks=("year"), date_labels="%Y") + 
  theme_bw() +
  mytheme

#ggsave(cost_plot2, file = "cost_ethanol2.png", width = 6, height = 4, dpi=600)


###########################################
####### Calculate profit per gallon #######
###########################################

dta$pi0 <- apply(dta[,c("revenue", "cost")],1,min)
dta$pi_pos <- dta$revenue - dta$pi0
dta$pi_neg <- dta$cost - dta$pi0

dta_pi <- melt(dta[complete.cases(dta[,c("pi0", "pi_pos", "pi_neg", "Date")]),c("pi0", "pi_pos", "pi_neg", "Date")], id="Date")

colnames(dta_pi) <- c("date", "profit", "value")
dta_pi$profit <- as.character(dta_pi$profit)

dta_pi$profit[dta_pi$profit=="pi0"] <- "Revenue covers cost"
dta_pi$profit[dta_pi$profit=="pi_pos"] <- "Gain"
dta_pi$profit[dta_pi$profit=="pi_neg"] <- "Loss"

pi_plot <- ggplot(data = dta_pi, aes(x = date, y = value, fill=profit)) + 
  geom_area(position="stack", alpha=0.25) + 
  geom_area(position="stack", colour="black", show.legend=FALSE, alpha=0.25) + 
  scale_fill_manual(values = c("green", "red", "blue")) + xlab("") + 
  ylab("Profit ($/gal)") + scale_x_date(date_breaks=("year"), date_labels="%Y") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position=c(0.30,0.95), legend.direction = "horizontal", legend.key = element_blank(), legend.background = element_blank(), axis.line = element_line()) 
    
#ggsave(pi_plot, file = "pi_ethanol.png", width = 6, height = 4, dpi=600)

##########################
dta$pi_neg <- -(dta$cost - dta$pi0)

dta_pi <- melt(dta[complete.cases(dta[,c("pi_pos", "pi_neg", "Date")]),c("pi_pos", "pi_neg", "Date")], id="Date")

colnames(dta_pi) <- c("date", "profit", "value")
dta_pi$profit <- as.character(dta_pi$profit)

dta_pi$profit[dta_pi$profit=="pi_pos"] <- "Gain"
dta_pi$profit[dta_pi$profit=="pi_neg"] <- "Loss"

pi_plot2 <- ggplot(data = dta_pi, aes(x = date, y = value, fill=profit)) + 
  geom_area(position="stack", alpha=0.25) + 
  geom_area(position="stack", colour="black", show.legend=FALSE, alpha=0.25) + 
  scale_fill_manual(values = c("green", "red")) + 
  xlab("") + 
  ylab("Profit ($/gal)") + 
  scale_y_continuous(breaks=c(-0.5,-0.25, 0, 0.25, 0.5, 0.75, 1.00, 1.25, 1.50), labels=c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.00, 1.25, 1.50)) + 
  scale_x_date(date_breaks=("year"), date_labels="%Y") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position=c(0.25,0.80), legend.direction = "horizontal", legend.key = element_blank(), legend.background = element_blank(), axis.line = element_line()) 

#ggsave(pi_plot2, file = "pi_ethanol2.png", width = 6, height = 4, dpi=600)

#pi_plot2 <- ggplot(data = dta_pi, aes(x = date, y = value, fill=profit)) + geom_area(position="stack", alpha=0.25) + geom_area(position="stack", colour="black", show_guide=FALSE, alpha=0.25) + scale_fill_manual(values = c("green", "red")) + xlab("") + ylab("Profit ($/gal)") + scale_y_continuous(breaks=c(-0.5,-0.25, 0, 0.25, 0.5, 0.75, 1.00, 1.25, 1.50), labels=c(-0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.00, 1.25, 1.50)) + scale_x_date(breaks=("year"), labels=date_format("%Y")) + labs(title="Profit") + theme_bw() + mytheme

############################

corn_plot <- ggplot(data = dta[!is.na(dta$pi_pos),c("Corn", "Date")], aes(x = Date, y = Corn)) + geom_line() + ylab("Corn ($/bu)") + scale_x_date(date_breaks=("year"), date_labels="%Y") + scale_y_continuous(breaks=seq(3,8), label=c("3.00", "4.00", "5.00", "6.00", "7.00", "8.00")) + xlab("") + labs(title="Corn price") + theme_bw() + mytheme

eth_plot <- ggplot(data = dta[!is.na(dta$pi_pos),c("Ethanol", "Date")], aes(x = Date, y = Ethanol)) + geom_line() + ylab("Ethanol ($/gal)") + scale_x_date(date_breaks=("year"), date_labels="%Y")  + scale_y_continuous(breaks=seq(1.5, 3, by=0.5), label=c("1.50", "2.00", "2.50", "3.00")) + xlab("") + labs(title="Ethanol price") + theme_bw() + mytheme

source("multiplot.r")

png(filename = "3_graphs.png", width = 7, height = 6, units = "in", res = 600)
multiplot(corn_plot, eth_plot, pi_plot2)
dev.off()



#######################
#### Price of RIN #####
#######################

rm(list = ls()[!(str_detect(ls(), c("plot")) | str_detect(ls(), c("theme")))])

load("D:/Box Sync/Research - Ethanol demand/RIN prices/D6 RIN price.RData")
rin$var <- "RIN ($/gal)"

colnames(rin) <- c("date", "value", "var")
max_date <- max(rin$date)

# #Corn
# corn <- Quandl("CHRIS/CME_C1", trim_start="1959-07-01", trim_end="2015-12-31") %>% tbl_df()
# 
# corn$Date <- as.Date(corn$Date)
# corn <- corn[, c("Date", "Settle")]
# corn$Settle <- corn$Settle/100
# 
# corn$var <- "Corn ($/bu)"
# 
# colnames(corn) <- c("date", "value", "var")
# 
# #Ethanol - futures
# 
# eth_futures <- Quandl("CHRIS/CME_EH1", trim_start="2011-10-03", trim_end="2015-12-31") %>% tbl_df()
# eth_futures$Date <- as.Date(eth_futures$Date)
# eth_futures <- eth_futures[, c("Date", "Settle")]
# 
# eth_futures$var <- "Ethanol ($/gal)"
# 
# colnames(eth_futures) <- c("date", "value", "var")
# 
# #Merge the data
# dta <- rin %>% rbind(corn) %>% rbind(eth_futures)
# 
# dta <- dta %>% filter(date > max(min(corn$date),min(eth_futures$date)), date < max_date)
# 
# #ggplot(data = dta, aes(x = date, y = value)) + geom_line() + facet_grid(var~., scales = "free") + ylab("") + xlab("") + scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) + theme_bw() + mytheme

rin <- rin %>% filter(date >= "2010-01-01")

rin_plot <- ggplot(data = rin, aes(x = date, y = value)) + geom_line() + ylab("$/gal") + scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) + theme_bw() + mytheme + theme(axis.title.x = element_blank(), axis.line = element_line())

#ggsave(p, file = "RIN_price.png", width = 6, height = 4, dpi=600)



