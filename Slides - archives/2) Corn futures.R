############################################################
rm(list = ls())

#Load some packages
library(pacman)
pacman::p_load(dplyr)
pacman::p_load(magrittr)
pacman::p_load(ggplot2)
suppressMessages(pacman::p_load(reshape2))
suppressMessages(pacman::p_load(gridExtra))
pacman::p_load(Quandl)
suppressMessages(pacman::p_load(lubridate))
pacman::p_load(scales)
pacman::p_load(viridis)

suppressWarnings(try(source("D:/Box Sync/Graph parameters.R"), silent = TRUE))
suppressWarnings(try(source("C:/Users/pouliot.IASTATE/Documents/Box Sync/Graph parameters.R"), silent = TRUE))
suppressWarnings(try(source("C:/Users/pouliot.IASTATE/Box Sync/Graph parameters.R"), silent = TRUE))

#Authentification
Quandl.api_key("2xcmYFJoJKuEDwawuV7J")

###############
##### Corn ####
###############

dta <- Quandl("CME/CZ1990", type="raw")
dta <- tbl_df(dta)

dta <- dta %>% mutate(contract = "1990",
              Settle = Settle/100,
              year = as.numeric(substr(Date, 1, 4)),
              week = 1:nrow(dta),
              day = day(Date),
              month = month(Date),
              date = paste("2008", month(Date), sep="-") %>% paste(day(Date), sep="-") %>% as.Date("%Y-%m-%d"))
dta <- dta %>% dplyr::filter(year==1990)
dta <- dta %>% dplyr::select(Date, Settle, contract, date)


for (i in 1991:2017){
  dta0 <- Quandl(paste("CME/CZ", i, sep=""), type="raw")
  dta0 <- tbl_df(dta0)
  dta0 <- mutate(dta0,
                contract = as.character(i),
                Settle = Settle/100,
                year = as.numeric(substr(Date, 1, 4)),
                week = 1:nrow(dta0),
                day = day(Date),
                month = month(Date),
                date = paste("2008", month(Date), sep="-") %>% paste(day(Date), sep="-") %>% as.Date("%Y-%m-%d"))
  dta0 <- dta0 %>% dplyr::filter(year==i)
  dta0 <- dta0 %>% dplyr::select(Date, Settle, contract, date)
  
  dta <- rbind(dta, dta0)
  
}

rm(dta0, i)

dta2 <- dta %>% dplyr::filter(contract=="2017" | contract=="2016" | contract=="2015" | contract=="2014" | contract=="2013")

windowsFonts(
  A=windowsFont("Arial Black"),
  B=windowsFont("Bookman Old Style"),
  C=windowsFont("Comic Sans MS"),
  D=windowsFont("Times New Roman")
)


corn_plot <- ggplot(data = dta, aes(x = date, y = Settle, group = contract)) + geom_line( colour = "gray70") + geom_line(data=dta2, aes(y = Settle, colour = contract), size=1) + scale_color_viridis(discrete = TRUE) + ylab("Price ($/bu)") +  scale_x_date(date_breaks="1 month", date_minor_breaks="1 month", labels=date_format("%b")) + scale_y_continuous(breaks = 2:8, minor_breaks = seq(2, 8, by = 0.5)) + theme_bw()+ mytheme + theme(axis.title.x = element_blank(), axis.line = element_line(color='black'))

corn_plot

#ggsave(corn_plot, file = "D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Slides/price_corn.png", width = 6, height = 4, dpi=600)


#Continous contract

dta <- Quandl("CHRIS/CME_C1")

dta <- tbl_df(dta)

dta <- dta %>% mutate(Settle = Settle/100,
              date = paste(as.numeric(substr(Date, 1, 4)), month(Date), sep="-") %>% paste(day(Date), sep="-") %>% as.Date("%Y-%m-%d"))
dta <- dta %>% dplyr::select(Date, Settle, date)

#Select date
dta <- dta %>% dplyr::filter(Date >= "2014-01-01")

corn_c_plot <- ggplot(data = dta, aes(x = date, y = Settle)) + geom_point( colour = "black", size = 1) + geom_smooth(method=lm, se=FALSE,linetype="dashed") + ylab("Price ($/bu)") +  scale_x_date(date_breaks="1 year", date_minor_breaks="1 month", labels=date_format("%Y")) + labs(title="Daily price of corn", size=5) + annotate("text", x = as.Date('2014-06-01','%Y-%m-%d'), y = 3.1, label = "The dashed line is a linear trend", size=4, family = "D") + theme_bw() + mytheme 

corn_c_plot

#ggsave(corn_c_plot, file = "D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Slides/price_corn_continous.png", width = 6, height = 4, dpi=600)





###################
##### Soybeans ####
###################


dta <- Quandl("CME/SX1990", type="raw")
dta <- tbl_df(dta)

dta <- mutate(dta,
              contract = "1990",
              Settle = Settle/100,
              year = as.numeric(substr(Date, 1, 4)),
              week = 1:nrow(dta),
              day = day(Date),
              month = month(Date),
              date = paste("2008", month(Date), sep="-") %>% paste(day(Date), sep="-") %>% as.Date("%Y-%m-%d"))
dta <- dta %>% dplyr::filter(year==1990)
dta <- dta %>% dplyr::select(Date, Settle, contract, date)


for (i in 1991:2017){
  dta0 <- Quandl(paste("CME/SX", i, sep=""), type="raw")
  dta0 <- tbl_df(dta0)
  dta0 <- mutate(dta0,
                 contract = as.character(i),
                 Settle = Settle/100,
                 year = as.numeric(substr(Date, 1, 4)),
                 week = 1:nrow(dta0),
                 day = day(Date),
                 month = month(Date),
                 date = paste("2008", month(Date), sep="-") %>% paste(day(Date), sep="-") %>% as.Date("%Y-%m-%d"))
  dta0 <- dta0 %>% dplyr::filter(year==i)
  dta0 <- dta0 %>% dplyr::select(Date, Settle, contract, date)
  
  dta <- rbind(dta, dta0)
  
}

rm(dta0, i)

dta2 <- dta %>% dplyr::filter(contract=="2017" | contract=="2016" | contract=="2015" | contract=="2014" | contract=="2013")

soy_plot <- ggplot(data = dta, aes(x = date, y = Settle, group = contract)) + geom_line( colour = "gray70") + geom_line(data=dta2, aes(y = Settle, colour = contract), size=1) + scale_color_viridis(discrete = TRUE) + ylab("Price ($/bu)") +  scale_x_date(date_breaks="1 month", date_minor_breaks="1 month", labels=date_format("%b")) + scale_y_continuous(breaks = seq(4, 17, by = 2), minor_breaks = seq(4, 17, by = 1)) + theme_bw()+ mytheme + theme(axis.title.x = element_blank(), axis.line = element_line(color='black'))

soy_plot


#ggsave(soy_plot, file = "D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Slides/price_soybeans.png", width = 6, height = 4, dpi=600)








