############################################################
rm(list = ls())

#Load some packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(grid)
library(gridExtra)
library(Quandl)
library(lubridate)
library(scales)
library(xlsx)
library(Quandl)

suppressWarnings(try(source("D:/Box Sync/Graph parameters.R"), silent = TRUE))

suppressWarnings(try(setwd("D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Slides/"), silent = TRUE))
suppressWarnings(try(setwd("C:/Users/pouliot.IASTATE/Documents/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Slides/"), silent = TRUE))

#call <- read_excel("D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Assignments/Options premia.xlsx", sheet = "Call option")
#put <- read_excel("D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Assignments/Options premia.xlsx", sheet = "Put option")
call <- read_excel("D:/Dropbox/Option prices/Options premium.xlsx", sheet = "Call option")
put <- read_excel("D:/Dropbox/Option prices/Options premium.xlsx", sheet = "Put option")


call <- call %>% mutate(Strike_name = as.character(Strike), 
                        Strike = as.numeric(Strike)/100,
                        date = as.Date(date))
put <- put %>% mutate(Strike_name = as.character(Strike), 
                      Strike = as.numeric(Strike)/100,
                      date = as.Date(date))

#Count the number of observations per strike price
#call$count <- 0
#put$count <- 0

for (j in unique(call$Strike_name)){
  call$count[call$Strike_name == j] <- sum(as.numeric(call$Strike_name == j))
}

for (j in unique(put$Strike_name)){
  put$count[put$Strike_name == j] <- sum(as.numeric(put$Strike_name == j))
}


call <- call[call$count %in% sort(unique(call$count), decreasing = TRUE)[1:3],]
put <- put[put$count %in% sort(unique(put$count), decreasing = TRUE)[1:3],]


### Futures contract

#Authentification
#Quandl.api_key("2xcmYFJoJKuEDwawuV7J")

#dta <- Quandl("CME/CZ2017", type="raw")
#dta <- Quandl("CME/SX2016", type="raw")
#dta <- Quandl("CME/CZ2015", type="raw")
#dta <- tbl_df(dta)
#dta <- dta %>% mutate(Settle = Settle/100) %>% dplyr::select(Date, Settle) %>% rename(date = Date)
#dta <- read.xlsx("CZ2015.xlsx", 1)

#Merge data
#call <- call %>% mutate(date = as.Date(date)) %>% left_join(dta)
#put <- put %>% mutate(date = as.Date(date)) %>% left_join(dta)

#Keep options that are in the money
#call <- call %>% dplyr::filter(!is.na(Settle)) #%>% dplyr::filter(Strike < mean(dta$Settle[dta$date >= min(call$date)]) + 0.5, Strike > mean(dta$Settle[dta$date >= min(call$date)]) - 0.3)

#put <- put %>% dplyr::filter(!is.na(Settle)) #%>% dplyr::filter(Strike > mean(dta$Settle[dta$date >= min(put$date)]) - 0.3, Strike < mean(dta$Settle[dta$date >= min(put$date)]) + 0.1)

#Dollars per bushel
call <- call %>% mutate(futures = futures/100)
put <- put %>% mutate(futures = futures/100)

### Intrinsic value
call <- call %>% mutate(intrinsic_value = ifelse(futures>Strike, futures-Strike,0))
put <- put  %>% mutate(intrinsic_value = ifelse(Strike>futures, Strike-futures,0))

### Time value
call <- call %>% mutate(time_value = Close - intrinsic_value)
put <- put %>% mutate(time_value = Close - intrinsic_value)

#Price of corn
price_plot <- ggplot(data = call, aes(x = date, y = futures)) + 
  geom_line() + 
  geom_point() + 
  theme_bw()+ 
  mytheme + 
  ylab("Dollars per bushel") + 
  theme(axis.title.x = element_blank(), legend.position="none", axis.line = element_line(color='black'))

#ggsave(price_plot, file = "figure_corn_futures.png", width = 6, height = 4, dpi=600)

#Plot of option premium
call_plot <- ggplot(data = call, aes(x = date, y = Close, color = Strike_name)) + 
  geom_line(size = 0.5) + 
  geom_point(size = 0.5) + 
  facet_grid(. ~ Strike_name) + 
  theme_bw()+ 
  mytheme + 
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b")) + ylab("Dollars per bushel") + 
  theme(axis.title.x = element_blank(), legend.position="none", 
        axis.text.x = element_text(size = 5, colour = 'black', family = "Times New Roman"),
        axis.line = element_line(color='black'))

#ggsave(call_plot, file = "call_premium.png", width = 6, height = 4, dpi=600)
#ggsave(call_plot, file = "D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2015/Slides/call_premium.png", width = 6, height = 4, dpi=600)

put_plot <- ggplot(data = put, aes(x = date, y = Close, color = Strike_name)) + 
  geom_line(size = 0.5) + geom_point(size = 0.5) + 
  facet_grid(. ~ Strike_name) + 
  theme_bw()+ 
  mytheme + 
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b")) + 
  ylab("Dollars per bushel") + 
  theme(axis.title.x = element_blank(), 
        legend.position="none", 
        axis.text.x = element_text(size = 5,colour = 'black', family = "Times New Roman"),
        axis.line = element_line(color='black'))

#ggsave(put_plot, file = "put_premium.png", width = 6, height = 4, dpi=600)
#ggsave(put_plot, file = "D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2015/Slides/put_premium.png", width = 6, height = 4, dpi=600)

### Intrinsic value
call_plot_intrinsic <- ggplot(data = call, aes(x = date, y = intrinsic_value, color = Strike_name)) +
  geom_line(size = 0.5) + geom_point(size = 0.5) + 
  facet_grid(. ~ Strike_name) + 
  theme_bw()+ 
  mytheme + 
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b")) + 
  ylab("Dollars per bushel") + 
  theme(axis.title.x = element_blank(), 
        legend.position="none", 
        axis.text.x = element_text(size = 5, colour = 'black', family = "Times New Roman"),
        axis.line = element_line(color='black'))
  
#ggsave(call_plot, file = "call_intrinsic_value.png", width = 6, height = 4, dpi=600)
#ggsave(call_plot, file = "D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2015/Slides/call_intrinsic_value.png", width = 6, height = 4, dpi=600)

put_plot_intrinsic <- ggplot(data = put, aes(x = date, y = intrinsic_value, color = Strike_name)) + 
  geom_line(size = 0.5) + 
  geom_point(size = 0.5) + 
  facet_grid(. ~ Strike_name) + 
  theme_bw()+ 
  mytheme + 
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b")) + 
  ylab("Dollars per bushel") + 
  theme(axis.title.x = element_blank(), legend.position="none", axis.text.x = element_text(size = 5, colour = 'black', family = "Times New Roman"),
        axis.line = element_line(color='black'))
  
#ggsave(put_plot, file = "put_intrinsic_value.png", width = 6, height = 4, dpi=600)
#ggsave(put_plot, file = "D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2015/Slides/put_intrinsic_value.png", width = 6, height = 4, dpi=600)

### Time value
call_plot_time <- ggplot(data = call, aes(x = date, y = time_value, color = Strike_name)) + 
  geom_line(size = 0.5) + 
  geom_point(size = 0.5) + 
  facet_grid(. ~ Strike_name) + 
  theme_bw()+ 
  mytheme + 
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b")) + ylab("Dollars per bushel") + 
  theme(axis.title.x = element_blank(), legend.position="none", axis.text.x = element_text(size = 5, colour = 'black', family = "Times New Roman"),
        axis.line = element_line(color='black'))

#ggsave(call_plot, file = "call_time_value.png", width = 6, height = 4, dpi=600)
#ggsave(call_plot, file = "D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2015/Slides/call_time_value.png", width = 6, height = 4, dpi=600)

put_plot_time <- ggplot(data = put, aes(x = date, y = time_value, color = Strike_name)) + 
  geom_line(size = 0.5) + 
  geom_point(size = 0.5) + 
  facet_grid(. ~ Strike_name) + 
  theme_bw()+ 
  mytheme + 
  scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%b")) + ylab("Dollars per bushel") + 
  theme(axis.title.x = element_blank(), legend.position="none", axis.text.x = element_text(size = 5, colour = 'black', family = "Times New Roman"),
        axis.line = element_line(color='black'))

#ggsave(put_plot, file = "put_time_value.png", width = 6, height = 4, dpi=600)
#ggsave(put_plot, file = "D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2015/Slides/put_time_value.png", width = 6, height = 4, dpi=600)







