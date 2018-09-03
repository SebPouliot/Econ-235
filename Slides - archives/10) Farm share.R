############################################################
rm(list = ls())

#Load some packages
library(dplyr)
library(magrittr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate)
library(scales)
library(readr)
library(tidyr)
library(extrafont)
loadfonts() #Load the fonts
library(viridis)
library(readxl)

source("D:/Box Sync/Graph parameters.R")

setwd("D:/Box Sync/Teaching/Econ 235/Econ 235 - Fall 2017/Slides")

#download.file("https://www.ers.usda.gov/webdocs/DataFiles/48769/FoodDollarDataReal.xls?v=42704", destfile = "FoodDollarDataReal.xls", mode = "wb")
#https://www.ers.usda.gov/data-products/food-dollar-series/download-the-data/

dta = read_excel("FoodDollarDataReal.xls", sheet = 2)

dta_graph  <- dta %>% 
  dplyr::filter(CATEGORY_DESC == "Farm share", Units == "Cents per Domestic Real Food Dollar", TABLE_NAME %in% c("Food dollar", "Food at home dollar", "Food away from home dollar")) %>% 
  rename(value = total, var = TABLE_NAME) %>%
  dplyr::select(year, var, value)

share_plot <- ggplot(data = dta_graph, aes(x = year, y = value, color = var)) + 
  geom_line() + 
  geom_point(aes(shape=var), size=1.25) + 
  scale_color_viridis(discrete = TRUE) + 
  scale_shape_manual(values=c(8,16,1)) + 
  ylab("Farm share (%)") + 
  scale_x_continuous(breaks=seq(1992, 2014, by=2)) + 
  theme_bw() + mytheme + theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'), legend.position = c(0.7, 0.30), legend.direction = "vertical", axis.text.x = element_text(size = 8, colour = 'black', family = "Times New Roman"))

#share_plot



