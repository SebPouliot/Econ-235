
#################
### Ag trade ####
#################

#download.file("https://www.ers.usda.gov/webdocs/DataFiles/US_Agricultural_Trade_Data_Update__18031//moustrade.xls", destfile = "Data/moustrade.xls", mode = "wb")

dta <- read_excel("Data/moustrade.xls", sheet = 1, skip = 6, col_names = FALSE) %>% dplyr::filter(!is.na(X2))
colnames(dta) <- c("Year", "Month", "Exports", "Imports", "Balance")

dta <- dta %>% mutate(Exports = Exports/1000000000, Imports = Imports/1000000000, Balance = Balance/1000000000, date = seq(as.Date("1975/10/1"), by = "month", length.out = nrow(dta))) %>%
  dplyr::select(-Year, - Month)

##############
### Graph ####
##############

dta_graph <- dta %>% dplyr::select(Exports, Imports, date) %>%
  mutate(Imports = -Imports) %>%
  gather(var, value, -date)


#Trade
agtrade <- ggplot(data = dta_graph, aes(x = date, y = value, fill = var))  + geom_bar(stat="identity", position = "identity") + scale_fill_viridis(discrete = TRUE) + ylab("Billion dollars") + theme_bw() + mytheme + theme(legend.position=c(0.25, 0.15), legend.direction = "horizontal") + theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'))

#ggsave(agtrade, file = "Figures/agtrade.png", width = 6, height = 4, dpi=600)


#Trade balance
balancetrade <- ggplot(data = dta, aes(x = date, y = Balance))  + geom_bar(stat="identity", position = "identity") + scale_fill_viridis(discrete = TRUE) + ylab("Billion dollars") + theme_bw() + mytheme + theme(legend.position=c(0.15, 0.15), legend.direction = "horizontal") + theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'))

#ggsave(balancetrade, file = "Figures/balancetrade.png", width = 6, height = 4, dpi=600)


####################################################
####### Trade by countries and commodities #########
####################################################

##############
### Graph ####
##############

#Exports by countries
#download.file("https://www.ers.usda.gov/webdocs/DataFiles/Calendar_year__18033//Xcytop15.xls", destfile = "Data/Xcytop15.xls", mode = "wb")


dta <- read_excel("Data/Xcytop15.xls", sheet = 1, skip = 7, col_names = FALSE) %>% dplyr::select(X2,X3)
colnames(dta) <- c("country", "exports")
dta  <- dta %>% dplyr::filter(!is.na(country))

dta <- dta %>% mutate(exports = exports/1000000000)

dta$country[dta$country=="European Union-28"] <- "EU"

Exportsbycountry <- ggplot(data = dta, aes(x = country, y = exports, fill = country))  + 
  geom_bar(stat="identity", color = "black") + 
  geom_text(aes(label=round(exports,2)), position=position_dodge(width=0.9), vjust=-0.25, size = 2) + 
  scale_fill_viridis(discrete = TRUE) + 
  ylab("Billion dollars") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position="none", axis.text.x = element_text(size  = 6, angle = 45, hjust = 1, vjust = 1), axis.title.x=element_blank()) + theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'))

#Exportsbycountry
#ggsave(Exportsbycountry, file = "Figures/Exportsbycountry.png", width = 8, height = 4, dpi=600)


#Imports
#download.file("https://www.ers.usda.gov/webdocs/DataFiles/Calendar_year__18033//McyTOP15.xls", destfile = "Data/McyTOP15.xls", mode = "wb")

dta <- read_excel("McyTOP15.xls", sheet = 1, skip = 7, col_names = FALSE) %>% dplyr::select(X2,X3)
colnames(dta) <- c("country", "imports")
dta  <- dta %>% dplyr::filter(!is.na(country))

dta <- dta %>% mutate(imports = imports/1000000000)

dta$country[dta$country=="European Union-28"] <- "EU"

Importsbycountry <- ggplot(data = dta, aes(x = country, y = imports, fill = country))  + 
  geom_bar(stat="identity", color = "black") + 
  geom_text(aes(label=round(imports,2)), position=position_dodge(width=0.9), vjust=-0.25, size = 2) + 
  scale_fill_viridis(discrete = TRUE) + 
  ylab("Billion dollars") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position="none", axis.text.x = element_text(size  = 6, angle = 45, hjust = 1, vjust = 1)) + theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'))

#Importsbycountry

#ggsave(Importsbycountry, file = "Figures/Importsbycountry.png", width = 8, height = 4, dpi=600)


#Exports by commodities
#download.file("https://www.ers.usda.gov/webdocs/DataFiles/Calendar_year__18033//cytop25hvpexp.xls", destfile = "Data/cytop25hvpexp.xls", mode = "wb")

dta <- read_excel("Data/cytop25hvpexp.xls", sheet = 1, skip = 2, col_names = FALSE) %>% dplyr::select(X2,X3, X4)
colnames(dta) <- c("product", "process", "exports")
dta  <- dta %>% dplyr::filter(!is.na(product))

dta <- dta %>% mutate(exports = exports/1000000000)

dta  <-  dta[1:15,]

Exportsbycommodities <- ggplot(data = dta, aes(x = product, y = exports, fill = product))  + 
  geom_bar(stat="identity", color = "black") + 
  geom_text(aes(label=round(exports,2)), position=position_dodge(width=0.9), vjust=-0.25, size = 2) + 
  scale_fill_viridis(discrete = TRUE) + 
  ylab("Billion dollars") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position="none") + theme(axis.text.x = element_text(size  = 6, angle = 45, hjust = 1, vjust = 1), axis.title.x=element_blank()) + theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'))

#Exportsbycommodities
#ggsave(p, file = "Figures/Exportsbycommodities.png", width = 8, height = 4, dpi=600)

#Imports by commodities
#download.file("https://www.ers.usda.gov/webdocs/DataFiles/Calendar_year__18033//cytop25hvpimp.xls", destfile = "Data/cytop25hvpimp.xls", mode = "wb")

dta <- read_excel("Data/cytop25hvpimp.xls", sheet = 1, skip = 2, col_names = FALSE) %>% dplyr::select(X2,X3, X4)
colnames(dta) <- c("product", "process", "imports")
dta  <- dta %>% dplyr::filter(!is.na(product))

dta <- dta %>% mutate(imports = imports/1000000000)

dta  <-  dta[1:15,]

Importsbycommodities <- ggplot(data = dta, aes(x = product, y = imports, fill = product))  +
  geom_bar(stat="identity", color = "black") + 
  geom_text(aes(label=round(imports,2)), position=position_dodge(width=0.9), vjust=-0.25, size = 2) + 
  scale_fill_viridis(discrete = TRUE) + 
  ylab("Billion dollars") + 
  theme_bw() + 
  mytheme + 
  theme(legend.position="none", axis.text.x = element_text(size  = 6, angle = 45, hjust = 1, vjust = 1)) + theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'))

#Importsbycommodities
#ggsave(Importsbycommodities, file = "Importsbycommodities.png", width = 8, height = 4, dpi=600)


###############################
####### Exchange rate #########
###############################

pacman::p_load(Quandl)

Quandl.api_key("2xcmYFJoJKuEDwawuV7J")

##############
### Graph ####
##############

#Canada
dtaCA <- try(Quandl("FRED/DEXCAUS")) %>% tbl_df %>% rename(Date = DATE, Value = VALUE) %>% dplyr::filter(Date >= "2000-01-01")

CAexchangerate <- ggplot(data = dtaCA, aes(x = Date, y = Value)) + 
  geom_hline(yintercept=1, size = 1, color = "gray") + 
  geom_line() + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) + 
  ylab("$CA/$US") + 
  theme_bw() + 
  mytheme + 
  theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'), axis.text.x = element_text(size  = 6))

#CAexchangerate
#ggsave(CAexchangerate, file = "Figures/CAexchangerate.png", width = 8, height = 4, dpi=600)


#Europe
dtaEU <- try(Quandl("FRED/DEXUSEU")) %>% tbl_df %>% rename(Date = DATE, Value = VALUE) %>% dplyr::filter(Date >= "2000-01-01")

EUexchangerate <- ggplot(data = dtaEU, aes(x = Date, y = Value)) + 
  geom_hline(yintercept=1, size = 1, color = "gray") + 
  geom_line() + scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) + 
  ylab("Euro/$US") + 
  theme_bw() + 
  mytheme + 
  theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'), axis.text.x = element_text(size  = 6))

#EUexchangerate
#ggsave(EUexchangerate, file = "Figures/EUexchangerate.png", width = 8, height = 4, dpi=600)


#China
dtaChina <- try(Quandl("FRED/EXCHUS")) %>% tbl_df%>% rename(Date = DATE, Value = VALUE) %>% dplyr::filter(Date >= "2000-01-01")

Chinaexchangerate <- ggplot(data = dtaChina, aes(x = Date, y = Value)) + 
  geom_line() + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) + 
  ylab("Renminbi/$US") + 
  theme_bw() + 
  mytheme + 
  theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'), axis.text.x = element_text(size  = 6))

#Chinaexchangerate
#ggsave(Chinaexchangerate, file = "Figures/Chinaexchangerate.png", width = 8, height = 4, dpi=600)


#############################
####### Food prices #########
#############################


##############
### Graph ####
##############

#download.file("http://www.fao.org/fileadmin/templates/worldfood/Reports_and_docs/Food_price_indices_data.xls", destfile = "Data/Food_price_indices_data.xls", mode = "wb")
#Must convert file to xlsx, delete unexisting columns

dta.n <- read_excel("Data/Food_price_indices_data.xlsx", sheet = 1, skip = 2)
dta.n$Date <- as.Date(paste("01", dta.n$Date, sep = "/"), "%d/%m/%Y")
dta.n <- dta.n %>% mutate(var = "Nominal")

dta.r <- read_excel("Data/Food_price_indices_data.xlsx", sheet = 3, skip = 2)
dta.r$Date <- as.Date(paste("01", dta.r$Date, sep = "/"), "%d/%m/%Y")
dta.r <- dta.r %>% mutate(var = "Real")
 
#Food price indices
dta <- rbind(dta.n[,c("Date", "Food Price Index", "var")], dta.r[,c("Date", "Food Price Index", "var")])

WorldFoodindex <- ggplot(data = dta, aes(x = Date, y = `Food Price Index`, color = var)) + 
  geom_line() + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) + 
  scale_color_viridis(discrete = TRUE) + 
  theme_bw() +
  mytheme + 
  theme(legend.position=c(0.15, 0.75), legend.direction = "horizontal") + 
  theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'), axis.text.x = element_text(size  = 4))

#World_Food_index
#ggsave(World_Food_index, file = "Figures/World_Food_index.png", width = 8, height = 4, dpi=600)


#Meat price indices
dta <- rbind(dta.n[,c("Date", "Meat Price Index", "var")], dta.r[,c("Date", "Meat Price Index", "var")])

WorldMeatindex <- ggplot(data = dta, aes(x = Date, y = `Meat Price Index`, color = var)) + 
  geom_line() + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) + 
  scale_color_viridis(discrete = TRUE) + 
  theme_bw() + 
  mytheme + 
  theme(legend.position=c(0.15, 0.75), legend.direction = "horizontal") + 
  theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'), axis.text.x = element_text(size  = 4))

#WorldMeatindex
#ggsave(World_Meat_index, file = "Figures/World_Meat_index.png", width = 8, height = 4, dpi=600)


#Dairy price indices
dta <- rbind(dta.n[,c("Date", "Dairy Price Index", "var")], dta.r[,c("Date", "Dairy Price Index", "var")])

World_Dairy_index <- ggplot(data = dta, aes(x = Date, y = `Dairy Price Index`, color = var)) + 
  geom_line() + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) + 
  scale_color_viridis(discrete = TRUE) + 
  theme_bw() + 
  mytheme + 
  theme(legend.position=c(0.15, 0.75), legend.direction = "horizontal") + 
  theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'), axis.text.x = element_text(size  = 4))

#World_Dairy_index
#ggsave(World_Dairy_index, file = "Figures/World_Dairy_index.png", width = 8, height = 4, dpi=600)


#Dairy price indices
dta <- rbind(dta.n[,c("Date", "Cereals Price Index", "var")], dta.r[,c("Date", "Cereals Price Index", "var")])

WorldCerealsindex <- ggplot(data = dta, aes(x = Date, y = `Cereals Price Index`, color = var)) + 
  geom_line() + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) + 
  scale_color_viridis(discrete = TRUE) + 
  theme_bw() + 
  mytheme + 
  theme(legend.position=c(0.15, 0.75), legend.direction = "horizontal") + 
  theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'), axis.text.x = element_text(size  = 4))

#World_Cereals_index
#ggsave(World_Cereals_index, file = "Figures/World_Cereals_index.png", width = 8, height = 4, dpi=600)


#Oils price indices
dta <- rbind(dta.n[,c("Date", "Oils Price Index", "var")], dta.r[,c("Date", "Oils Price Index", "var")])

World_Oils_index <- ggplot(data = dta, aes(x = Date, y = `Oils Price Index`, color = var)) + 
  geom_line() + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) + 
  scale_color_viridis(discrete = TRUE) + 
  theme_bw() + 
  mytheme + 
  theme(legend.position=c(0.15, 0.75), legend.direction = "horizontal") + 
  theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'), axis.text.x = element_text(size  = 4))

#World_Oils_index
#ggsave(World_Oils_index, file = "Figures/World_Oils_index.png", width = 8, height = 4, dpi=600)

#Sugar price indices
dta <- rbind(dta.n[,c("Date", "Sugar Price Index", "var")], dta.r[,c("Date", "Sugar Price Index", "var")])

World_Sugar_index <- ggplot(data = dta, aes(x = Date, y = `Sugar Price Index`, color = var)) +
  geom_line() + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("year")) + 
  scale_color_viridis(discrete = TRUE) + 
  theme_bw() + 
  mytheme + 
  theme(legend.position=c(0.15, 0.75), legend.direction = "horizontal") + 
  theme(axis.title.x=element_blank(), axis.line.x = element_line(color='black'), axis.line.y = element_line(color='black'), axis.text.x = element_text(size  = 4))

#World_Sugar_index
#ggsave(World_Sugar_index, file = "Figures/World_Sugar_index.png", width = 8, height = 4, dpi=600)


#summary(dta.n[dta.n$Date>="2002-01-01" & dta.n$Date<="2004-12-31",])
#summary(dta.r[dta.r$Date>="2002-01-01" & dta.r$Date<="2004-12-31",])








