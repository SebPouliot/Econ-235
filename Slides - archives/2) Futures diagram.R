############################################################
rm(list = ls())


dta = data.frame(array(NA, c(17,3)))
colnames(dta) <- c("price", "short", "long")

dta$price <- seq(2,6, by= 0.25)
dta$long <- (dta$price-4)*5000
dta$short <- (4-dta$price)*5000

library(ggplot2)
library(grid)#for arrow in graph
library(gridExtra)
library(reshape2)
library(data.table)

suppressWarnings(try(source("D:/Box Sync/Graph parameters.R"), silent = TRUE))
suppressWarnings(try(source("C:/Users/pouliot.IASTATE/Documents/Box Sync/Graph parameters.R"), silent = TRUE))
suppressWarnings(try(source("C:/Users/pouliot.IASTATE/Box Sync/Graph parameters.R"), silent = TRUE))


dta_graph <-  melt(dta[,c("price", "long", "short")], id="price")
colnames(dta_graph) <- c("price", "position", "gain")
dta_graph$position <- as.character(dta_graph$position)

dta_graph$position[dta_graph$position=="long"] <- "Long position"
dta_graph$position[dta_graph$position=="short"] <- "Short position"


long_plot <- ggplot(data = dta_graph[dta_graph$position=="Long position",], aes(x = price, y = gain, color = position)) + geom_hline(aes(yintercept=0), color="grey", size=0.5) + geom_vline(aes(xintercept=4), color="grey", size=0.5) + geom_line(aes(linetype=position), size=0.5, color="red") + scale_linetype_manual(values=c(1)) + geom_point(aes(shape=position, color = position), size=1.25, color="red") + scale_shape_manual(values=c(8))  + scale_x_continuous(breaks=c(2,3, 4, 5, 6), labels=c(2,3, 4, 5, 6)) + scale_y_continuous(breaks=c(-10000, - 5000, 0, 5000, 10000), labels=c("-10,000", "-5,000", "0", "5,000", "10,000")) + coord_cartesian(ylim = c(-10000, 10000), xlim = c(2, 6)) + ylab("Loss or gain ($)")  + xlab("Price ($/bu)") + theme_bw() + mytheme 

#long_plot
#dev.off()

#ggsave(long_plot, file="Teaching/Econ 235/Spring 2017/Slides/diagram_long.png", width=6, height=4)

short_plot <- ggplot(data = dta_graph[dta_graph$position=="Short position",], aes(x = price, y = gain, color = position)) + geom_hline(aes(yintercept=0), color="grey", size=0.5) + geom_vline(aes(xintercept=4), color="grey", size=0.5) + geom_line(aes(linetype=position), color="blue", size=0.5) + scale_linetype_manual(values=c(1)) + geom_point(aes(shape=position), color="blue") + scale_shape_manual(values=c(16)) + scale_x_continuous(breaks=c(2, 3, 4, 5, 6), labels=c(2, 3, 4, 5, 6)) + scale_y_continuous(breaks=c(-10000, - 5000, 0, 5000, 10000), labels=c("-10,000", "-5,000", "0", "5,000", "10,000")) + coord_cartesian(ylim = c(-10000, 10000), xlim = c(2, 6)) + ylab("Loss or gain ($)")  + xlab("Price ($/bu)") + theme_bw() + mytheme 
#dev.off()

#short_plot
#dev.off()

#ggsave(short_plot, file="Teaching/Econ 235/Spring 2014/Slides/diagram_short.png", width=6, height=4)


position_plot <- ggplot(data = dta_graph, aes(x = price, y = gain, color = position)) + geom_hline(aes(yintercept=0), color="grey", size=0.5) + geom_vline(aes(xintercept=4), color="grey", size=0.5)  + geom_line(aes(linetype=position)) + scale_linetype_manual(values=c(1,1)) + geom_point(aes(shape=position)) + scale_color_manual(values = c("red", "blue")) + scale_shape_manual(values=c(8,16)) + scale_size_manual(values = c(0.5, 0.5)) + scale_x_continuous(breaks=c(2, 3, 4, 5, 6), labels=c(2, 3, 4, 5, 6)) + scale_y_continuous(breaks=c(-10000, - 5000, 0, 5000, 10000), labels=c("-10,000", "-5,000", "0", "5,000", "10,000")) + coord_cartesian(ylim = c(-10000, 10000), xlim = c(2, 6))  + ylab("Loss or gain ($)")  + xlab("Price ($/bu)") + theme_bw() + mytheme 
#dev.off()

position_plot
#dev.off()

#ggsave(position_plot, file="Teaching/Econ 235/Spring 2014/Slides/diagram_short_long.png", width=6, height=4)


position_plot2 <- ggplot(data = dta_graph, aes(x = price, y = gain, color = position)) + geom_segment(aes(x=5, y=-10000, xend=5, yend=5000), color="black", linetype=c(2), size=0.5) + geom_segment(aes(x=2, y=-5000, xend=5, yend=-5000), color="black", linetype=c(2), size=0.5) + geom_segment(aes(x=2, y=5000, xend=5, yend=5000), color="black", linetype=c(2), size=0.5) + geom_hline(aes(yintercept=0), color="grey", size=0.5) + geom_vline(aes(xintercept=4), color="grey", size=0.5)  + geom_line(aes(linetype=position)) + scale_linetype_manual(values=c(1,1)) + geom_point(aes(shape=position)) + scale_color_manual(values = c("red", "blue")) + scale_shape_manual(values=c(8,16)) + scale_size_manual(values = c(0.5, 0.5)) + scale_x_continuous(breaks=c(2, 3, 4, 5, 6), labels=c(2, 3, 4, 5, 6)) + scale_y_continuous(breaks=c(-10000, - 5000, 0, 5000, 10000), labels=c("-10,000", "-5,000", "0", "5,000", "10,000")) + coord_cartesian(ylim = c(-10000, 10000), xlim = c(2, 6))  + ylab("Loss or gain ($)")  + xlab("Price ($/bu)") + theme_bw() + mytheme 
#dev.off()

position_plot2

#ggsave(position_plot2, file="Teaching/Econ 235/Spring 2014/Slides/diagram_short_long2.png", width=6, height=4)



