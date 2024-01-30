#Load packages####
library(dplR)
library(ggplot2)
library(tidyverse)
library(wesanderson)
library(signal)
library(shiny)
library(treeclim)
library(ggplot2)
library(reshape2)
library(dplyr)

#Load the data####
e13c_bet_n_01_s01 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_01_/S/E13C.Bet.n.01.s01.csv")
e13c_bet_n_01_s02 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_01_/S/E13C.Bet.n.01.s02.csv")
e13c_bet_n_01_s03 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_01_/S/E13C.Bet.n.01.s03.csv")
e13c_bet_n_01_s04 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_01_/S/E13C.Bet.n.01.s04.csv")

#remove the core year

e13c_bet_n_01_s01 <- e13c_bet_n_01_s01[-1, ]
e13c_bet_n_01_s02 <- e13c_bet_n_01_s02[-1, ]
e13c_bet_n_01_s03 <- e13c_bet_n_01_s03[-1, ]
e13c_bet_n_01_s04 <- e13c_bet_n_01_s04[-1, ]


#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_01_s01$row_names <- rownames(e13c_bet_n_01_s01)
e13c_bet_n_01_s02$row_names <- rownames(e13c_bet_n_01_s02)
e13c_bet_n_01_s03$row_names <- rownames(e13c_bet_n_01_s03)
e13c_bet_n_01_s04$row_names <- rownames(e13c_bet_n_01_s04)

# Merge the data frames using Reduce and merge
e13c_bet_n_01_s <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13c_bet_n_01_s01, e13c_bet_n_01_s02, e13c_bet_n_01_s03, e13c_bet_n_01_s04))

 # Set row names and remove the extra column
 rownames(e13c_bet_n_01_s) <- e13c_bet_n_01_s[[common_column]]
 e13c_bet_n_01_s[[common_column]] <- NULL

view(e13c_bet_n_01_s)

#export rwl

write.rwl(e13c_bet_n_01_s, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/s/e13c_bet_n_01_S", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#Data Analysis####
##Statistics####
e13c_bet_n_01_s_stats <- rwl.stats(e13c_bet_n_01_s) #summary and stats
print(e13c_bet_n_01_s_stats)

e13c_bet_n_01_s_ms <- sens2(e13c_bet_n_01_s) #calculates the mean sensitivity
print(e13c_bet_n_01_s_ms)

e13c_bet_n_01_s_report <- rwl.report(e13c_bet_n_01_s)  #report on rwl
print(e13c_bet_n_01_s_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_01_s) #creates a segment plot
spag.plot(e13c_bet_n_01_s, zfac=0.007,) #creates a spaghetti plot

##Analysis####

e13c_bet_n_01_s_inter <- interseries.cor(e13c_bet_n_01_s, n = NULL, prewhiten = TRUE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13c_bet_n_01_s_inter)

###General correlation####

corr.rwl.seg(rwl = e13c_bet_n_01_s, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "e13cbetn01s", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

###Series vs Master####
####E13CBetn01S####
#e13c_bet_n_01_s01 
rwl <- e13c_bet_n_01_s_filter
serie <- "E13CBetn01s01_r1"
e13c_bet_n_01_s01_r1_serie <- series.rwl.plot(rwl, series = serie, 
                                            series.yrs = as.numeric(names(series)),
                                            seg.length = 2, bin.floor = 0, n=NULL,
                                            prewhiten = FALSE, biweight = FALSE, 
                                            floor.plus1 = FALSE)
title(sub= "E13CBetn01s01_r1", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r01r2
serie <- "E13CBetn01s01_r2"
e13c_bet_n_01_s01_r2_serie <- series.rwl.plot(rwl, series = serie, 
                                            series.yrs = as.numeric(names(series)),
                                            seg.length = 2, bin.floor = 0, n=NULL,
                                            prewhiten = FALSE, biweight = FALSE, 
                                            floor.plus1 = FALSE
)
title(sub= "E13CBetn01s01_r2", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01s01_r3

serie <- "E13CBetn01s01_r3"
e13c_bet_n_01_s01_r3_serie <- series.rwl.plot(rwl, series = serie, 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = TRUE, biweight = TRUE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r01r3", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r01r4
serie <- "E13CBetn01s01_r4"
e13c_bet_n_01_s01_r4_serie <- series.rwl.plot(rwl, series = serie, 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r01r4", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title



#average for e13c_bet_n_01_sS####
# #create a Df with the average of each section for e13c_bet_n_01_S
#
# # Create a new DataFrame with the same row names as the original DataFrame
 e13c_bet_n_01_s_average <- data.frame(RowNames = rownames(e13c_bet_n_01_s))
#
# # Calculate the average for each set of columns and add them to the new DataFrame
# e13c_bet_n_01_R_average$E13CBetn01r01 <- rowMeans(e13c_bet_n_01_R_aligned[, c("E13CBetn01r01r1",
#                                                                               "E13CBetn01r01r2",
#                                                                               "E13CBetn01r01r3",
#                                                                               "E13CBetn01r01r4")],
#                                                   na.rm = TRUE)
# e13c_bet_n_01_R_average$E13CBetn01r02 <- rowMeans(e13c_bet_n_01_R_aligned[, c("E13CBetn01r02r1",
#                                                                               "E13CBetn01r02r2",
#                                                                               "E13CBetn01r02r3",
#                                                                               "E13CBetn01r02r4")],
#                                                   na.rm = TRUE)
# e13c_bet_n_01_R_average$E13CBetn01r03 <- rowMeans(e13c_bet_n_01_R_aligned[, c("E13CBetn01r03r1",
#                                                                               "E13CBetn01r03r2",
#                                                                               "E13CBetn01r03r3",
#                                                                               "E13CBetn01r03r4")],
#                                                   na.rm = TRUE)
# e13c_bet_n_01_R_average$E13CBetn01r04 <- rowMeans(e13c_bet_n_01_R_aligned[, c("E13CBetn01r04r1",
#                                                                               "E13CBetn01r04r2",
#                                                                               "E13CBetn01r04r3",
#                                                                               "E13CBetn01r04r4")],
#                                                   na.rm = TRUE)
# e13c_bet_n_01_R_average$E13CBetn01r05 <- rowMeans(e13c_bet_n_01_R_aligned[, c("E13CBetn01r05r1",
#                                                                               "E13CBetn01r05r2",
#                                                                               "E13CBetn01r05r3",
#                                                                               "E13CBetn01r05r4")],
#                                                   na.rm = TRUE)
#
# rownames(e13c_bet_n_01_R_average) <- e13c_bet_n_01_R_average$RowNames
# e13c_bet_n_01_R_average <- e13c_bet_n_01_R_average[, -1]
# view(e13c_bet_n_01_R_average)
#
# # fix the Nas and eliminate the data from the "cores"
# e13c_bet_n_01_R_average <- e13c_bet_n_01_R_average[-1,] #remove the year 2001 which has core measurments.
#
# #E13CBetn01r01
# e13c_bet_n_01_R_average["2002", "E13CBetn01r01"] <- NA
# e13c_bet_n_01_R_average["2003", "E13CBetn01r01"] <- NA
# e13c_bet_n_01_R_average["2004", "E13CBetn01r01"] <- NA
# e13c_bet_n_01_R_average["2005", "E13CBetn01r01"] <- NA
#
# #E13CBetn01r02
# e13c_bet_n_01_R_average["2002", "E13CBetn01r02"] <- NA
# e13c_bet_n_01_R_average["2003", "E13CBetn01r02"] <- NA
# e13c_bet_n_01_R_average["2004", "E13CBetn01r02"] <- NA
# e13c_bet_n_01_R_average["2005", "E13CBetn01r02"] <- NA
# e13c_bet_n_01_R_average["2006", "E13CBetn01r02"] <- NA
#
# #E13CBetn01r3
# e13c_bet_n_01_R_average["2002", "E13CBetn01r03"] <- NA
# e13c_bet_n_01_R_average["2003", "E13CBetn01r03"] <- NA
# e13c_bet_n_01_R_average["2004", "E13CBetn01r03"] <- NA
# e13c_bet_n_01_R_average["2005", "E13CBetn01r03"] <- NA
# e13c_bet_n_01_R_average["2006", "E13CBetn01r03"] <- NA

#
# view(e13c_bet_n_01_R_average)
# write.csv(e13c_bet_n_01_R_average, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/r/e13c_bet_n_01_R_average.csv")
# #stats for the aligned average
# rwl <- e13c_bet_n_01_R_average
#
# e13c_bet_n_01_R_average_stats <- rwl.stats(rwl) #summary and stats
# print(e13c_bet_n_01_R_average_stats)
#
# e13c_bet_n_01_R_average_ms <- sens2(rwl) #calculates the mean sensitivity
# print(e13c_bet_n_01_R_average_ms)
#
# e13c_bet_n_01_R_average_report <- rwl.report(rwl)  #report on rwl
# print(e13c_bet_n_01_R_average_report)
# 
# ##Cross-dating and alignment####
# 
# #Check the alignment of the series
# 
# #graphs
# seg.plot(rwl) #creates a segment plot
# spag.plot(rwl, zfac=0.005,) #creates a spaghetti plot


# 
# ##Analysis####
# 
# e13c_bet_n_01_R_aligned_inter <- interseries.cor(e13c_bet_n_01_R_aligned, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
# print(e13c_bet_n_01_R_aligned_inter)
# 
# ###General correlation####
# 
# corr.rwl.seg(rwl = e13c_bet_n_01_R_aligned, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis
# 
# title(main = "e13c_bet_n_01_S", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title