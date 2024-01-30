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
e13c_bet_n_07_s01 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_07/S/E13C.Bet.n.07.s01.csv")
e13c_bet_n_07_s04 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_07/S/E13C.Bet.n.07.s04.csv")
e13c_bet_n_07_s06 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_07/S/E13C.Bet.n.07.s06.csv")


#remove the core year

e13c_bet_n_07_s01 <- e13c_bet_n_07_s01[-1, ]
e13c_bet_n_07_s04 <- e13c_bet_n_07_s04[-1, ]
e13c_bet_n_07_s06 <- e13c_bet_n_07_s06[-1, ]


#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_07_s01$row_names <- rownames(e13c_bet_n_07_s01)
e13c_bet_n_07_s04$row_names <- rownames(e13c_bet_n_07_s04)
e13c_bet_n_07_s06$row_names <- rownames(e13c_bet_n_07_s06)



# Merge the data frames using Reduce and merge
e13c_bet_n_07_s <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13c_bet_n_07_s01, 
                               e13c_bet_n_07_s04, 
                               e13c_bet_n_07_s06)
)

# Set row names and remove the extra column
rownames(e13c_bet_n_07_s) <- e13c_bet_n_07_s[[common_column]]
e13c_bet_n_07_s[[common_column]] <- NULL

view(e13c_bet_n_07_s)

#export rwl

write.rwl(e13c_bet_n_07_s, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/s/e13c_bet_n_07_s", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#Data Analysis####
##Statistics####
e13c_bet_n_07_s_stats <- rwl.stats(e13c_bet_n_07_s) #summary and stats
print(e13c_bet_n_07_s_stats)

e13c_bet_n_07_s_ms <- sens2(e13c_bet_n_07_s) #calculates the mean sensitivity
print(e13c_bet_n_07_s_ms)

e13c_bet_n_07_s_report <- rwl.report(e13c_bet_n_07_s)  #report on rwl
print(e13c_bet_n_07_s_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_07_s) #creates a segment plot
spag.plot(e13c_bet_n_07_s, zfac=0.007,) #creates a spaghetti plot
title(main = "e13cbetn07r", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.6) #add title

##Analysis####

e13c_bet_n_07_s_inter <- interseries.cor(e13c_bet_n_07_s, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13c_bet_n_07_s_inter)

###General correlation####

corr.rwl.seg(rwl = e13c_bet_n_07_s, seg.length = 8, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13cbetn07r", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

