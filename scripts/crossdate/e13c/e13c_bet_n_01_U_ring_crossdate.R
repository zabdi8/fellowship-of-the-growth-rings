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
e13c_bet_n_01_u01 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_01_/U/E13C.Bet.n.01.u01.csv")
e13c_bet_n_01_u02 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_01_/U/E13C.Bet.n.01.u02.csv")
e13c_bet_n_01_u03 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_01_/U/E13C.Bet.n.01.u03.csv")
e13c_bet_n_01_u04 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_01_/U/E13C.Bet.n.01.u04.csv")


#remove the core year

e13c_bet_n_01_u01 <- e13c_bet_n_01_u01[-1, ]
e13c_bet_n_01_u02 <- e13c_bet_n_01_u02[-1, ]
e13c_bet_n_01_u03 <- e13c_bet_n_01_u03[-1, ]
e13c_bet_n_01_u04 <- e13c_bet_n_01_u04[-1, ]



#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_01_u01$row_names <- rownames(e13c_bet_n_01_u01)
e13c_bet_n_01_u02$row_names <- rownames(e13c_bet_n_01_u02)
e13c_bet_n_01_u03$row_names <- rownames(e13c_bet_n_01_u03)
e13c_bet_n_01_u04$row_names <- rownames(e13c_bet_n_01_u04)


# Merge the data frames using Reduce and merge
e13c_bet_n_01_u <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13c_bet_n_01_u01, 
                               e13c_bet_n_01_u02, 
                               e13c_bet_n_01_u03, 
                               e13c_bet_n_01_u04)
                          )

# Set row names and remove the extra column
rownames(e13c_bet_n_01_u) <- e13c_bet_n_01_u[[common_column]]
e13c_bet_n_01_u[[common_column]] <- NULL

view(e13c_bet_n_01_u)

#export rwl

write.rwl(e13c_bet_n_01_u, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/u/e13c_bet_n_01_U", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#Data Analysis####
##Statistics####
e13c_bet_n_01_u_stats <- rwl.stats(e13c_bet_n_01_u) #summary and stats
print(e13c_bet_n_01_u_stats)

e13c_bet_n_01_u_ms <- sens2(e13c_bet_n_01_u) #calculates the mean sensitivity
print(e13c_bet_n_01_u_ms)

e13c_bet_n_01_u_report <- rwl.report(e13c_bet_n_01_u)  #report on rwl
print(e13c_bet_n_01_u_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_01_u) #creates a segment plot
spag.plot(e13c_bet_n_01_u, zfac=0.007,) #creates a spaghetti plot

##Analysis####

e13c_bet_n_01_u_inter <- interseries.cor(e13c_bet_n_01_u, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13c_bet_n_01_u_inter)

###General correlation####

corr.rwl.seg(rwl = e13c_bet_n_01_u, seg.length = 1, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "e13cbetn01u", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title




