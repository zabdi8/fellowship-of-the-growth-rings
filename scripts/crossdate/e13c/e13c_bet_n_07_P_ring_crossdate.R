#Load packages####
library(dplR)

#Load the data####
e13c_bet_n_07_p01 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_07/P/E13C.Bet.n.07.p01.csv")
e13c_bet_n_07_p02 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_07/P/E13C.Bet.n.07.p02.csv")
e13c_bet_n_07_p03 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_07/P/E13C.Bet.n.07.p03.csv")
e13c_bet_n_07_p04 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_07/P/E13C.Bet.n.07.p04.csv")


#remove the core year

e13c_bet_n_07_p01 <- e13c_bet_n_07_p01[-1, ]
e13c_bet_n_07_p02 <- e13c_bet_n_07_p02[-1, ]
e13c_bet_n_07_p03 <- e13c_bet_n_07_p03[-1, ]
e13c_bet_n_07_p04 <- e13c_bet_n_07_p04[-1, ]


#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_07_p01$row_names <- rownames(e13c_bet_n_07_p01)
e13c_bet_n_07_p02$row_names <- rownames(e13c_bet_n_07_p02)
e13c_bet_n_07_p03$row_names <- rownames(e13c_bet_n_07_p03)
e13c_bet_n_07_p04$row_names <- rownames(e13c_bet_n_07_p04)

# Merge the data frames using Reduce and merge
e13c_bet_n_07_p <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13c_bet_n_07_p01,
                               e13c_bet_n_07_p02, 
                               e13c_bet_n_07_p03, 
                               e13c_bet_n_07_p04)
                          )

# Set row names and remove the extra column
rownames(e13c_bet_n_07_p) <- e13c_bet_n_07_p[[common_column]]
e13c_bet_n_07_p[[common_column]] <- NULL

view(e13c_bet_n_07_p)

#export rwl

write.rwl(e13c_bet_n_07_p, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/p/e13c_bet_n_07_p", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Data Analysis####
##Statistics####
e13c_bet_n_07_p_stats <- rwl.stats(e13c_bet_n_07_p) #summary and stats
print(e13c_bet_n_07_p_stats)

e13c_bet_n_07_p_ms <- sens2(e13c_bet_n_07_p) #calculates the mean sensitivity
print(e13c_bet_n_07_p_ms)

e13c_bet_n_07_p_report <- rwl.report(e13c_bet_n_07_p)  #report on rwl 
print(e13c_bet_n_07_p_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_07_p) #creates a segment plot
spag.plot(e13c_bet_n_07_p, zfac=0.01) #creates a spaghetti plot
title(main = "e13cbetn07p", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.6) #add title

##Analysis####

e13c_bet_n_07_p_inter <- interseries.cor(e13c_bet_n_07_p, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13c_bet_n_07_p_inter)

###General correlation####

corr.rwl.seg(rwl = e13c_bet_n_07_p, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13cbetn07p", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

