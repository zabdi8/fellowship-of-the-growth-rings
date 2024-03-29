#Load packages####
library(dplR)

#Load the data####
e13c_bet_n_07_v01 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_07/V/E13C.Bet.n.07.v01.csv")
e13c_bet_n_07_v02 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_07/V/E13C.Bet.n.07.v02.csv")
e13c_bet_n_07_v03 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_07/V/E13C.Bet.n.07.v03.csv")
e13c_bet_n_07_v04 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_07/V/E13C.Bet.n.07.v04.csv")
e13c_bet_n_07_v05 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_07/V/E13C.Bet.n.07.v05.csv")
e13c_bet_n_07_v06 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_07/V/E13C.Bet.n.07.v06.csv")


#remove the core year

e13c_bet_n_07_v01 <- e13c_bet_n_07_v01[-1, ]
e13c_bet_n_07_v02 <- e13c_bet_n_07_v02[-1, ]
e13c_bet_n_07_v03 <- e13c_bet_n_07_v03[-1, ]
e13c_bet_n_07_v04 <- e13c_bet_n_07_v04[-1, ]
e13c_bet_n_07_v05 <- e13c_bet_n_07_v05[-1, ]
e13c_bet_n_07_v06 <- e13c_bet_n_07_v06[-1, ]

#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_07_v01$row_names <- rownames(e13c_bet_n_07_v01)
e13c_bet_n_07_v02$row_names <- rownames(e13c_bet_n_07_v02)
e13c_bet_n_07_v03$row_names <- rownames(e13c_bet_n_07_v03)
e13c_bet_n_07_v04$row_names <- rownames(e13c_bet_n_07_v04)
e13c_bet_n_07_v05$row_names <- rownames(e13c_bet_n_07_v05)
e13c_bet_n_07_v06$row_names <- rownames(e13c_bet_n_07_v06)

# Merge the data frames using Reduce and merge
e13c_bet_n_07_v <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13c_bet_n_07_v01,
                               e13c_bet_n_07_v02,
                               e13c_bet_n_07_v03,
                               e13c_bet_n_07_v04,
                               e13c_bet_n_07_v05, 
                               e13c_bet_n_07_v06)
                          )

# Set row names and remove the extra column
rownames(e13c_bet_n_07_v) <- e13c_bet_n_07_v[[common_column]]
e13c_bet_n_07_v[[common_column]] <- NULL

view(e13c_bet_n_07_v)

#export rwl

write.rwl(e13c_bet_n_07_v, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/v/e13c_bet_n_07_v", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Data Analysis####
##Statistics####
e13c_bet_n_07_v_stats <- rwl.stats(e13c_bet_n_07_v) #summary and stats
print(e13c_bet_n_07_v_stats)

e13c_bet_n_07_v_ms <- sens2(e13c_bet_n_07_v) #calculates the mean sensitivity
print(e13c_bet_n_07_v_ms)

e13c_bet_n_07_v_report <- rwl.report(e13c_bet_n_07_v)  #report on rwl
print(e13c_bet_n_07_v_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_07_v) #creates a segment plot
spag.plot(e13c_bet_n_07_v, zfac=0.007,) #creates a spaghetti plot
title(main = "e13cbetn07v", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.6) #add title

##Analysis####

e13c_bet_n_07_v_inter <- interseries.cor(e13c_bet_n_07_v, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13c_bet_n_07_v_inter)

###General correlation####

corr.rwl.seg(rwl = e13c_bet_n_07_v, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13cbetn07v", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title