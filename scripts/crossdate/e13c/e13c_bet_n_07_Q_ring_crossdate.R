#Load packages####
library(dplR)

#Load the data####
e13c_bet_n_07_q01 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_07/Q/E13C.Bet.n.07.q01.csv")
e13c_bet_n_07_q02 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_07/Q/E13C.Bet.n.07.q02.csv")
e13c_bet_n_07_q03 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_07/Q/E13C.Bet.n.07.q03.csv")
e13c_bet_n_07_q04 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_07/Q/E13C.Bet.n.07.q04.csv")


#remove the core year

e13c_bet_n_07_q01 <- e13c_bet_n_07_q01[-1, ]
e13c_bet_n_07_q02 <- e13c_bet_n_07_q02[-1, ]
e13c_bet_n_07_q03 <- e13c_bet_n_07_q03[-1, ]
e13c_bet_n_07_q04 <- e13c_bet_n_07_q04[-1, ]


#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_07_q01$row_names <- rownames(e13c_bet_n_07_q01)
e13c_bet_n_07_q02$row_names <- rownames(e13c_bet_n_07_q02)
e13c_bet_n_07_q03$row_names <- rownames(e13c_bet_n_07_q03)
e13c_bet_n_07_q04$row_names <- rownames(e13c_bet_n_07_q04)



# Merge the data frames using Reduce and merge
e13c_bet_n_07_q <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13c_bet_n_07_q01, 
                               e13c_bet_n_07_q02,
                               e13c_bet_n_07_q03, 
                               e13c_bet_n_07_q04)
)

# Set row names and remove the extra column
rownames(e13c_bet_n_07_q) <- e13c_bet_n_07_q[[common_column]]
e13c_bet_n_07_q[[common_column]] <- NULL

view(e13c_bet_n_07_q)

#export rwl

write.rwl(e13c_bet_n_07_q, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/q/e13c_bet_n_07_q", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#Data Analysis####
##Statistics####
e13c_bet_n_07_q_stats <- rwl.stats(e13c_bet_n_07_q) #summary and stats
print(e13c_bet_n_07_q_stats)

e13c_bet_n_07_q_ms <- sens2(e13c_bet_n_07_q) #calculates the mean sensitivity
print(e13c_bet_n_07_q_ms)

e13c_bet_n_07_q_report <- rwl.report(e13c_bet_n_07_q)  #report on rwl
print(e13c_bet_n_07_q_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_07_q) #creates a segment plot
spag.plot(e13c_bet_n_07_q, zfac=0.007,) #creates a spaghetti plot
title(main = "e13cbetn07q", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.6) #add title

##Analysis####

e13c_bet_n_07_q_inter <- interseries.cor(e13c_bet_n_07_q, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13c_bet_n_07_q_inter)

###General correlation####

corr.rwl.seg(rwl = e13c_bet_n_07_q, seg.length = 8, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13cbetn07q", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

