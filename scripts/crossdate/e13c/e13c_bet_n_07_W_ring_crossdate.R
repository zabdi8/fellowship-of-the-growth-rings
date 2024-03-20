#Load packages####
library(dplR)

#Load the data####
e13c_bet_n_07_w01 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_07/W/E13C.Bet.n.07.w01.csv")
e13c_bet_n_07_w02 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_07/W/E13C.Bet.n.07.w02.csv")
e13c_bet_n_07_w04 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_07/W/E13C.Bet.n.07.w04.csv")


#remove the core year

e13c_bet_n_07_w01 <- e13c_bet_n_07_w01[-1, ]
e13c_bet_n_07_w02 <- e13c_bet_n_07_w02[-1, ]
e13c_bet_n_07_w04 <- e13c_bet_n_07_w04[-1, ]


#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_07_w01$row_names <- rownames(e13c_bet_n_07_w01)
e13c_bet_n_07_w02$row_names <- rownames(e13c_bet_n_07_w02)
e13c_bet_n_07_w04$row_names <- rownames(e13c_bet_n_07_w04)



# Merge the data frames using Reduce and merge
e13c_bet_n_07_w <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13c_bet_n_07_w01, 
                               e13c_bet_n_07_w02, 
                               e13c_bet_n_07_w04)
                          )

# Set row names and remove the extra column
rownames(e13c_bet_n_07_w) <- e13c_bet_n_07_w[[common_column]]
e13c_bet_n_07_w[[common_column]] <- NULL

view(e13c_bet_n_07_w)

#export rwl

write.rwl(e13c_bet_n_07_w, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/w/e13c_bet_n_07_w", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Data Analysis####
##Statistics####
e13c_bet_n_07_w_stats <- rwl.stats(e13c_bet_n_07_w) #summary and stats
print(e13c_bet_n_07_w_stats)

e13c_bet_n_07_w_ms <- sens2(e13c_bet_n_07_w) #calculates the mean sensitivity
print(e13c_bet_n_07_w_ms)

e13c_bet_n_07_w_report <- rwl.report(e13c_bet_n_07_w)  #report on rwl
print(e13c_bet_n_07_w_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_07_w) #creates a segment plot
spag.plot(e13c_bet_n_07_w, zfac=0.007,) #creates a spaghetti plot
title(main = "e13cbetn07w", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.6) #add title

##Analysis####

e13c_bet_n_07_w_inter <- interseries.cor(e13c_bet_n_07_w, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13c_bet_n_07_w_inter)

###General correlation####

corr.rwl.seg(rwl = e13c_bet_n_07_w, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13cbetn07w", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

