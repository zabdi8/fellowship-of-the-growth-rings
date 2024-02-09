#Load packages####
library(dplR)

#Load the data####
e13c_bet_n_07_o01 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_07/O/E13C.Bet.n.07.o01.csv")
e13c_bet_n_07_o05 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_07/O/E13C.Bet.n.07.o05.csv")
e13c_bet_n_07_o08 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_07/O/E13C.Bet.n.07.o08.csv")


#remove the core year

e13c_bet_n_07_o01 <- e13c_bet_n_07_o01[-1, ]
e13c_bet_n_07_o05 <- e13c_bet_n_07_o05[-1, ]
e13c_bet_n_07_o08 <- e13c_bet_n_07_o08[-1, ]


#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_07_o01$row_names <- rownames(e13c_bet_n_07_o01)
e13c_bet_n_07_o05$row_names <- rownames(e13c_bet_n_07_o05)
e13c_bet_n_07_o08$row_names <- rownames(e13c_bet_n_07_o08)



# Merge the data frames using Reduce and merge
e13c_bet_n_07_o <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13c_bet_n_07_o01, 
                               e13c_bet_n_07_o05, 
                               e13c_bet_n_07_o08)
)

# Set row names and remove the extra column
rownames(e13c_bet_n_07_o) <- e13c_bet_n_07_o[[common_column]]
e13c_bet_n_07_o[[common_column]] <- NULL

view(e13c_bet_n_07_o)

#export rwl

write.rwl(e13c_bet_n_07_o, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/o/e13c_bet_n_07_o", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#Data Analysis####
##Statistics####
e13c_bet_n_07_o_stats <- rwl.stats(e13c_bet_n_07_o) #summary and stats
print(e13c_bet_n_07_o_stats)

e13c_bet_n_07_o_ms <- sens2(e13c_bet_n_07_o) #calculates the mean sensitivity
print(e13c_bet_n_07_o_ms)

e13c_bet_n_07_o_report <- rwl.report(e13c_bet_n_07_o)  #report on rwl #there's too few observations to make the analysis
print(e13c_bet_n_07_o_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_07_o) #creates a segment plot
spag.plot(e13c_bet_n_07_o, zfac=0.007,) #creates a spaghetti plot
title(main = "e13cbetn07o", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

##Analysis####

e13c_bet_n_07_o_inter <- interseries.cor(e13c_bet_n_07_o, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13c_bet_n_07_o_inter)

###General correlation####

corr.rwl.seg(rwl = e13c_bet_n_07_o, seg.length = 8, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13cbetn07o", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

