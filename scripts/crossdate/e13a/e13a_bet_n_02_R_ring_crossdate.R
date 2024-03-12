#Load packages####
library(dplR)

#Load the data####
e13a_bet_n_02_r01 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/r/E13A.Bet.n.02.r01.csv")
e13a_bet_n_02_r02 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/r/E13A.Bet.n.02.r02.csv")
e13a_bet_n_02_r03 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/r/E13A.Bet.n.02.r03.csv")
e13a_bet_n_02_r04 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/r/E13A.Bet.n.02.r04.csv")


#remove the core year

e13a_bet_n_02_r01 <- e13a_bet_n_02_r01[-1, ]
e13a_bet_n_02_r02 <- e13a_bet_n_02_r02[-1, ]
e13a_bet_n_02_r03 <- e13a_bet_n_02_r03[-1, ]
e13a_bet_n_02_r04 <- e13a_bet_n_02_r04[-1, ]



#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13a_bet_n_02_r01$row_names <- rownames(e13a_bet_n_02_r01)
e13a_bet_n_02_r02$row_names <- rownames(e13a_bet_n_02_r02)
e13a_bet_n_02_r03$row_names <- rownames(e13a_bet_n_02_r03)
e13a_bet_n_02_r04$row_names <- rownames(e13a_bet_n_02_r04)

# Merge the data frames using Reduce and merge
e13a_bet_n_02_r <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13a_bet_n_02_r01, 
                               e13a_bet_n_02_r02,
                               e13a_bet_n_02_r03,
                               e13a_bet_n_02_r04)
                          )

# Set row names and remove the extra column
rownames(e13a_bet_n_02_r) <- e13a_bet_n_02_r[[common_column]]
e13a_bet_n_02_r[[common_column]] <- NULL

View(e13a_bet_n_02_r)

#export rwl

write.rwl(e13a_bet_n_02_r, "data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02/e13a_bet_n_02_r.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Data Analysis####
##Statistics####
e13a_bet_n_02_r_stats <- rwl.stats(e13a_bet_n_02_r) #summary and stats
print(e13a_bet_n_02_r_stats)

e13a_bet_n_02_r_ms <- sens2(e13a_bet_n_02_r) #calculates the mean sensitivity
print(e13a_bet_n_02_r_ms)

e13a_bet_n_02_r_report <- rwl.report(e13a_bet_n_02_r)  #report on rwl
print(e13a_bet_n_02_r_report)

##Cross-dating and alignment####

#shorten the name
e13a_bet_n_02_r_short <- e13a_bet_n_02_r
colnames(e13a_bet_n_02_r)
new_colnames <- sub("^E13ABetn02", "", colnames(e13a_bet_n_02_r_short))

# Assign the new column names to the data frame
colnames(e13a_bet_n_02_r_short) <- new_colnames

head(e13a_bet_n_02_r_short)
colnames(e13a_bet_n_02_r_short)

#graphs
seg.plot(e13a_bet_n_02_r_short) #creates a segment plot
spag.plot(e13a_bet_n_02_r_short, zfac=0.009,) #creates a spaghetti plot
title(main = "e13abetn01r", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13a_bet_n_02_r_inter <- interseries.cor(e13a_bet_n_02_r, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13a_bet_n_02_r_inter)

###General correlation####

corr.rwl.seg(rwl = e13a_bet_n_02_r, seg.length = 8, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13abetn01r", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

