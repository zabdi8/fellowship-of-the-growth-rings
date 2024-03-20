#Load packages####
library(dplR)

#Load the data####
e13c_bet_n_01_r01 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_01/r/e13c.Bet.n.01.r01.csv")
e13c_bet_n_01_r02 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_01/r/e13c.Bet.n.01.r02.csv")
e13c_bet_n_01_r03 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_01/r/e13c.Bet.n.01.r03.csv")
e13c_bet_n_01_r04 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_01/r/e13c.Bet.n.01.r04.csv")
e13c_bet_n_01_r05 <- csv2rwl("data/ring_data/wedging_rings/e13c/e13c.bet.n/e13c_bet_n_01/r/e13c.Bet.n.01.r05.csv")



#remove the core year

e13c_bet_n_01_r01 <- e13c_bet_n_01_r01[-1, ]
e13c_bet_n_01_r02 <- e13c_bet_n_01_r02[-1, ]
e13c_bet_n_01_r03 <- e13c_bet_n_01_r03[-1, ]
e13c_bet_n_01_r04 <- e13c_bet_n_01_r04[-1, ]
e13c_bet_n_01_r05 <- e13c_bet_n_01_r05[-1, ]

#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_01_r01$row_names <- rownames(e13c_bet_n_01_r01)
e13c_bet_n_01_r02$row_names <- rownames(e13c_bet_n_01_r02)
e13c_bet_n_01_r03$row_names <- rownames(e13c_bet_n_01_r03)
e13c_bet_n_01_r04$row_names <- rownames(e13c_bet_n_01_r04)
e13c_bet_n_01_r05$row_names <- rownames(e13c_bet_n_01_r05)



# Merge the data frames using Reduce and merge
e13c_bet_n_01_r <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13c_bet_n_01_r01, 
                               e13c_bet_n_01_r02,
                               e13c_bet_n_01_r03,
                               e13c_bet_n_01_r04,
                               e13c_bet_n_01_r05)
                          )

# Set row names and remove the extra column
rownames(e13c_bet_n_01_r) <- e13c_bet_n_01_r[[common_column]]
e13c_bet_n_01_r[[common_column]] <- NULL

View(e13c_bet_n_01_r)

#export rwl

write.rwl(e13c_bet_n_01_r, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/e13c_bet_n_01_r.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Data Analysis####
##Statistics####
e13c_bet_n_01_r_stats <- rwl.stats(e13c_bet_n_01_r) #summary and stats
print(e13c_bet_n_01_r_stats)

e13c_bet_n_01_r_ms <- sens2(e13c_bet_n_01_r) #calculates the mean sensitivity
print(e13c_bet_n_01_r_ms)

e13c_bet_n_01_r_report <- rwl.report(e13c_bet_n_01_r)  #report on rwl
print(e13c_bet_n_01_r_report)

##Cross-dating and alignment####

#shorten the name
e13c_bet_n_01_r_short <- e13c_bet_n_01_r
colnames(e13c_bet_n_01_r)
new_colnames <- sub("^e13cBetn01", "", colnames(e13c_bet_n_01_r_short))

# Assign the new column names to the data frame
colnames(e13c_bet_n_01_r_short) <- new_colnames

head(e13c_bet_n_01_r_short)
View(e13c_bet_n_01_r_short)

#graphs
seg.plot(e13c_bet_n_01_r_short) #creates a segment plot
title(main = "e13cbetn01r", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13c_bet_n_01_r_short, zfac=0.01) #creates a spaghetti plot
title(main = "e13cbetn01r", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13c_bet_n_01_r_inter <- interseries.cor(e13c_bet_n_01_r, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13c_bet_n_01_r_inter)

###General correlation####

e13c_bet_n_01_r_corr_seg <- corr.rwl.seg(rwl = e13c_bet_n_01_r, seg.length = 8, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13cbetn01r", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title