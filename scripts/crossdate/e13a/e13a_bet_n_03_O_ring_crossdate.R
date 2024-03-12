#Load packages####
library(dplR)

#Load the data####
e13a_bet_n_03_o01 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_03/o/E13A.Bet.n.03.o01.csv")
e13a_bet_n_03_o02 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_03/o/E13A.Bet.n.03.o02.csv")
e13a_bet_n_03_o03 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_03/o/E13A.Bet.n.03.o03.csv")
e13a_bet_n_03_o04 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_03/o/E13A.Bet.n.03.o04.csv")

#remove the core year

e13a_bet_n_03_o01 <- e13a_bet_n_03_o01[-1, ]
e13a_bet_n_03_o02 <- e13a_bet_n_03_o02[-1, ]
e13a_bet_n_03_o03 <- e13a_bet_n_03_o03[-1, ]
e13a_bet_n_03_o04 <- e13a_bet_n_03_o04[-1, ]

#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13a_bet_n_03_o01$row_names <- rownames(e13a_bet_n_03_o01)
e13a_bet_n_03_o02$row_names <- rownames(e13a_bet_n_03_o02)
e13a_bet_n_03_o03$row_names <- rownames(e13a_bet_n_03_o03)
e13a_bet_n_03_o04$row_names <- rownames(e13a_bet_n_03_o04)

# Merge the data frames using Reduce and merge
e13a_bet_n_03_o <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13a_bet_n_03_o01, 
                               e13a_bet_n_03_o02,
                               e13a_bet_n_03_o03,
                               e13a_bet_n_03_o04)
                          )

# Set row names and remove the extra column
rownames(e13a_bet_n_03_o) <- e13a_bet_n_03_o[[common_column]]
e13a_bet_n_03_o[[common_column]] <- NULL

View(e13a_bet_n_03_o)

#export rwl

write.rwl(e13a_bet_n_03_o, "data/ring_data/aligned/e13a/e13c.bet.n/e13a_bet_n_03/e13a_bet_n_03_o.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Data Analysis####
##Statistics####
e13a_bet_n_03_o_stats <- rwl.stats(e13a_bet_n_03_o) #summary and stats
print(e13a_bet_n_03_o_stats)

e13a_bet_n_03_o_ms <- sens2(e13a_bet_n_03_o) #calculates the mean sensitivity
print(e13a_bet_n_03_o_ms)

e13a_bet_n_03_o_report <- rwl.report(e13a_bet_n_03_o)  #report on rwl
print(e13a_bet_n_03_o_report)

##Cross-dating and alignment####

#shorten the name
e13a_bet_n_03_o_short <- e13a_bet_n_03_o
colnames(e13a_bet_n_03_o)
new_colnames <- sub("^E13ABetn03", "", colnames(e13a_bet_n_03_o_short))

# Assign the new column names to the data frame
colnames(e13a_bet_n_03_o_short) <- new_colnames

head(e13a_bet_n_03_o_short)
colnames(e13a_bet_n_03_o_short)

#graphs
seg.plot(e13a_bet_n_03_o_short) #creates a segment plot
spag.plot(e13a_bet_n_03_o_short, zfac=0.03,) #creates a spaghetti plot
title(main = "e13abetn03o", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13a_bet_n_03_o_inter <- interseries.cor(e13a_bet_n_03_o, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13a_bet_n_03_o_inter)

###General correlation####

corr.rwl.seg(rwl = e13a_bet_n_03_o, seg.length = 8, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, 
             biweight = FALSE, method = c("spearman"), 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13abetn03o", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

