#Load packages####
library(dplR)

#Load the data####
e13a_bet_n_03_s01 <- csv2rwl("data/ring_data/wedging_rings/e13a/e13a.bet.n/e13a_bet_n_03/s/E13A.Bet.n.03.s01.csv")
e13a_bet_n_03_s02 <- csv2rwl("data/ring_data/wedging_rings/e13a/e13a.bet.n/e13a_bet_n_03/s/E13A.Bet.n.03.s02.csv")
e13a_bet_n_03_s03 <- csv2rwl("data/ring_data/wedging_rings/e13a/e13a.bet.n/e13a_bet_n_03/s/E13A.Bet.n.03.s03.csv")
e13a_bet_n_03_s04 <- csv2rwl("data/ring_data/wedging_rings/e13a/e13a.bet.n/e13a_bet_n_03/s/E13A.Bet.n.03.s04.csv")
e13a_bet_n_03_s05 <- csv2rwl("data/ring_data/wedging_rings/e13a/e13a.bet.n/e13a_bet_n_03/s/E13A.Bet.n.03.s05.csv")
e13a_bet_n_03_s06 <- csv2rwl("data/ring_data/wedging_rings/e13a/e13a.bet.n/e13a_bet_n_03/s/E13A.Bet.n.03.s06.csv")

#remove the core year
e13a_bet_n_03_s01 <- e13a_bet_n_03_s01[-1, ]
e13a_bet_n_03_s02 <- e13a_bet_n_03_s02[-1, ]
e13a_bet_n_03_s03 <- e13a_bet_n_03_s03[-1, ]
e13a_bet_n_03_s04 <- e13a_bet_n_03_s04[-1, ]
e13a_bet_n_03_s05 <- e13a_bet_n_03_s05[-1, ]
e13a_bet_n_03_s06 <- e13a_bet_n_03_s06[-1, ]

#merge in a single data frame:####
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13a_bet_n_03_s01$row_names <- rownames(e13a_bet_n_03_s01)
e13a_bet_n_03_s02$row_names <- rownames(e13a_bet_n_03_s02)
e13a_bet_n_03_s03$row_names <- rownames(e13a_bet_n_03_s03)
e13a_bet_n_03_s04$row_names <- rownames(e13a_bet_n_03_s04)
e13a_bet_n_03_s05$row_names <- rownames(e13a_bet_n_03_s05)
e13a_bet_n_03_s06$row_names <- rownames(e13a_bet_n_03_s06)

# Merge the data frames using Reduce and merge
e13a_bet_n_03_s <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13a_bet_n_03_s01, 
                               e13a_bet_n_03_s02,
                               e13a_bet_n_03_s03,
                               e13a_bet_n_03_s04,
                               e13a_bet_n_03_s05,
                               e13a_bet_n_03_s06)
                          )

# Set row names and remove the extra column
rownames(e13a_bet_n_03_s) <- e13a_bet_n_03_s[[common_column]]
e13a_bet_n_03_s[[common_column]] <- NULL

View(e13a_bet_n_03_s)

#export rwl

write.rwl(e13a_bet_n_03_s, "data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03/e13a_bet_n_03_s.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Data Analysis####
##Statistics####
e13a_bet_n_03_s_stats <- rwl.stats(e13a_bet_n_03_s) #summary and stats
print(e13a_bet_n_03_s_stats)

e13a_bet_n_03_s_ms <- sens2(e13a_bet_n_03_s) #calculates the mean sensitivity
print(e13a_bet_n_03_s_ms)

e13a_bet_n_03_s_report <- rwl.report(e13a_bet_n_03_s)  #report on rwl
print(e13a_bet_n_03_s_report)

##Cross-dating and alignment####

#shorten the name
e13a_bet_n_03_s_short <- e13a_bet_n_03_s
colnames(e13a_bet_n_03_s)
new_colnames <- sub("^E13ABetn03", "", colnames(e13a_bet_n_03_s_short))

# Assign the new column names to the data frame
colnames(e13a_bet_n_03_s_short) <- new_colnames

head(e13a_bet_n_03_s_short)
colnames(e13a_bet_n_03_s_short)

#graphs
seg.plot(e13a_bet_n_03_s_short) #creates a segment plot
spag.plot(e13a_bet_n_03_s_short, zfac=0.009,) #creates a spaghetti plot
title(main = "e13abetn03s", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13a_bet_n_03_s_inter <- interseries.cor(e13a_bet_n_03_s, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13a_bet_n_03_s_inter)

###General correlation####

corr.rwl.seg(rwl = e13a_bet_n_03_s, seg.length = 8, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, 
             biweight = FALSE, method = c("spearman"), 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13abetn03s", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

