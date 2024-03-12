#Load packages####
library(dplR)

#Load the data####
e13a_bet_n_02_s00 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/s/E13A.Bet.n.02.s00.csv")
e13a_bet_n_02_s01 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/s/E13A.Bet.n.02.s01.csv")
e13a_bet_n_02_s02 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/s/E13A.Bet.n.02.s02.csv")
e13a_bet_n_02_s03 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/s/E13A.Bet.n.02.s03.csv")
e13a_bet_n_02_s04 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/s/E13A.Bet.n.02.s04.csv")
e13a_bet_n_02_s05 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/s/E13A.Bet.n.02.s05.csv")
e13a_bet_n_02_s06 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/s/E13A.Bet.n.02.s06.csv")
e13a_bet_n_02_s07 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/s/E13A.Bet.n.02.s07.csv")
e13a_bet_n_02_s08 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/s/E13A.Bet.n.02.s08.csv")
e13a_bet_n_02_s09 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/s/E13A.Bet.n.02.s09.csv")
e13a_bet_n_02_s10 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/s/E13A.Bet.n.02.s10.csv")
e13a_bet_n_02_s20 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/s/E13A.Bet.n.02.s20.csv")

#remove the core year
e13a_bet_n_02_s00 <- e13a_bet_n_02_s00[-1, ]
e13a_bet_n_02_s01 <- e13a_bet_n_02_s01[-1, ]
e13a_bet_n_02_s02 <- e13a_bet_n_02_s02[-1, ]
e13a_bet_n_02_s03 <- e13a_bet_n_02_s03[-1, ]
e13a_bet_n_02_s04 <- e13a_bet_n_02_s04[-1, ]
e13a_bet_n_02_s05 <- e13a_bet_n_02_s05[-1, ]
e13a_bet_n_02_s06 <- e13a_bet_n_02_s06[-1, ]
e13a_bet_n_02_s07 <- e13a_bet_n_02_s07[-1, ]
e13a_bet_n_02_s08 <- e13a_bet_n_02_s08[-1, ]
e13a_bet_n_02_s09 <- e13a_bet_n_02_s09[-1, ]
e13a_bet_n_02_s10 <- e13a_bet_n_02_s10[-1, ]
e13a_bet_n_02_s20 <- e13a_bet_n_02_s20[-1, ]

#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13a_bet_n_02_s00$row_names <- rownames(e13a_bet_n_02_s00)
e13a_bet_n_02_s01$row_names <- rownames(e13a_bet_n_02_s01)
e13a_bet_n_02_s02$row_names <- rownames(e13a_bet_n_02_s02)
e13a_bet_n_02_s03$row_names <- rownames(e13a_bet_n_02_s03)
e13a_bet_n_02_s04$row_names <- rownames(e13a_bet_n_02_s04)
e13a_bet_n_02_s05$row_names <- rownames(e13a_bet_n_02_s05)
e13a_bet_n_02_s06$row_names <- rownames(e13a_bet_n_02_s06)
e13a_bet_n_02_s07$row_names <- rownames(e13a_bet_n_02_s07)
e13a_bet_n_02_s08$row_names <- rownames(e13a_bet_n_02_s08)
e13a_bet_n_02_s09$row_names <- rownames(e13a_bet_n_02_s09)
e13a_bet_n_02_s10$row_names <- rownames(e13a_bet_n_02_s10)
e13a_bet_n_02_s20$row_names <- rownames(e13a_bet_n_02_s20)

# Merge the data frames using Reduce and merge
e13a_bet_n_02_s <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13a_bet_n_02_s00,
                               e13a_bet_n_02_s01, 
                               e13a_bet_n_02_s02,
                               e13a_bet_n_02_s03,
                               e13a_bet_n_02_s04,
                               e13a_bet_n_02_s05,
                               e13a_bet_n_02_s06,
                               e13a_bet_n_02_s07,
                               e13a_bet_n_02_s08,
                               e13a_bet_n_02_s09,
                               e13a_bet_n_02_s10,
                               e13a_bet_n_02_s20)
                          )

# Set row names and remove the extra column
rownames(e13a_bet_n_02_s) <- e13a_bet_n_02_s[[common_column]]
e13a_bet_n_02_s[[common_column]] <- NULL

View(e13a_bet_n_02_s)

#export rwl

write.rwl(e13a_bet_n_02_s, "data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02/e13a_bet_n_02_s.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Data Analysis####
##Statistics####
e13a_bet_n_02_s_stats <- rwl.stats(e13a_bet_n_02_s) #summary and stats
print(e13a_bet_n_02_s_stats)

e13a_bet_n_02_s_ms <- sens2(e13a_bet_n_02_s) #calculates the mean sensitivity
print(e13a_bet_n_02_s_ms)

e13a_bet_n_02_s_report <- rwl.report(e13a_bet_n_02_s)  #report on rwl
print(e13a_bet_n_02_s_report)

##Cross-dating and alignment####

#shorten the name
e13a_bet_n_02_s_short <- e13a_bet_n_02_s
colnames(e13a_bet_n_02_s)
new_colnames <- sub("^E13ABetn02", "", colnames(e13a_bet_n_02_s_short))

# Assign the new column names to the data frame
colnames(e13a_bet_n_02_s_short) <- new_colnames

head(e13a_bet_n_02_s_short)
colnames(e13a_bet_n_02_s_short)

#graphs
seg.plot(e13a_bet_n_02_s_short) #creates a segment plot
spag.plot(e13a_bet_n_02_s_short, zfac=0.01) #creates a spaghetti plot
title(main = "e13abetn02s", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13a_bet_n_02_s_inter <- interseries.cor(e13a_bet_n_02_s, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13a_bet_n_02_s_inter)

###General correlation####

corr.rwl.seg(rwl = e13a_bet_n_02_s, seg.length = 8, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, 
             biweight = FALSE, method = c("spearman"), 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13abetn02s", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

