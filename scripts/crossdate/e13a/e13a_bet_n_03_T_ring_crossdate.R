#Load packages####
library(dplR)

#Load the data####
e13a_bet_n_03_t01 <- csv2rwl("data/ring_data/wedging_rings/e13a/e13a.bet.n/e13a_bet_n_03/t/E13A.Bet.n.03.t01.csv")
e13a_bet_n_03_t02 <- csv2rwl("data/ring_data/wedging_rings/e13a/e13a.bet.n/e13a_bet_n_03/t/E13A.Bet.n.03.t02.csv")
e13a_bet_n_03_t03 <- csv2rwl("data/ring_data/wedging_rings/e13a/e13a.bet.n/e13a_bet_n_03/t/E13A.Bet.n.03.t03.csv")
e13a_bet_n_03_t04 <- csv2rwl("data/ring_data/wedging_rings/e13a/e13a.bet.n/e13a_bet_n_03/t/E13A.Bet.n.03.t04.csv")
e13a_bet_n_03_t05 <- csv2rwl("data/ring_data/wedging_rings/e13a/e13a.bet.n/e13a_bet_n_03/t/E13A.Bet.n.03.t05.csv")

#remove the core year
e13a_bet_n_03_t01 <- e13a_bet_n_03_t01[-1, ]
e13a_bet_n_03_t02 <- e13a_bet_n_03_t02[-1, ]
e13a_bet_n_03_t03 <- e13a_bet_n_03_t03[-1, ]
e13a_bet_n_03_t04 <- e13a_bet_n_03_t04[-1, ]
e13a_bet_n_03_t05 <- e13a_bet_n_03_t05[-1, ]

#merge in a single data frame:####
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13a_bet_n_03_t01$row_names <- rownames(e13a_bet_n_03_t01)
e13a_bet_n_03_t02$row_names <- rownames(e13a_bet_n_03_t02)
e13a_bet_n_03_t03$row_names <- rownames(e13a_bet_n_03_t03)
e13a_bet_n_03_t04$row_names <- rownames(e13a_bet_n_03_t04)
e13a_bet_n_03_t05$row_names <- rownames(e13a_bet_n_03_t05)

# Merge the data frames using Reduce and merge
e13a_bet_n_03_t <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13a_bet_n_03_t01, 
                               e13a_bet_n_03_t02,
                               e13a_bet_n_03_t03,
                               e13a_bet_n_03_t04,
                               e13a_bet_n_03_t05)
                          )

# Set row names and remove the extra column
rownames(e13a_bet_n_03_t) <- e13a_bet_n_03_t[[common_column]]
e13a_bet_n_03_t[[common_column]] <- NULL

View(e13a_bet_n_03_t)

#export rwl

write.rwl(e13a_bet_n_03_t, "data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03/e13a_bet_n_03_t.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Data Analysis####
##Statistics####
e13a_bet_n_03_t_stats <- rwl.stats(e13a_bet_n_03_t) #summary and stats
print(e13a_bet_n_03_t_stats)

e13a_bet_n_03_t_ms <- sens2(e13a_bet_n_03_t) #calculates the mean sensitivity
print(e13a_bet_n_03_t_ms)

e13a_bet_n_03_t_report <- rwl.report(e13a_bet_n_03_t)  #report on rwl
print(e13a_bet_n_03_t_report)

##Cross-dating and alignment####

#shorten the name
e13a_bet_n_03_t_short <- e13a_bet_n_03_t
colnames(e13a_bet_n_03_t)
new_colnames <- sub("^E13ABetn03", "", colnames(e13a_bet_n_03_t_short))

# Assign the new column names to the data frame
colnames(e13a_bet_n_03_t_short) <- new_colnames

head(e13a_bet_n_03_t_short)
colnames(e13a_bet_n_03_t_short)

#graphs
seg.plot(e13a_bet_n_03_t_short) #creates a segment plot
spag.plot(e13a_bet_n_03_t_short, zfac=0.009,) #creates a spaghetti plot
title(main = "e13abetn03s", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13a_bet_n_03_t_inter <- interseries.cor(e13a_bet_n_03_t, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13a_bet_n_03_t_inter)

###General correlation####

corr.rwl.seg(rwl = e13a_bet_n_03_t, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, 
             biweight = FALSE, method = c("spearman"), 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13abetn03s", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title