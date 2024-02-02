#Load packages####
library(dplR)
library(treeclim)
library(ggplot2)

#Load the data####
e13d_bet_n_02_u01 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_02/u/E13D.Bet.n.02.u01.csv")
e13d_bet_n_02_u02 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_02/u/E13D.Bet.n.02.u02.csv")
e13d_bet_n_02_u03 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_02/u/E13D.Bet.n.02.u03.csv")
e13d_bet_n_02_u04 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_02/u/E13D.Bet.n.02.u04.csv")




#remove the core year
e13d_bet_n_02_u01 <- e13d_bet_n_02_u01[-1, ]
e13d_bet_n_02_u02 <- e13d_bet_n_02_u02[-1, ]
e13d_bet_n_02_u03 <- e13d_bet_n_02_u03[-1, ]
e13d_bet_n_02_u04 <- e13d_bet_n_02_u04[-1, ]

#merge in a single data frame:####
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13d_bet_n_02_u01$row_names <- rownames(e13d_bet_n_02_u01)
e13d_bet_n_02_u02$row_names <- rownames(e13d_bet_n_02_u02)
e13d_bet_n_02_u03$row_names <- rownames(e13d_bet_n_02_u03)
e13d_bet_n_02_u04$row_names <- rownames(e13d_bet_n_02_u04)

# Merge the data frames using Reduce and merge
e13d_bet_n_02_u <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13d_bet_n_02_u01, 
                               e13d_bet_n_02_u02,
                               e13d_bet_n_02_u03,
                               e13d_bet_n_02_u04)
                          )

# Set row names and remove the extra column
rownames(e13d_bet_n_02_u) <- e13d_bet_n_02_u[[common_column]]
e13d_bet_n_02_u[[common_column]] <- NULL

View(e13d_bet_n_02_u)

#export rwl

write.rwl(e13d_bet_n_02_u, "data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_02/e13d_bet_n_02_u.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#Data Analysis####
##Statistics####
e13d_bet_n_02_u_stats <- rwl.stats(e13d_bet_n_02_u) #summary and stats
print(e13d_bet_n_02_u_stats)

e13d_bet_n_02_u_ms <- sens2(e13d_bet_n_02_u) #calculates the mean sensitivity
print(e13d_bet_n_02_u_ms)

e13d_bet_n_02_u_report <- rwl.report(e13d_bet_n_02_u)  #report on rwl
print(e13d_bet_n_02_u_report)

##Cross-dating and alignment####

#shorten the name
e13d_bet_n_02_u_short <- e13d_bet_n_02_u
colnames(e13d_bet_n_02_u)
new_colnames <- sub("^E13DBetn02", "", colnames(e13d_bet_n_02_u_short))

# Assign the new column names to the data frame
colnames(e13d_bet_n_02_u_short) <- new_colnames

head(e13d_bet_n_02_u_short)
colnames(e13d_bet_n_02_u_short)

#graphs
seg.plot(e13d_bet_n_02_u_short) #creates a segment plot
title(main = "E13DBetn02u", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.2) #add title
spag.plot(e13d_bet_n_02_u_short, zfac=0.009,) #creates a spaghetti plot
title(main = "E13DBetn02u", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13d_bet_n_02_u_inter <- interseries.cor(e13d_bet_n_02_u_short, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13d_bet_n_02_u_inter)

###General correlation####

corr.rwl.seg(rwl = e13d_bet_n_02_u_short, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, 
             biweight = FALSE, method = c("spearman"), 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

title(main = "E13DBetn02u", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title