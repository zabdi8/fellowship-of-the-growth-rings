#Load packages####
library(dplR)
library(treeclim)
library(ggplot2)

#Load the data####
e13a_bet_n_02_u01 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/u/E13A.Bet.n.02.u01.csv")
e13a_bet_n_02_u08 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/u/E13A.Bet.n.02.u08.csv")
e13a_bet_n_02_u14 <- csv2rwl("data/ring_data/raw/e13a/e13a.bet.n/e13a_bet_n_02/u/E13A.Bet.n.02.u14.csv")



#remove the core year

e13a_bet_n_02_u01 <- e13a_bet_n_02_u01[-1, ]
e13a_bet_n_02_u08 <- e13a_bet_n_02_u08[-1, ]
e13a_bet_n_02_u14 <- e13a_bet_n_02_u14[-1, ]



#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13a_bet_n_02_u01$row_names <- rownames(e13a_bet_n_02_u01)
e13a_bet_n_02_u08$row_names <- rownames(e13a_bet_n_02_u08)
e13a_bet_n_02_u14$row_names <- rownames(e13a_bet_n_02_u14)

# Merge the data frames using Reduce and merge
e13a_bet_n_02_u <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13a_bet_n_02_u01, 
                               e13a_bet_n_02_u08,
                               e13a_bet_n_02_u14)
)

# Set row names and remove the extra column
rownames(e13a_bet_n_02_u) <- e13a_bet_n_02_u[[common_column]]
e13a_bet_n_02_u[[common_column]] <- NULL

View(e13a_bet_n_02_u)

#export rwl

write.rwl(e13a_bet_n_02_u, "data/ring_data/aligned/e13a/e13c.bet.n/e13a_bet_n_02/e13a_bet_n_02_u.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#Data Analysis####
##Statistics####
e13a_bet_n_02_u_stats <- rwl.stats(e13a_bet_n_02_u) #summary and stats
print(e13a_bet_n_02_u_stats)

e13a_bet_n_02_u_ms <- sens2(e13a_bet_n_02_u) #calculates the mean sensitivity
print(e13a_bet_n_02_u_ms)

e13a_bet_n_02_u_report <- rwl.report(e13a_bet_n_02_u)  #report on rwl
print(e13a_bet_n_02_u_report)

##Cross-dating and alignment####

#shorten the name
e13a_bet_n_02_u_short <- e13a_bet_n_02_u
colnames(e13a_bet_n_02_u)
new_colnames <- sub("^E13ABetn02", "", colnames(e13a_bet_n_02_u_short))

# Assign the new column names to the data frame
colnames(e13a_bet_n_02_u_short) <- new_colnames

head(e13a_bet_n_02_u_short)
colnames(e13a_bet_n_02_u_short)

#graphs
seg.plot(e13a_bet_n_02_u_short) #creates a segment plot
spag.plot(e13a_bet_n_02_u_short, zfac=0.009,) #creates a spaghetti plot
title(main = "e13abetn02s", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13a_bet_n_02_u_inter <- interseries.cor(e13a_bet_n_02_u, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13a_bet_n_02_u_inter)

###General correlation####

corr.rwl.seg(rwl = e13a_bet_n_02_u, seg.length = 8, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, 
             biweight = FALSE, method = c("spearman"), 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13abetn02s", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

