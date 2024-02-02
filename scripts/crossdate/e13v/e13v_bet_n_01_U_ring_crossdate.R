#Load packages####
library(dplR)
library(treeclim)
library(ggplot2)

#Load the data####
e13v_bet_n_01_u01 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_01/u/E13V.Bet.n.01.u01.csv")
e13v_bet_n_01_u02 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_01/u/E13V.Bet.n.01.u02.csv")
e13v_bet_n_01_u03 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_01/u/E13V.Bet.n.01.u03.csv")
e13v_bet_n_01_u04 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_01/u/E13V.Bet.n.01.u04.csv")

#remove the core year
e13v_bet_n_01_u01 <- e13v_bet_n_01_u01[-1, ]
e13v_bet_n_01_u02 <- e13v_bet_n_01_u02[-1, ]
e13v_bet_n_01_u03 <- e13v_bet_n_01_u03[-1, ]
e13v_bet_n_01_u04 <- e13v_bet_n_01_u04[-1, ]

#merge in a single data frame:####
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13v_bet_n_01_u01$row_names <- rownames(e13v_bet_n_01_u01)
e13v_bet_n_01_u02$row_names <- rownames(e13v_bet_n_01_u02)
e13v_bet_n_01_u03$row_names <- rownames(e13v_bet_n_01_u03)
e13v_bet_n_01_u04$row_names <- rownames(e13v_bet_n_01_u04)

# Merge the data frames using Reduce and merge
e13v_bet_n_01_u <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13v_bet_n_01_u01, 
                               e13v_bet_n_01_u02,
                               e13v_bet_n_01_u03,
                               e13v_bet_n_01_u04)
                          )

# Set row names and remove the extra column
rownames(e13v_bet_n_01_u) <- e13v_bet_n_01_u[[common_column]]
e13v_bet_n_01_u[[common_column]] <- NULL

View(e13v_bet_n_01_u)

#export rwl

write.rwl(e13v_bet_n_01_u, "data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01/e13v_bet_n_01_u.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#Data Analysis####
##Statistics####
e13v_bet_n_01_u_stats <- rwl.stats(e13v_bet_n_01_u) #summary and stats
print(e13v_bet_n_01_u_stats)

e13v_bet_n_01_u_ms <- sens2(e13v_bet_n_01_u) #calculates the mean sensitivity
print(e13v_bet_n_01_u_ms)

e13v_bet_n_01_u_report <- rwl.report(e13v_bet_n_01_u)  #report on rwl
print(e13v_bet_n_01_u_report)

##Cross-dating and alignment####

#shorten the name
e13v_bet_n_01_u_short <- e13v_bet_n_01_u
colnames(e13v_bet_n_01_u)
new_colnames <- sub("^E13VBetn01", "", colnames(e13v_bet_n_01_u_short))

# Assign the new column names to the data frame
colnames(e13v_bet_n_01_u_short) <- new_colnames

head(e13v_bet_n_01_u_short)
colnames(e13v_bet_n_01_u_short)

#graphs
seg.plot(e13v_bet_n_01_u_short) #creates a segment plot
title(main = "e13vBetn03u", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
spag.plot(e13v_bet_n_01_u_short, zfac=0.006,) #creates a spaghetti plot
title(main = "e13vBetn03u", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13v_bet_n_01_u_inter <- interseries.cor(e13v_bet_n_01_u_short, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13v_bet_n_01_u_inter)

###General correlation####

corr.rwl.seg(rwl = e13v_bet_n_01_u_short, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, 
             biweight = FALSE, method = c("spearman"), 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

title(main = "e13vBetn03u", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title
