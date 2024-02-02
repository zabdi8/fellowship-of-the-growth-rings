#Load packages####
library(dplR)
library(treeclim)
library(ggplot2)

#Load the data####
e13d_bet_n_03_v01 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_03/v/E13D.Bet.n.03.v01.csv")
e13d_bet_n_03_v02 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_03/v/E13D.Bet.n.03.v02.csv")
e13d_bet_n_03_v04 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_03/v/E13D.Bet.n.03.v04.csv")




#remove the core year
e13d_bet_n_03_v01 <- e13d_bet_n_03_v01[-1, ]
e13d_bet_n_03_v02 <- e13d_bet_n_03_v02[-1, ]
e13d_bet_n_03_v04 <- e13d_bet_n_03_v04[-1, ]

#merge in a single data frame:####
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13d_bet_n_03_v01$row_names <- rownames(e13d_bet_n_03_v01)
e13d_bet_n_03_v02$row_names <- rownames(e13d_bet_n_03_v02)
e13d_bet_n_03_v04$row_names <- rownames(e13d_bet_n_03_v04)

# Merge the data frames using Reduce and merge
e13d_bet_n_03_v <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13d_bet_n_03_v01, 
                               e13d_bet_n_03_v02,
                               e13d_bet_n_03_v04)
)

# Set row names and remove the extra column
rownames(e13d_bet_n_03_v) <- e13d_bet_n_03_v[[common_column]]
e13d_bet_n_03_v[[common_column]] <- NULL

View(e13d_bet_n_03_v)

#export rwl

write.rwl(e13d_bet_n_03_v, "data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03/e13d_bet_n_03_v.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#Data Analysis####
##Statistics####
e13d_bet_n_03_v_stats <- rwl.stats(e13d_bet_n_03_v) #summary and stats
print(e13d_bet_n_03_v_stats)

e13d_bet_n_03_v_ms <- sens2(e13d_bet_n_03_v) #calculates the mean sensitivity
print(e13d_bet_n_03_v_ms)

e13d_bet_n_03_v_report <- rwl.report(e13d_bet_n_03_v)  #report on rwl
print(e13d_bet_n_03_v_report)

##Cross-dating and alignment####

#shorten the name
e13d_bet_n_03_v_short <- e13d_bet_n_03_v
colnames(e13d_bet_n_03_v)
new_colnames <- sub("^E13DBetn03", "", colnames(e13d_bet_n_03_v_short))

# Assign the new column names to the data frame
colnames(e13d_bet_n_03_v_short) <- new_colnames

head(e13d_bet_n_03_v_short)
colnames(e13d_bet_n_03_v_short)

#graphs
seg.plot(e13d_bet_n_03_v_short) #creates a segment plot
title(main = "E13DBetn03v", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
spag.plot(e13d_bet_n_03_v_short, zfac=0.005,) #creates a spaghetti plot
title(main = "E13DBetn03v", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13d_bet_n_03_v_inter <- interseries.cor(e13d_bet_n_03_v_short, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13d_bet_n_03_v_inter)

###General correlation####

corr.rwl.seg(rwl = e13d_bet_n_03_v_short, seg.length = 4, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, 
             biweight = FALSE, method = c("spearman"), 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

title(main = "E13DBetn03v", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

