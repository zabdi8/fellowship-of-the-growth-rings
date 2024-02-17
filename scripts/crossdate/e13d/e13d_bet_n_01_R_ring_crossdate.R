#Load packages####
library(dplR)

#Load the data####
e13d_bet_n_01_r01 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_01/r/E13D.Bet.n.01.r01.csv")
e13d_bet_n_01_r07 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_01/r/E13D.Bet.n.01.r07.csv")
e13d_bet_n_01_r12 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_01/r/E13D.Bet.n.01.r12.csv")



#remove the core year
e13d_bet_n_01_r01 <- e13d_bet_n_01_r01[-1, ]
e13d_bet_n_01_r07 <- e13d_bet_n_01_r07[-1, ]
e13d_bet_n_01_r12 <- e13d_bet_n_01_r12[-1, ]

#merge in a single data frame:####
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13d_bet_n_01_r01$row_names <- rownames(e13d_bet_n_01_r01)
e13d_bet_n_01_r07$row_names <- rownames(e13d_bet_n_01_r07)
e13d_bet_n_01_r12$row_names <- rownames(e13d_bet_n_01_r12)

# Merge the data frames using Reduce and merge
e13d_bet_n_01_r <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13d_bet_n_01_r01, 
                               e13d_bet_n_01_r07,
                               e13d_bet_n_01_r12)
                          )

# Set row names and remove the extra column
rownames(e13d_bet_n_01_r) <- e13d_bet_n_01_r[[common_column]]
e13d_bet_n_01_r[[common_column]] <- NULL

View(e13d_bet_n_01_r)

#export rwl

write.rwl(e13d_bet_n_01_r, "data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01/e13d_bet_n_01_r.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#Data Analysis####
##Statistics####
e13d_bet_n_01_r_stats <- rwl.stats(e13d_bet_n_01_r) #summary and stats
print(e13d_bet_n_01_r_stats)

e13d_bet_n_01_r_ms <- sens2(e13d_bet_n_01_r) #calculates the mean sensitivity
print(e13d_bet_n_01_r_ms)

e13d_bet_n_01_r_report <- rwl.report(e13d_bet_n_01_r)  #report on rwl
print(e13d_bet_n_01_r_report)

##Cross-dating and alignment####

#shorten the name
e13d_bet_n_01_r_short <- e13d_bet_n_01_r
colnames(e13d_bet_n_01_r)
new_colnames <- sub("^E13DBetn01", "", colnames(e13d_bet_n_01_r_short))

# Assign the new column names to the data frame
colnames(e13d_bet_n_01_r_short) <- new_colnames

head(e13d_bet_n_01_r_short)
colnames(e13d_bet_n_01_r_short)

#graphs
seg.plot(e13d_bet_n_01_r_short) #creates a segment plot
title(main = "e13dbetn01r", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.2) #add title
spag.plot(e13d_bet_n_01_r_short, zfac=0.009,) #creates a spaghetti plot
title(main = "e13dbetn01r", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13d_bet_n_01_r_inter <- interseries.cor(e13d_bet_n_01_r, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13d_bet_n_01_r_inter)

###General correlation####

corr.rwl.seg(rwl = e13d_bet_n_01_r, seg.length = 8, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, 
             biweight = FALSE, method = c("spearman"), 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis #too few observations! 

title(main = "e13dbetn01r", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

