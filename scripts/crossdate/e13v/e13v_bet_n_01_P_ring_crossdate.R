#Load packages####
library(dplR)

#Load the data####
e13v_bet_n_01_p01 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/p/E13V.Bet.n.01.p01.csv")
e13v_bet_n_01_p02 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/p/E13V.Bet.n.01.p02.csv")
e13v_bet_n_01_p03 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/p/E13V.Bet.n.01.p03.csv")
e13v_bet_n_01_p04 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/p/E13V.Bet.n.01.p04.csv")
e13v_bet_n_01_p05 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/p/E13V.Bet.n.01.p05.csv")
e13v_bet_n_01_p06 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/p/E13V.Bet.n.01.p06.csv")

#remove the core year
e13v_bet_n_01_p01 <- e13v_bet_n_01_p01[-1, ]
e13v_bet_n_01_p02 <- e13v_bet_n_01_p02[-1, ]
e13v_bet_n_01_p03 <- e13v_bet_n_01_p03[-1, ]
e13v_bet_n_01_p04 <- e13v_bet_n_01_p04[-1, ]
e13v_bet_n_01_p05 <- e13v_bet_n_01_p05[-1, ]
e13v_bet_n_01_p06 <- e13v_bet_n_01_p06[-1, ]

#merge in a single data frame:####
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13v_bet_n_01_p01$row_names <- rownames(e13v_bet_n_01_p01)
e13v_bet_n_01_p02$row_names <- rownames(e13v_bet_n_01_p02)
e13v_bet_n_01_p03$row_names <- rownames(e13v_bet_n_01_p03)
e13v_bet_n_01_p04$row_names <- rownames(e13v_bet_n_01_p04)
e13v_bet_n_01_p05$row_names <- rownames(e13v_bet_n_01_p05)
e13v_bet_n_01_p06$row_names <- rownames(e13v_bet_n_01_p06)

# Merge the data frames using Reduce and merge
e13v_bet_n_01_p <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13v_bet_n_01_p01, 
                               e13v_bet_n_01_p02,
                               e13v_bet_n_01_p03,
                               e13v_bet_n_01_p04,
                               e13v_bet_n_01_p05,
                               e13v_bet_n_01_p06)
                          )

# Set row names and remove the extra column
rownames(e13v_bet_n_01_p) <- e13v_bet_n_01_p[[common_column]]
e13v_bet_n_01_p[[common_column]] <- NULL

View(e13v_bet_n_01_p)

#export rwl

write.rwl(e13v_bet_n_01_p, "data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01/e13v_bet_n_01_p.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Data Analysis####
##Statistics####
e13v_bet_n_01_p_stats <- rwl.stats(e13v_bet_n_01_p) #summary and stats
print(e13v_bet_n_01_p_stats)

e13v_bet_n_01_p_ms <- sens2(e13v_bet_n_01_p) #calculates the mean sensitivity
print(e13v_bet_n_01_p_ms)

e13v_bet_n_01_p_report <- rwl.report(e13v_bet_n_01_p)  #report on rwl
print(e13v_bet_n_01_p_report)

##Cross-dating and alignment####

#shorten the name
e13v_bet_n_01_p_short <- e13v_bet_n_01_p
colnames(e13v_bet_n_01_p)
new_colnames <- sub("^E13VBetn01", "", colnames(e13v_bet_n_01_p_short))

# Assign the new column names to the data frame
colnames(e13v_bet_n_01_p_short) <- new_colnames

head(e13v_bet_n_01_p_short)
colnames(e13v_bet_n_01_p_short)

#graphs
seg.plot(e13v_bet_n_01_p_short) #creates a segment plot
title(main = "e13vBetn03p", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
spag.plot(e13v_bet_n_01_p_short, zfac=0.02,) #creates a spaghetti plot
title(main = "e13vBetn03p", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13v_bet_n_01_p_inter <- interseries.cor(e13v_bet_n_01_p_short, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13v_bet_n_01_p_inter)

###General correlation####

corr.rwl.seg(rwl = e13v_bet_n_01_p_short, seg.length = 4, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, 
             biweight = FALSE, method = c("spearman"), 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

title(main = "e13vBetn03p", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title