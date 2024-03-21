#Load packages####
library(dplR)

#Load the data####
e13v_bet_n_01_o01 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/o/E13V.Bet.n.01.o01.csv")
e13v_bet_n_01_o02 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/o/E13V.Bet.n.01.o02.csv")
e13v_bet_n_01_o03 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/o/E13V.Bet.n.01.o03.csv")
e13v_bet_n_01_o04 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/o/E13V.Bet.n.01.o04.csv")
e13v_bet_n_01_o05 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/o/E13V.Bet.n.01.o05.csv")
e13v_bet_n_01_o06 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/o/e13v.Bet.n.01.o06.csv")
e13v_bet_n_01_o07 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/o/e13v.Bet.n.01.o07.csv")
e13v_bet_n_01_o08 <- csv2rwl("data/ring_data/wedging_rings/e13v/e13v.bet.n/e13v_bet_n_01/o/e13v.Bet.n.01.o08.csv")

#remove the core year
e13v_bet_n_01_o01 <- e13v_bet_n_01_o01[-1, ]
e13v_bet_n_01_o02 <- e13v_bet_n_01_o02[-1, ]
e13v_bet_n_01_o03 <- e13v_bet_n_01_o03[-1, ]
e13v_bet_n_01_o04 <- e13v_bet_n_01_o04[-1, ]
e13v_bet_n_01_o05 <- e13v_bet_n_01_o05[-1, ]
e13v_bet_n_01_o06 <- e13v_bet_n_01_o06[-1, ]
e13v_bet_n_01_o07 <- e13v_bet_n_01_o07[-1, ]
e13v_bet_n_01_o08 <- e13v_bet_n_01_o08[-1, ]

#merge in a single data frame:####
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13v_bet_n_01_o01$row_names <- rownames(e13v_bet_n_01_o01)
e13v_bet_n_01_o02$row_names <- rownames(e13v_bet_n_01_o02)
e13v_bet_n_01_o03$row_names <- rownames(e13v_bet_n_01_o03)
e13v_bet_n_01_o04$row_names <- rownames(e13v_bet_n_01_o04)
e13v_bet_n_01_o05$row_names <- rownames(e13v_bet_n_01_o05)
e13v_bet_n_01_o06$row_names <- rownames(e13v_bet_n_01_o06)
e13v_bet_n_01_o07$row_names <- rownames(e13v_bet_n_01_o07)
e13v_bet_n_01_o08$row_names <- rownames(e13v_bet_n_01_o08)

# Merge the data frames using Reduce and merge
e13v_bet_n_01_o <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13v_bet_n_01_o01, 
                               e13v_bet_n_01_o02,
                               e13v_bet_n_01_o03,
                               e13v_bet_n_01_o04,
                               e13v_bet_n_01_o05,
                               e13v_bet_n_01_o06,
                               e13v_bet_n_01_o07,
                               e13v_bet_n_01_o08)
                          )

# Set row names and remove the extra column
rownames(e13v_bet_n_01_o) <- e13v_bet_n_01_o[[common_column]]
e13v_bet_n_01_o[[common_column]] <- NULL

View(e13v_bet_n_01_o)

#export rwl

write.rwl(e13v_bet_n_01_o, "data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01/e13v_bet_n_01_o.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Data Analysis####
##Statistics####
e13v_bet_n_01_o_stats <- rwl.stats(e13v_bet_n_01_o) #summary and stats
print(e13v_bet_n_01_o_stats)

e13v_bet_n_01_o_ms <- sens2(e13v_bet_n_01_o) #calculates the mean sensitivity
print(e13v_bet_n_01_o_ms)

e13v_bet_n_01_o_report <- rwl.report(e13v_bet_n_01_o)  #report on rwl
print(e13v_bet_n_01_o_report)

##Cross-dating and alignment####

#shorten the name
e13v_bet_n_01_o_short <- e13v_bet_n_01_o
colnames(e13v_bet_n_01_o)
new_colnames <- sub("^E13VBetn01", "", colnames(e13v_bet_n_01_o_short))

# Assign the new column names to the data frame
colnames(e13v_bet_n_01_o_short) <- new_colnames

head(e13v_bet_n_01_o_short)
colnames(e13v_bet_n_01_o_short)

#graphs
seg.plot(e13v_bet_n_01_o_short) #creates a segment plot
title(main = "e13vBetn01o", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
spag.plot(e13v_bet_n_01_o_short, zfac=0.009,) #creates a spaghetti plot
title(main = "e13vBetn01o", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13v_bet_n_01_o_inter <- interseries.cor(e13v_bet_n_01_o_short, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13v_bet_n_01_o_inter)

###General correlation####

corr.rwl.seg(rwl = e13v_bet_n_01_o_short, seg.length = 2, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, 
             biweight = FALSE, method = c("spearman"), 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

title(main = "e13vBetn01o", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title
