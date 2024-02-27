#Load packages####
library(dplR)

#Load the data####
e13d_bet_n_01_t01 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_01/t/E13D.Bet.n.01.t01.csv")
e13d_bet_n_01_t02 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_01/t/E13D.Bet.n.01.t02.csv")
e13d_bet_n_01_t03 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_01/t/E13D.Bet.n.01.t03.csv")
e13d_bet_n_01_t04 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_01/t/E13D.Bet.n.01.t04.csv")
e13d_bet_n_01_t06 <- csv2rwl("data/ring_data/raw/e13d/e13d.bet.n/e13d_bet_n_01/t/E13D.Bet.n.01.t06.csv")


#remove the core year
e13d_bet_n_01_t01 <- e13d_bet_n_01_t01[-1, ]
e13d_bet_n_01_t02 <- e13d_bet_n_01_t02[-1, ]
e13d_bet_n_01_t03 <- e13d_bet_n_01_t03[-1, ]
e13d_bet_n_01_t04 <- e13d_bet_n_01_t04[-1, ]
e13d_bet_n_01_t06 <- e13d_bet_n_01_t06[-1, ]

#merge in a single data frame:####
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13d_bet_n_01_t01$row_names <- rownames(e13d_bet_n_01_t01)
e13d_bet_n_01_t02$row_names <- rownames(e13d_bet_n_01_t02)
e13d_bet_n_01_t03$row_names <- rownames(e13d_bet_n_01_t03)
e13d_bet_n_01_t04$row_names <- rownames(e13d_bet_n_01_t04)
e13d_bet_n_01_t06$row_names <- rownames(e13d_bet_n_01_t06)

# Merge the data frames using Reduce and merge
e13d_bet_n_01_t <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13d_bet_n_01_t01,
                               e13d_bet_n_01_t02,
                               e13d_bet_n_01_t03, 
                               e13d_bet_n_01_t04,
                               e13d_bet_n_01_t06)
)

# Set row names and remove the extra column
rownames(e13d_bet_n_01_t) <- e13d_bet_n_01_t[[common_column]]
e13d_bet_n_01_t[[common_column]] <- NULL

View(e13d_bet_n_01_t)

#export rwl

write.rwl(e13d_bet_n_01_t, "data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01/e13d_bet_n_01_t.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Data Analysis####
##Statistics####
e13d_bet_n_01_t_stats <- rwl.stats(e13d_bet_n_01_t) #summary and stats
print(e13d_bet_n_01_t_stats)

e13d_bet_n_01_t_ms <- sens2(e13d_bet_n_01_t) #calculates the mean sensitivity
print(e13d_bet_n_01_t_ms)

e13d_bet_n_01_t_report <- rwl.report(e13d_bet_n_01_t)  #report on rwl
print(e13d_bet_n_01_t_report)

##Cross-dating and alignment####

#shorten the name
e13d_bet_n_01_t_short <- e13d_bet_n_01_t
colnames(e13d_bet_n_01_t)
new_colnames <- sub("^E13DBetn01", "", colnames(e13d_bet_n_01_t_short))

# Assign the new column names to the data frame
colnames(e13d_bet_n_01_t_short) <- new_colnames

head(e13d_bet_n_01_t_short)
colnames(e13d_bet_n_01_t_short)

#graphs
seg.plot(e13d_bet_n_01_t_short) #creates a segment plot
title(main = "e13dbetn01t", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.2) #add title
spag.plot(e13d_bet_n_01_t_short, zfac=0.009,) #creates a spaghetti plot
title(main = "e13dbetn01t", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13d_bet_n_01_t_inter <- interseries.cor(e13d_bet_n_01_t, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13d_bet_n_01_t_inter)

###General correlation####

corr.rwl.seg(rwl = e13d_bet_n_01_t_short, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, 
             biweight = FALSE, method = c("spearman"), 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

title(main = "e13dbetn01t", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

