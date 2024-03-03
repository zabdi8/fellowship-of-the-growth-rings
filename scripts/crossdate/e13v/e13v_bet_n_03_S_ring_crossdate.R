#Load packages####
library(dplR)

#Load the data####
e13v_bet_n_03_s01 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/s/E13V.Bet.n.03.s01.csv")
e13v_bet_n_03_s02 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/s/E13V.Bet.n.03.s02.csv")
e13v_bet_n_03_s03 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/s/E13V.Bet.n.03.s03.csv")
e13v_bet_n_03_s04 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/s/E13V.Bet.n.03.s04.csv")
e13v_bet_n_03_s05 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/s/E13V.Bet.n.03.s05.csv")
e13v_bet_n_03_s06 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/s/E13V.Bet.n.03.s06.csv")
e13v_bet_n_03_s07 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/s/E13V.Bet.n.03.s07.csv")
e13v_bet_n_03_s08 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/s/E13V.Bet.n.03.s08.csv")
e13v_bet_n_03_s09 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/s/E13V.Bet.n.03.s09.csv")
e13v_bet_n_03_s10 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/s/E13V.Bet.n.03.s10.csv")


#remove the core year
e13v_bet_n_03_s01 <- e13v_bet_n_03_s01[-1, ]
e13v_bet_n_03_s02 <- e13v_bet_n_03_s02[-1, ]
e13v_bet_n_03_s03 <- e13v_bet_n_03_s03[-1, ]
e13v_bet_n_03_s04 <- e13v_bet_n_03_s04[-1, ]
e13v_bet_n_03_s05 <- e13v_bet_n_03_s05[-1, ]
e13v_bet_n_03_s06 <- e13v_bet_n_03_s06[-1, ]
e13v_bet_n_03_s07 <- e13v_bet_n_03_s07[-1, ]
e13v_bet_n_03_s08 <- e13v_bet_n_03_s08[-1, ]
e13v_bet_n_03_s09 <- e13v_bet_n_03_s09[-1, ]
e13v_bet_n_03_s10 <- e13v_bet_n_03_s10[-1, ]



#merge in a single data frame:####
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13v_bet_n_03_s01$row_names <- rownames(e13v_bet_n_03_s01)
e13v_bet_n_03_s02$row_names <- rownames(e13v_bet_n_03_s02)
e13v_bet_n_03_s03$row_names <- rownames(e13v_bet_n_03_s03)
e13v_bet_n_03_s04$row_names <- rownames(e13v_bet_n_03_s04)
e13v_bet_n_03_s05$row_names <- rownames(e13v_bet_n_03_s05)
e13v_bet_n_03_s06$row_names <- rownames(e13v_bet_n_03_s06)
e13v_bet_n_03_s07$row_names <- rownames(e13v_bet_n_03_s07)
e13v_bet_n_03_s08$row_names <- rownames(e13v_bet_n_03_s08)
e13v_bet_n_03_s09$row_names <- rownames(e13v_bet_n_03_s09)
e13v_bet_n_03_s10$row_names <- rownames(e13v_bet_n_03_s10)



# Merge the data frames using Reduce and merge
e13v_bet_n_03_s <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13v_bet_n_03_s01,
                               e13v_bet_n_03_s02,
                               e13v_bet_n_03_s03,
                               e13v_bet_n_03_s04, 
                               e13v_bet_n_03_s05,
                               e13v_bet_n_03_s06,
                               e13v_bet_n_03_s07,
                               e13v_bet_n_03_s08,
                               e13v_bet_n_03_s09,
                               e13v_bet_n_03_s10)
                          )

# Set row names and remove the extra column
rownames(e13v_bet_n_03_s) <- e13v_bet_n_03_s[[common_column]]
e13v_bet_n_03_s[[common_column]] <- NULL

View(e13v_bet_n_03_s)

#export rwl

write.rwl(e13v_bet_n_03_s, "data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03/e13v_bet_n_03_s.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Data Analysis####
##Statistics####
e13v_bet_n_03_s_stats <- rwl.stats(e13v_bet_n_03_s) #summary and stats
print(e13v_bet_n_03_s_stats)

e13v_bet_n_03_s_ms <- sens2(e13v_bet_n_03_s) #calculates the mean sensitivity
print(e13v_bet_n_03_s_ms)

e13v_bet_n_03_s_report <- rwl.report(e13v_bet_n_03_s)  #report on rwl in this case really few cases
print(e13v_bet_n_03_s_report)

##Cross-dating and alignment####

#shorten the name
e13v_bet_n_03_s_short <- e13v_bet_n_03_s
colnames(e13v_bet_n_03_s)
new_colnames <- sub("^E13VBetn03", "", colnames(e13v_bet_n_03_s_short))

# Assign the new column names to the data frame
colnames(e13v_bet_n_03_s_short) <- new_colnames

head(e13v_bet_n_03_s_short)
colnames(e13v_bet_n_03_s_short)

#graphs
seg.plot(e13v_bet_n_03_s_short) #creates a segment plot
title(main = "e13vBetn03s", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
spag.plot(e13v_bet_n_03_s_short, zfac=0.02,) #creates a spaghetti plot
title(main = "e13vBetn03s", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13v_bet_n_03_s_inter <- interseries.cor(e13v_bet_n_03_s_short, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master not enough observations
print(e13v_bet_n_03_s_inter)

###General correlation####

corr.rwl.seg(rwl = e13v_bet_n_03_s_short, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05,
             biweight = FALSE, method = c("spearman"),
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

title(main = "e13vBetn03s", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

