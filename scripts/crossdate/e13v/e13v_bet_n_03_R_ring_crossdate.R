#Load packages####
library(dplR)

#Load the data####
e13v_bet_n_03_r01 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/r/E13V.Bet.n.03.r01.csv")
e13v_bet_n_03_r02 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/r/E13V.Bet.n.03.r02.csv")
e13v_bet_n_03_r03 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/r/E13V.Bet.n.03.r03.csv")
e13v_bet_n_03_r04 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_03/r/E13V.Bet.n.03.r04.csv")


#remove the core year
e13v_bet_n_03_r01 <- e13v_bet_n_03_r01[-1, ]
e13v_bet_n_03_r02 <- e13v_bet_n_03_r02[-1, ]
e13v_bet_n_03_r03 <- e13v_bet_n_03_r03[-1, ]
e13v_bet_n_03_r04 <- e13v_bet_n_03_r04[-1, ]



#merge in a single data frame:####
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13v_bet_n_03_r01$row_names <- rownames(e13v_bet_n_03_r01)
e13v_bet_n_03_r02$row_names <- rownames(e13v_bet_n_03_r02)
e13v_bet_n_03_r03$row_names <- rownames(e13v_bet_n_03_r03)
e13v_bet_n_03_r04$row_names <- rownames(e13v_bet_n_03_r04)



# Merge the data frames using Reduce and merge
e13v_bet_n_03_r <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13v_bet_n_03_r01,
                               e13v_bet_n_03_r02, 
                               e13v_bet_n_03_r03,
                               e13v_bet_n_03_r04)
                          )

# Set row names and remove the extra column
rownames(e13v_bet_n_03_r) <- e13v_bet_n_03_r[[common_column]]
e13v_bet_n_03_r[[common_column]] <- NULL

View(e13v_bet_n_03_r)

#export rwl

write.rwl(e13v_bet_n_03_r, "data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03/e13v_bet_n_03_r.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#Data Analysis####
##Statistics####
e13v_bet_n_03_r_stats <- rwl.stats(e13v_bet_n_03_r) #summary and stats
print(e13v_bet_n_03_r_stats)

e13v_bet_n_03_r_ms <- sens2(e13v_bet_n_03_r) #calculates the mean sensitivity
print(e13v_bet_n_03_r_ms)

e13v_bet_n_03_r_report <- rwl.report(e13v_bet_n_03_r)  #report on rwl in this case really few cases
print(e13v_bet_n_03_r_report)

##Cross-dating and alignment####

#shorten the name
e13v_bet_n_03_r_short <- e13v_bet_n_03_r
colnames(e13v_bet_n_03_r)
new_colnames <- sub("^E13VBetn03", "", colnames(e13v_bet_n_03_r_short))

# Assign the new column names to the data frame
colnames(e13v_bet_n_03_r_short) <- new_colnames

head(e13v_bet_n_03_r_short)
colnames(e13v_bet_n_03_r_short)

#graphs
seg.plot(e13v_bet_n_03_r_short) #creates a segment plot
title(main = "e13vBetn03r", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
spag.plot(e13v_bet_n_03_r_short, zfac=0.006,) #creates a spaghetti plot
title(main = "e13vBetn03r", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13v_bet_n_03_r_inter <- interseries.cor(e13v_bet_n_03_r_short, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master not enough observations
print(e13v_bet_n_03_r_inter)

###General correlation####

corr.rwl.seg(rwl = e13v_bet_n_03_r_short, seg.length = 8, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05,
             biweight = FALSE, method = c("spearman"),
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

title(main = "e13vBetn03r", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title
