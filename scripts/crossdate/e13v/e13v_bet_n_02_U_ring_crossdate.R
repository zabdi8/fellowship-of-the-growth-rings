#Load packages####
library(dplR)
library(treeclim)
library(ggplot2)

#Load the data####
e13v_bet_n_02_u01 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_02/u/E13V.Bet.n.02.u01.csv")
e13v_bet_n_02_u04 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_02/u/E13V.Bet.n.02.u04.csv")
e13v_bet_n_02_u08 <- csv2rwl("data/ring_data/raw/e13v/e13v.bet.n/e13v_bet_n_02/u/E13V.Bet.n.02.u08.csv")


#remove the core year
e13v_bet_n_02_u01 <- e13v_bet_n_02_u01[-1, ]
e13v_bet_n_02_u04 <- e13v_bet_n_02_u04[-1, ]
e13v_bet_n_02_u08 <- e13v_bet_n_02_u08[-1, ]



#merge in a single data frame:####
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13v_bet_n_02_u01$row_names <- rownames(e13v_bet_n_02_u01)
e13v_bet_n_02_u04$row_names <- rownames(e13v_bet_n_02_u04)
e13v_bet_n_02_u08$row_names <- rownames(e13v_bet_n_02_u08)



# Merge the data frames using Reduce and merge
e13v_bet_n_02_u <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13v_bet_n_02_u01, 
                               e13v_bet_n_02_u04,
                               e13v_bet_n_02_u08)
)

# Set row names and remove the extra column
rownames(e13v_bet_n_02_u) <- e13v_bet_n_02_u[[common_column]]
e13v_bet_n_02_u[[common_column]] <- NULL

View(e13v_bet_n_02_u)

#export rwl

write.rwl(e13v_bet_n_02_u, "data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02/e13v_bet_n_02_u.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#Data Analysis####
##Statistics####
e13v_bet_n_02_u_stats <- rwl.stats(e13v_bet_n_02_u) #summary and stats
print(e13v_bet_n_02_u_stats)

e13v_bet_n_02_u_ms <- sens2(e13v_bet_n_02_u) #calculates the mean sensitivity
print(e13v_bet_n_02_u_ms)

e13v_bet_n_02_u_report <- rwl.report(e13v_bet_n_02_u)  #report on rwl in this case really few cases
print(e13v_bet_n_02_u_report)

##Cross-dating and alignment####

#shorten the name
e13v_bet_n_02_u_short <- e13v_bet_n_02_u
colnames(e13v_bet_n_02_u)
new_colnames <- sub("^E13VBetn02", "", colnames(e13v_bet_n_02_u_short))

# Assign the new column names to the data frame
colnames(e13v_bet_n_02_u_short) <- new_colnames

head(e13v_bet_n_02_u_short)
colnames(e13v_bet_n_02_u_short)

#graphs
seg.plot(e13v_bet_n_02_u_short) #creates a segment plot
title(main = "e13vBetn02t", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
spag.plot(e13v_bet_n_02_u_short, zfac=0.006,) #creates a spaghetti plot
title(main = "e13vBetn02t", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

##Analysis####

e13v_bet_n_02_u_inter <- interseries.cor(e13v_bet_n_02_u_short, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master not enough observations
print(e13v_bet_n_02_u_inter)

###General correlation####
#can't be created because there are very few observations
# corr.rwl.seg(rwl = e13v_bet_n_02_u_short, seg.length = 4, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, 
#              biweight = FALSE, method = c("spearman"), 
#              make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)
# 
# title(main = "e13vBetn02t", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

