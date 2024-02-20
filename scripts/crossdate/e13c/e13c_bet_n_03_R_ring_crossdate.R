#Load packages####
library(dplR)

#Load the data####
e13c_bet_n_03_r01 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_03/R/E13C.Bet.n.03.r01.csv")
e13c_bet_n_03_r02 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_03/R/E13C.Bet.n.03.r02.csv")
e13c_bet_n_03_r03 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_03/R/E13C.Bet.n.03.r03.csv")
e13c_bet_n_03_r04 <- csv2rwl("data/ring_data/raw/e13c/e13c.bet.n/e13c_bet_n_03/R/E13C.Bet.n.03.r04.csv")


#remove the core year

e13c_bet_n_03_r01 <- e13c_bet_n_03_r01[-1, ]
e13c_bet_n_03_r02 <- e13c_bet_n_03_r02[-1, ]
e13c_bet_n_03_r03 <- e13c_bet_n_03_r03[-1, ]
e13c_bet_n_03_r04 <- e13c_bet_n_03_r04[-1, ]




#merge in a single data frame:####

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_03_r01$row_names <- rownames(e13c_bet_n_03_r01)
e13c_bet_n_03_r02$row_names <- rownames(e13c_bet_n_03_r02)
e13c_bet_n_03_r03$row_names <- rownames(e13c_bet_n_03_r03)
e13c_bet_n_03_r04$row_names <- rownames(e13c_bet_n_03_r04)



# Merge the data frames using Reduce and merge
e13c_bet_n_03_r <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13c_bet_n_03_r01, 
                               e13c_bet_n_03_r02,
                               e13c_bet_n_03_r03, 
                               e13c_bet_n_03_r04)
)

# Set row names and remove the extra column
rownames(e13c_bet_n_03_r) <- e13c_bet_n_03_r[[common_column]]
e13c_bet_n_03_r[[common_column]] <- NULL

view(e13c_bet_n_03_r)

#export rwl

write.rwl(e13c_bet_n_03_r, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_03/r/e13c_bet_n_03_r", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#Data Analysis####
##Statistics####
e13c_bet_n_03_r_stats <- rwl.stats(e13c_bet_n_03_r) #summary and stats
print(e13c_bet_n_03_r_stats)

e13c_bet_n_03_r_ms <- sens2(e13c_bet_n_03_r) #calculates the mean sensitivity
print(e13c_bet_n_03_r_ms)

e13c_bet_n_03_r_report <- rwl.report(e13c_bet_n_03_r)  #report on rwl
print(e13c_bet_n_03_r_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_03_r) #creates a segment plot
spag.plot(e13c_bet_n_03_r, zfac=0.007,) #creates a spaghetti plot

##Analysis####

e13c_bet_n_03_r_inter <- interseries.cor(e13c_bet_n_03_r, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13c_bet_n_03_r_inter)

###General correlation####

corr.rwl.seg(rwl = e13c_bet_n_03_r, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "e13cbetn03r", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title




