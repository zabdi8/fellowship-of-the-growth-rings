#Load packages####
library(dplR)

#Load the data####
view(e13c_bet_n_01_R_aligned)

#Data Analysis####
##Statistics####
e13c_bet_n_01_R_aligned_stats <- rwl.stats(e13c_bet_n_01_R_aligned) #summary and stats
print(e13c_bet_n_01_R_aligned_stats)

e13c_bet_n_01_R_aligned_ms <- sens2(e13c_bet_n_01_R_aligned) #calculates the mean sensitivity
print(e13c_bet_n_01_R_aligned_ms)

e13c_bet_n_01_R_aligned_report <- rwl.report(e13c_bet_n_01_R_aligned)  #report on rwl
print(e13c_bet_n_01_R_aligned_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_01_R_aligned) #creates a segment plot
spag.plot(e13c_bet_n_01_R_aligned, zfac=0.01,) #creates a spaghetti plot
e13c_bet_n_01_R_figure <- spag.plot(e13c_bet_n_01_R_aligned, zfac=0.01,)

ggsave("figures/spaghetti_plots/e13c_bet_n_01_R.jpeg", plot = e13c_bet_n_01_R_figure, device = "jpeg", width = 10, height = 6, units = "in")

##Analysis####

e13c_bet_n_01_R_aligned_inter <- interseries.cor(e13c_bet_n_01_R_aligned, n = NULL, prewhiten = TRUE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13c_bet_n_01_R_aligned_inter)

###General correlation####

corr.rwl.seg(rwl = e13c_bet_n_01_R_aligned, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "e13c_bet_n_01_R_aligned", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

#create a Df with the average of each section for e13c_bet_n_01_R_aligned

# Create a new DataFrame with the same row names as the original DataFrame
e13c_bet_n_01_R_average <- data.frame(RowNames = rownames(e13c_bet_n_01_R_aligned))

# Calculate the average for each set of columns and add them to the new DataFrame
e13c_bet_n_01_R_average$E13CBetn01r01 <- rowMeans(e13c_bet_n_01_R_aligned[, c("E13CBetn01r01r1",
                                                                              "E13CBetn01r01r2", 
                                                                              "E13CBetn01r01r3",
                                                                              "E13CBetn01r01r4")], 
                                                  na.rm = TRUE)
e13c_bet_n_01_R_average$E13CBetn01r02 <- rowMeans(e13c_bet_n_01_R_aligned[, c("E13CBetn01r02r1",
                                                                              "E13CBetn01r02r2",
                                                                              "E13CBetn01r02r3",
                                                                              "E13CBetn01r02r4")],
                                                  na.rm = TRUE)
e13c_bet_n_01_R_average$E13CBetn01r03 <- rowMeans(e13c_bet_n_01_R_aligned[, c("E13CBetn01r03r1",
                                                                              "E13CBetn01r03r2",
                                                                              "E13CBetn01r03r3",
                                                                              "E13CBetn01r03r4")], 
                                                  na.rm = TRUE)
e13c_bet_n_01_R_average$E13CBetn01r04 <- rowMeans(e13c_bet_n_01_R_aligned[, c("E13CBetn01r04r1",
                                                                              "E13CBetn01r04r2",
                                                                              "E13CBetn01r04r3",
                                                                              "E13CBetn01r04r4")],
                                                  na.rm = TRUE)
e13c_bet_n_01_R_average$E13CBetn01r05 <- rowMeans(e13c_bet_n_01_R_aligned[, c("E13CBetn01r05r1",
                                                                              "E13CBetn01r05r2",
                                                                              "E13CBetn01r05r3",
                                                                              "E13CBetn01r05r4")], 
                                                  na.rm = TRUE)

rownames(e13c_bet_n_01_R_average) <- e13c_bet_n_01_R_average$RowNames
e13c_bet_n_01_R_average <- e13c_bet_n_01_R_average[, -1]
view(e13c_bet_n_01_R_average)

# fix the Nas and eliminate the data from the "cores"
e13c_bet_n_01_R_average <- e13c_bet_n_01_R_average[-1,] #remove the year 2001 which has core measurments.

#E13CBetn01r01
e13c_bet_n_01_R_average["2002", "E13CBetn01r01"] <- NA
e13c_bet_n_01_R_average["2003", "E13CBetn01r01"] <- NA
e13c_bet_n_01_R_average["2004", "E13CBetn01r01"] <- NA
e13c_bet_n_01_R_average["2005", "E13CBetn01r01"] <- NA

#E13CBetn01r02
e13c_bet_n_01_R_average["2002", "E13CBetn01r02"] <- NA
e13c_bet_n_01_R_average["2003", "E13CBetn01r02"] <- NA
e13c_bet_n_01_R_average["2004", "E13CBetn01r02"] <- NA
e13c_bet_n_01_R_average["2005", "E13CBetn01r02"] <- NA
e13c_bet_n_01_R_average["2006", "E13CBetn01r02"] <- NA

#E13CBetn01r3
e13c_bet_n_01_R_average["2002", "E13CBetn01r03"] <- NA
e13c_bet_n_01_R_average["2003", "E13CBetn01r03"] <- NA
e13c_bet_n_01_R_average["2004", "E13CBetn01r03"] <- NA
e13c_bet_n_01_R_average["2005", "E13CBetn01r03"] <- NA
e13c_bet_n_01_R_average["2006", "E13CBetn01r03"] <- NA


view(e13c_bet_n_01_R_average)
write.csv(e13c_bet_n_01_R_average, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/r/e13c_bet_n_01_R_average.csv")
#stats for the aligned average
rwl <- e13c_bet_n_01_R_average

e13c_bet_n_01_R_average_stats <- rwl.stats(rwl) #summary and stats
print(e13c_bet_n_01_R_average_stats)

e13c_bet_n_01_R_average_ms <- sens2(rwl) #calculates the mean sensitivity
print(e13c_bet_n_01_R_average_ms)

e13c_bet_n_01_R_average_report <- rwl.report(rwl)  #report on rwl
print(e13c_bet_n_01_R_average_report)

##Cross-dating and alignment####

#Check the alignment of the series

#graphs
seg.plot(rwl) #creates a segment plot
spag.plot(rwl, zfac=0.005,) #creates a spaghetti plot



# Assuming your data frame is named rwl
library(lattice)

xyplot(E13CBetn01r01 + E13CBetn01r02 + E13CBetn01r03 + E13CBetn01r04 + E13CBetn01r05 ~ as.numeric(rownames(rwl)),
       data = rwl, type = 'l',
       xlab = 'Year', ylab = 'Values',
       auto.key = list(columns = 5))
  print(rwl)
##Analysis####

e13c_bet_n_01_R_aligned_inter <- interseries.cor(e13c_bet_n_01_R_aligned, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "spearman")        #correlation between series and master
print(e13c_bet_n_01_R_aligned_inter)

###General correlation####

corr.rwl.seg(rwl = e13c_bet_n_01_R_aligned, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "e13c_bet_n_01_R_aligned", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title


###Series vs Master####
####E13CBetn01r####
#E13CBetn01r01

serie <- "E13CBetn01r01"
E13CBetn01r01r1_complete <- series.rwl.plot(rwl, series = serie, 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE)
  title(sub= "E13CBetn01r01 (complete)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r01r2
  serie <- "E13CBetn01r02"
E13CBetn01r01r2_complete <- series.rwl.plot(rwl, series = serie, 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
  title(sub= "E13CBetn01r01r2", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r01r3
E13CBetn01r01r3 <- series.rwl.plot(e13c_bet_n_01_R01.rwl, series = "E13CBetn01r01r3", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = TRUE, biweight = TRUE, 
                floor.plus1 = FALSE
                )
  title(sub= "E13CBetn01r01r3", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r01r4
E13CBetn01r01r4 <- series.rwl.plot(e13c_bet_n_01_R01.rwl, series = "E13CBetn01r01r4", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r01r4", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

