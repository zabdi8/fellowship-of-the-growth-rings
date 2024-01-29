#Load packages####
library(dplR)
library(signal)
library(treeclim)
library(ggplot2)

#Load the data####
E13CBetn01r03
E13CBetn01r03 <- E13CBetn01r03[-1,] #Remove the core year for all samples (2005 for this)

#Data Analysis####
##Statistics####
E13CBetn01r03_stats <- rwl.stats(E13CBetn01r03) #summary and stats
print(E13CBetn01r03_stats)

E13CBetn01r03_ms <- sens2(E13CBetn01r03) #calculates the mean sensitivity
print(E13CBetn01r03_ms)

E13CBetn01r03_report <- rwl.report(E13CBetn01r03)  #report on rwl
print(E13CBetn01r03_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(E13CBetn01r03) #creates a segment plot
spag.plot(E13CBetn01r03, zfac=0.01,) #creates a spaghetti plot

##Analysis####
E13CBetn01r03_inter <- interseries.cor(E13CBetn01r03, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "pearson") #correlation between series and master
print(E13CBetn01r03_inter)

###General correlation####
corr.rwl.seg(rwl = E13CBetn01r03, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "E13CBetn01r03", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

###Series vs Master####
####E13CBetn01r03####
 
#E13CBetn01r03r1
E13CBetn01r03r1 <- series.rwl.plot(E13CBetn01r03, series = "E13CBetn01r03r1", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE)
  title(sub= "E13CBetn01r03r1", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title
view(E13CBetn01r03)
#E13CBetn01r03r2
E13CBetn01r03r2 <- series.rwl.plot(E13CBetn01r03, series = "E13CBetn01r03r2", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r03r2", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r03r3
E13CBetn01r03r3 <- series.rwl.plot(E13CBetn01r03, series = "E13CBetn01r03r3", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r03r3", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r03r4
E13CBetn01r03r4 <- series.rwl.plot(E13CBetn01r03, series = "E13CBetn01r03r4", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r03r4", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#Alignment
E13CBetn01r03_aligned <- E13CBetn01r03

#corrections on E13CBetn01r01r1 based on image
E13CBetn01r03_aligned["2008", "E13CBetn01r03r1"] <- 116.473
E13CBetn01r03_aligned["2009", "E13CBetn01r03r1"] <- 211.298
E13CBetn01r03_aligned["2010", "E13CBetn01r03r1"] <- 157.250
E13CBetn01r03_aligned["2011", "E13CBetn01r03r1"] <- 109.711
E13CBetn01r03_aligned["2012", "E13CBetn01r03r1"] <- 86.206
E13CBetn01r03_aligned["2013", "E13CBetn01r03r1"] <- 67.793
E13CBetn01r03_aligned["2014", "E13CBetn01r03r1"] <- 83.438
E13CBetn01r03_aligned["2016", "E13CBetn01r03r1"] <- 79.550
E13CBetn01r03_aligned["2017", "E13CBetn01r03r1"] <- 43.053
E13CBetn01r03_aligned["2018", "E13CBetn01r03r1"] <- 48.128
E13CBetn01r03_aligned["2020", "E13CBetn01r03r1"] <- 53.705

#re run with corrected values
E13CBetn01r03r1_aligned <- series.rwl.plot(E13CBetn01r03_aligned, series = "E13CBetn01r03r1", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r03r1 (aligned)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#corrections on E13CBetn01r02r2 based on image

E13CBetn01r03_aligned["2008", "E13CBetn01r03r2"] <- 111.605
E13CBetn01r03_aligned["2009", "E13CBetn01r03r2"] <- 183.167
E13CBetn01r03_aligned["2010", "E13CBetn01r03r2"] <- 148.165
E13CBetn01r03_aligned["2011", "E13CBetn01r03r2"] <- 127.434
E13CBetn01r03_aligned["2012", "E13CBetn01r03r2"] <- 103.626
E13CBetn01r03_aligned["2013", "E13CBetn01r03r2"] <- 88.827
E13CBetn01r03_aligned["2014", "E13CBetn01r03r2"] <- 75.545
E13CBetn01r03_aligned["2015", "E13CBetn01r03r2"] <- 92.470
E13CBetn01r03_aligned["2016", "E13CBetn01r03r2"] <- 78.261
E13CBetn01r03_aligned["2017", "E13CBetn01r03r2"] <- 29.368
E13CBetn01r03_aligned["2018", "E13CBetn01r03r2"] <- 56.616
E13CBetn01r03_aligned["2019", "E13CBetn01r03r2"] <- 45.768




#rerun with corrected values
E13CBetn01r03r2_aligned <- series.rwl.plot(E13CBetn01r03_aligned, series = "E13CBetn01r03r2", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r03r2 (aligned)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#corrections on E13CBetn01r02r3 based on image

E13CBetn01r03_aligned["2009", "E13CBetn01r03r3"] <- 211.119
E13CBetn01r03_aligned["2010", "E13CBetn01r03r3"] <- 164.662
E13CBetn01r03_aligned["2011", "E13CBetn01r03r3"] <- 144.438
E13CBetn01r03_aligned["2012", "E13CBetn01r03r3"] <- 132.542
E13CBetn01r03_aligned["2013", "E13CBetn01r03r3"] <- 99.366
E13CBetn01r03_aligned["2014", "E13CBetn01r03r3"] <- 100.307
E13CBetn01r03_aligned["2015", "E13CBetn01r03r3"] <- 100.853
E13CBetn01r03_aligned["2016", "E13CBetn01r03r3"] <- 68.360
E13CBetn01r03_aligned["2019", "E13CBetn01r03r3"] <- 24.703
E13CBetn01r03_aligned["2020", "E13CBetn01r03r3"] <- 111.322


#rerun with corrected values
E13CBetn01r03r3_aligned <- series.rwl.plot(E13CBetn01r03_aligned, series = "E13CBetn01r03r3", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r03r3 (aligned)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title
view(E13CBetn01r03_aligned)

#corrections on E13CBetn01r02r4 based on image

E13CBetn01r03_aligned["2008", "E13CBetn01r03r4"] <- 124.727
E13CBetn01r03_aligned["2009", "E13CBetn01r03r4"] <- 202.764
E13CBetn01r03_aligned["2010", "E13CBetn01r03r4"] <- 163.316
E13CBetn01r03_aligned["2011", "E13CBetn01r03r4"] <- 102.815
E13CBetn01r03_aligned["2012", "E13CBetn01r03r4"] <- 84.674
E13CBetn01r03_aligned["2013", "E13CBetn01r03r4"] <- 81.857
E13CBetn01r03_aligned["2014", "E13CBetn01r03r4"] <- 94.572
E13CBetn01r03_aligned["2015", "E13CBetn01r03r4"] <- 98.197
E13CBetn01r03_aligned["2016", "E13CBetn01r03r4"] <- 67.569
E13CBetn01r03_aligned["2017", "E13CBetn01r03r4"] <- 25.187
E13CBetn01r03_aligned["2018", "E13CBetn01r03r4"] <- 27.251
E13CBetn01r03_aligned["2019", "E13CBetn01r03r4"] <- 18.623
E13CBetn01r03_aligned["2020", "E13CBetn01r03r4"] <- 56.741

#rerun with aligned values
E13CBetn01r03r4_aligned <- series.rwl.plot(E13CBetn01r03_aligned, series = "E13CBetn01r03r4", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r03r4 (aligned)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title
view(E13CBetn01r03_aligned)

#statistics with aligned df####

E13CBetn01r03_aligned_stats <- rwl.stats(E13CBetn01r03_aligned) #summary and stats
print(E13CBetn01r03_aligned_stats)

E13CBetn01r03_aligned_ms <- sens2(E13CBetn01r03_aligned) #calculates the mean sensitivity
print(E13CBetn01r03_aligned_ms)

E13CBetn01r03_aligned_report_aligned <- rwl.report(E13CBetn01r03_aligned)  #report on rwl
print(E13CBetn01r03_aligned_report_aligned)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(E13CBetn01r03_aligned) #creates a segment plot
spag.plot(E13CBetn01r03_aligned, zfac=0.01,) #creates a spaghetti plot

##Analysis with aligned df####

E13CBetn01r03_inter_aligned <- interseries.cor(E13CBetn01r03_aligned, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "pearson") #correlation between series and master
print(E13CBetn01r03_inter_aligned)

###General correlation####
corr.rwl.seg(rwl = E13CBetn01r03_aligned, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "E13CBetn01r02_aligned", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

#export data
#I made an update using the picture and found a missing ring in 2011

E13CBetn01r03_missing <- read.csv("~/Documents/GitHub/fellowship-of-the-growth-rings/data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/r/E13CBetn01r03_aligned.csv", row.names=1)


E13CBetn01r03_aligned <- E13CBetn01r03_missing

write.rwl(E13CBetn01r03_aligned, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/r/E13CBetn01r03_aligned.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )
view(E13CBetn01r03_aligned)
#Update the e13c_bet_n_01_R version with the aligned data.

colnames(E13CBetn01r03_aligned)

#substitute with the aligned data
years_update <- 2006:2020 #year gap for update

columns_update <- c("E13CBetn01r03r1", "E13CBetn01r03r2", "E13CBetn01r03r3", "E13CBetn01r03r4") #columns to update

for (col in columns_update) {
  e13c_bet_n_01_R_aligned[as.character(years_update), col] <- E13CBetn01r03_aligned[as.character(years_update), col]
}

#confirm that the results are the same for both aligned data frames

#check they that were updated
if (identical(
  e13c_bet_n_01_R_aligned[years_update, columns_update], #confirm the years and columns for each year
  E13CBetn01r03_aligned[years_update, columns_update])) {
  cat(paste("The new data from", paste(columns_update, collapse = ", "), "has successfully been updated."))
} else {
  cat("The data is not the same for the specified years.\n")
}

view(e13c_bet_n_01_R_aligned)

#write the new aligned rwl file
write.rwl(e13c_bet_n_01_R_aligned, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/r/e13c_bet_n_01_R_aligned", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )
