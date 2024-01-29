#Load packages####
library(dplR)
library(signal)
library(treeclim)
library(ggplot2)

#Load the data####
E13CBetn01r01 <- read.rwl("data/ring_data/e13c/e13c.bet.n/e13c_bet_n_01/r/e13c_bet_n_01_R01.rwl")
E13CBetn01r01 <- E13CBetn01r01[-1,] #Remove the core year for all samples (2005 for this)

#Data Analysis####
##Statistics####
E13CBetn01r01_stats <- rwl.stats(E13CBetn01r01) #summary and stats
print(rwl_stats)

ms <- sens2(E13CBetn01r01) #calculates the mean sensitivity
print(ms)

rwl_report <- rwl.report(E13CBetn01r01)  #report on rwl
print(rwl_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(E13CBetn01r01) #creates a segment plot
spag.plot(E13CBetn01r01, zfac=0.01,) #creates a spaghetti plot

##Analysis####
inter <- interseries.cor(E13CBetn01r01, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "pearson") #correlation between series and master
print(inter)

###General correlation####
corr.rwl.seg(rwl = E13CBetn01r01, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "E13CBetn01r01", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

###Series vs Master####
####E13CBetn01r01r1####
#E13CBetn01r01r1
E13CBetn01r01r1 <- series.rwl.plot(E13CBetn01r01, series = "E13CBetn01r01r1", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE)
  title(sub= "E13CBetn01r01r1", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r01r2
E13CBetn01r01r2 <- series.rwl.plot(E13CBetn01r01, series = "E13CBetn01r01r2", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r01r2", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r01r3
E13CBetn01r01r3 <- series.rwl.plot(E13CBetn01r01, series = "E13CBetn01r01r3", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r01r3", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r01r4
E13CBetn01r01r4 <- series.rwl.plot(E13CBetn01r01, series = "E13CBetn01r01r4", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r01r4", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#Alignment
E13CBetn01r01_aligned <- E13CBetn01r01

#corrections on E13CBetn01r01r1 based on image
E13CBetn01r01_aligned["2006", "E13CBetn01r01r1"] <- 105.380
E13CBetn01r01_aligned["2007", "E13CBetn01r01r1"] <- 51.536
E13CBetn01r01_aligned["2008", "E13CBetn01r01r1"] <- 113.918
E13CBetn01r01_aligned["2009", "E13CBetn01r01r1"] <- 154.822
E13CBetn01r01_aligned["2010", "E13CBetn01r01r1"] <- 265.654
E13CBetn01r01_aligned["2011", "E13CBetn01r01r1"] <- 233.582
E13CBetn01r01_aligned["2012", "E13CBetn01r01r1"] <- 215.102

#re run with corrected values
E13CBetn01r01r1_aligned <- series.rwl.plot(E13CBetn01r01, series = "E13CBetn01r01r1", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE)
title(sub= "E13CBetn01r01r1", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#corrections on E13CBetn01r01r2 based on image

E13CBetn01r01_aligned["2006", "E13CBetn01r01r2"] <- 115.882
E13CBetn01r01_aligned["2007", "E13CBetn01r01r2"] <- 106.255
E13CBetn01r01_aligned["2008", "E13CBetn01r01r2"] <- 122.532
E13CBetn01r01_aligned["2009", "E13CBetn01r01r2"] <- 104.030
E13CBetn01r01_aligned["2010", "E13CBetn01r01r2"] <- 145.582
E13CBetn01r01_aligned["2011", "E13CBetn01r01r2"] <- 195.399
E13CBetn01r01_aligned["2012", "E13CBetn01r01r2"] <- 185.882
E13CBetn01r01_aligned["2013", "E13CBetn01r01r2"] <- 136.408
E13CBetn01r01_aligned["2014", "E13CBetn01r01r2"] <- 105.524
E13CBetn01r01_aligned["2015", "E13CBetn01r01r2"] <- 146.429
E13CBetn01r01_aligned["2016", "E13CBetn01r01r2"] <- 50.012
E13CBetn01r01_aligned["2017", "E13CBetn01r01r2"] <- 93.383
E13CBetn01r01_aligned["2018", "E13CBetn01r01r2"] <- 100.689
E13CBetn01r01_aligned["2019", "E13CBetn01r01r2"] <- 33.818
E13CBetn01r01_aligned["2020", "E13CBetn01r01r2"] <- 19.742

#rerun with corrected values
E13CBetn01r01r2_aligned <- series.rwl.plot(E13CBetn01r01, series = "E13CBetn01r01r2", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
                                   )
title(sub= "E13CBetn01r01r2", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#corrections on E13CBetn01r01r3 based on image

E13CBetn01r01_aligned["2006", "E13CBetn01r01r3"] <- 240.608
E13CBetn01r01_aligned["2007", "E13CBetn01r01r3"] <- 118.224
E13CBetn01r01_aligned["2008", "E13CBetn01r01r3"] <- 116.209
E13CBetn01r01_aligned["2009", "E13CBetn01r01r3"] <- 11.005
E13CBetn01r01_aligned["2010", "E13CBetn01r01r3"] <- 71.911
E13CBetn01r01_aligned["2011", "E13CBetn01r01r3"] <- 117.653
E13CBetn01r01_aligned["2012", "E13CBetn01r01r3"] <- 112.981
E13CBetn01r01_aligned["2013", "E13CBetn01r01r3"] <- 83.207
E13CBetn01r01_aligned["2014", "E13CBetn01r01r3"] <- 102.612
E13CBetn01r01_aligned["2015", "E13CBetn01r01r3"] <- 162.341
E13CBetn01r01_aligned["2016", "E13CBetn01r01r3"] <- 28.717
E13CBetn01r01_aligned["2017", "E13CBetn01r01r3"] <- 61.747
E13CBetn01r01_aligned["2018", "E13CBetn01r01r3"] <- 157.786
E13CBetn01r01_aligned["2019", "E13CBetn01r01r3"] <- 78.382
E13CBetn01r01_aligned["2020", "E13CBetn01r01r3"] <- 70.975

#rerun with corrected values
E13CBetn01r01r3_aligned <- series.rwl.plot(E13CBetn01r01, series = "E13CBetn01r01r3", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r01r3", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#corrections on E13CBetn01r01r4 based on image

E13CBetn01r01_aligned["2006", "E13CBetn01r01r4"] <- 139.019
E13CBetn01r01_aligned["2007", "E13CBetn01r01r4"] <- 75.167
E13CBetn01r01_aligned["2008", "E13CBetn01r01r4"] <- 163.983
E13CBetn01r01_aligned["2009", "E13CBetn01r01r4"] <- 152.203
E13CBetn01r01_aligned["2010", "E13CBetn01r01r4"] <- 200.870
E13CBetn01r01_aligned["2011", "E13CBetn01r01r4"] <- 223.398
E13CBetn01r01_aligned["2012", "E13CBetn01r01r4"] <- 209.860
E13CBetn01r01_aligned["2013", "E13CBetn01r01r4"] <- 143.251
E13CBetn01r01_aligned["2014", "E13CBetn01r01r4"] <- 142.599
E13CBetn01r01_aligned["2015", "E13CBetn01r01r4"] <- 245.443
E13CBetn01r01_aligned["2016", "E13CBetn01r01r4"] <- 79.195
E13CBetn01r01_aligned["2019", "E13CBetn01r01r4"] <- 93.258
E13CBetn01r01_aligned["2020", "E13CBetn01r01r4"] <- 14.574

#rerun with corrected values
E13CBetn01r01r4_aligned <- series.rwl.plot(E13CBetn01r01, series = "E13CBetn01r01r4", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r01r4", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#statistics with aligned df####

E13CBetn01r01_aligned_stats <- rwl.stats(E13CBetn01r01_aligned) #summary and stats
print(E13CBetn01r01_aligned_stats)

ms_aligned <- sens2(E13CBetn01r01_aligned) #calculates the mean sensitivity
print(ms_aligned)

rwl_report_aligned <- rwl.report(E13CBetn01r01_aligned)  #report on rwl
print(rwl_report_aligned)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(E13CBetn01r01_aligned) #creates a segment plot
spag.plot(E13CBetn01r01_aligned, zfac=0.01,) #creates a spaghetti plot

##Analysis####
inter_aligned <- interseries.cor(E13CBetn01r01_aligned, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "pearson") #correlation between series and master
print(inter_aligned)

###General correlation####
corr.rwl.seg(rwl = E13CBetn01r01_aligned, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "E13CBetn01r01_aligned", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

#export data

write.rwl(E13CBetn01r01_aligned, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/r/E13CBetn01r01_aligned.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#create a new e13c_bet_n_01_R version with the aligned data.

e13c_bet_n_01_R_aligned <- e13c_bet_n_01_R #creates the df

#substitute with the aligned data

years_E13CBetn01r01 <- 2006:2020 #year gap for update
columns_E13CBetn01r01 <- c("E13CBetn01r01r1", "E13CBetn01r01r2", "E13CBetn01r01r3", "E13CBetn01r01r4") #columns to update

for (col in columns_E13CBetn01r01) {
  e13c_bet_n_01_R_aligned[as.character(years_E13CBetn01r01), col] <- E13CBetn01r01_aligned[as.character(years_E13CBetn01r01), col]
}

#write the new aligned rwl file
write.rwl(e13c_bet_n_01_R_aligned, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/r/e13c_bet_n_01_R_aligned", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )
