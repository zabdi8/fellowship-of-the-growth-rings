#Load packages####
library(dplR)
library(signal)
library(treeclim)
library(ggplot2)

#Load the data####
E13CBetn01r04
E13CBetn01r04 <- E13CBetn01r04[-1,] #Remove the core year for all samples (2002 for this)

#Data Analysis####
##Statistics####
E13CBetn01r04_stats <- rwl.stats(E13CBetn01r04) #summary and stats
print(E13CBetn01r04_stats)

E13CBetn01r04_ms <- sens2(E13CBetn01r04) #calculates the mean sensitivity
print(E13CBetn01r04_ms)

E13CBetn01r04_report <- rwl.report(E13CBetn01r04)  #report on rwl
print(E13CBetn01r04_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(E13CBetn01r04) #creates a segment plot
spag.plot(E13CBetn01r04, zfac=0.01,) #creates a spaghetti plot

##Analysis####
E13CBetn01r04_inter <- interseries.cor(E13CBetn01r04, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "pearson") #correlation between series and master
print(E13CBetn01r04_inter)

###General correlation####
corr.rwl.seg(rwl = E13CBetn01r04, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "E13CBetn01r04", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

###Series vs Master####
####E13CBetn01r03####
 
#E13CBetn01r03r1
E13CBetn01r04r1 <- series.rwl.plot(E13CBetn01r04, series = "E13CBetn01r04r1", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE)
  title(sub= "E13CBetn01r04r1", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r03r2
E13CBetn01r04r2 <- series.rwl.plot(E13CBetn01r04, series = "E13CBetn01r04r2", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r04r2", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r03r3
E13CBetn01r04r3 <- series.rwl.plot(E13CBetn01r04, series = "E13CBetn01r04r3", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r04r3", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r03r4
E13CBetn01r04r4 <- series.rwl.plot(E13CBetn01r04, series = "E13CBetn01r04r4", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r04r4", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#Alignment
E13CBetn01r04_aligned <- E13CBetn01r04

#corrections on E13CBetn01r01r1 based on image
E13CBetn01r04_aligned["2002", "E13CBetn01r04r1"] <- 153.773
E13CBetn01r04_aligned["2003", "E13CBetn01r04r1"] <- 48.263
E13CBetn01r04_aligned["2004", "E13CBetn01r04r1"] <- 90.707
E13CBetn01r04_aligned["2005", "E13CBetn01r04r1"] <- 47.318
E13CBetn01r04_aligned["2006", "E13CBetn01r04r1"] <- 53.289
E13CBetn01r04_aligned["2007", "E13CBetn01r04r1"] <- 58.332
E13CBetn01r04_aligned["2008", "E13CBetn01r04r1"] <- 135.065
E13CBetn01r04_aligned["2009", "E13CBetn01r04r1"] <- 145.912
E13CBetn01r04_aligned["2010", "E13CBetn01r04r1"] <- 42.393
E13CBetn01r04_aligned["2011", "E13CBetn01r04r1"] <- 56.990
E13CBetn01r04_aligned["2012", "E13CBetn01r04r1"] <- 76.405
E13CBetn01r04_aligned["2016", "E13CBetn01r04r1"] <- 9.819
E13CBetn01r04_aligned["2017", "E13CBetn01r04r1"] <- 5.893
E13CBetn01r04_aligned["2018", "E13CBetn01r04r1"] <- 9.751
E13CBetn01r04_aligned["2019", "E13CBetn01r04r1"] <- 7.047
E13CBetn01r04_aligned["2020", "E13CBetn01r04r1"] <- 63.312

#re run with corrected values
E13CBetn01r04r1_aligned <- series.rwl.plot(E13CBetn01r04_aligned, series = "E13CBetn01r04r1", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r04r1 (aligned)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title
view(E13CBetn01r04_aligned)

#corrections on E13CBetn01r02r2 based on image

E13CBetn01r04_aligned["2004", "E13CBetn01r04r2"] <- 112.729
E13CBetn01r04_aligned["2005", "E13CBetn01r04r2"] <- 67.531
E13CBetn01r04_aligned["2006", "E13CBetn01r04r2"] <- 94.710
E13CBetn01r04_aligned["2007", "E13CBetn01r04r2"] <- 138.007
E13CBetn01r04_aligned["2008", "E13CBetn01r04r2"] <- 268.723
E13CBetn01r04_aligned["2009", "E13CBetn01r04r2"] <- 263.792
E13CBetn01r04_aligned["2010", "E13CBetn01r04r2"] <- 114.830
E13CBetn01r04_aligned["2011", "E13CBetn01r04r2"] <- 105.971
E13CBetn01r04_aligned["2012", "E13CBetn01r04r2"] <- 103.607
E13CBetn01r04_aligned["2013", "E13CBetn01r04r2"] <- 8.427

#rerun with corrected values
E13CBetn01r04r2_aligned <- series.rwl.plot(E13CBetn01r04_aligned, series = "E13CBetn01r04r2", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r04r2 (aligned)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title
view(E13CBetn01r04_aligned)

#corrections on E13CBetn01r04r3 based on image

E13CBetn01r04_aligned["2003", "E13CBetn01r04r3"] <- 46.066
E13CBetn01r04_aligned["2004", "E13CBetn01r04r3"] <- 123.148
E13CBetn01r04_aligned["2008", "E13CBetn01r04r3"] <- 257.958
E13CBetn01r04_aligned["2009", "E13CBetn01r04r3"] <- 266.754
E13CBetn01r04_aligned["2010", "E13CBetn01r04r3"] <- 124.362
E13CBetn01r04_aligned["2011", "E13CBetn01r04r3"] <- 95.449
E13CBetn01r04_aligned["2012", "E13CBetn01r04r3"] <- 105.181



#rerun with corrected values
E13CBetn01r04r3_aligned <- series.rwl.plot(E13CBetn01r04_aligned, series = "E13CBetn01r04r3", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r04r3 (aligned)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title
view(E13CBetn01r04_aligned)

#corrections on E13CBetn01r04r4 based on image

E13CBetn01r04_aligned["2005", "E13CBetn01r04r4"] <- 54.258
E13CBetn01r04_aligned["2006", "E13CBetn01r04r4"] <- 63.490
E13CBetn01r04_aligned["2007", "E13CBetn01r04r4"] <- 67.249
E13CBetn01r04_aligned["2008", "E13CBetn01r04r4"] <- 162.842
E13CBetn01r04_aligned["2009", "E13CBetn01r04r4"] <- 170.012
E13CBetn01r04_aligned["2010", "E13CBetn01r04r4"] <- 95.367
E13CBetn01r04_aligned["2011", "E13CBetn01r04r4"] <- 95.379
E13CBetn01r04_aligned["2012", "E13CBetn01r04r4"] <- 159.183
E13CBetn01r04_aligned["2013", "E13CBetn01r04r4"] <- 49.040
E13CBetn01r04_aligned["2014", "E13CBetn01r04r4"] <- 126.386
E13CBetn01r04_aligned["2015", "E13CBetn01r04r4"] <- 135.664
E13CBetn01r04_aligned["2016", "E13CBetn01r04r4"] <- 69.837
E13CBetn01r04_aligned["2018", "E13CBetn01r04r4"] <- 51.844
E13CBetn01r04_aligned["2019", "E13CBetn01r04r4"] <- 30.469
E13CBetn01r04_aligned["2020", "E13CBetn01r04r4"] <- 169.961


#rerun with aligned values
E13CBetn01r04r4_aligned <- series.rwl.plot(E13CBetn01r04_aligned, series = "E13CBetn01r04r4", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r04r4 (aligned)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title
view(E13CBetn01r04_aligned)

#statistics with aligned df####

E13CBetn01r04_aligned_stats <- rwl.stats(E13CBetn01r04_aligned) #summary and stats
print(E13CBetn01r04_aligned_stats)

E13CBetn01r04_aligned_ms <- sens2(E13CBetn01r04_aligned) #calculates the mean sensitivity
print(E13CBetn01r04_aligned_ms)

E13CBetn01r04_aligned_report_aligned <- rwl.report(E13CBetn01r04_aligned)  #report on rwl
print(E13CBetn01r04_aligned_report_aligned)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(E13CBetn01r04_aligned) #creates a segment plot
spag.plot(E13CBetn01r04_aligned, zfac=0.01,) #creates a spaghetti plot

##Analysis with aligned df####

E13CBetn01r03_inter_aligned <- interseries.cor(E13CBetn01r03_aligned, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "pearson") #correlation between series and master
print(E13CBetn01r03_inter_aligned)

###General correlation####
corr.rwl.seg(rwl = E13CBetn01r02_aligned, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "E13CBetn01r02_aligned", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

#export data

write.rwl(E13CBetn01r04_aligned, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/r/E13CBetn01r04_aligned.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Update the e13c_bet_n_01_R version with the aligned data.


#substitute with the aligned data
years_update <- 2002:2020 #year gap for update
colnames(E13CBetn01r04_aligned) #get the names of the columns
columns_update <- c("E13CBetn01r04r1", "E13CBetn01r04r2", "E13CBetn01r04r3", "E13CBetn01r04r4") #columns to update

for (col in columns_update) {
  e13c_bet_n_01_R_aligned[as.character(years_update), col] <- E13CBetn01r04_aligned[as.character(years_update), col]
}

#confirm that the results are the same for both aligned data frames

#check they that were updated
if (identical(
  e13c_bet_n_01_R_aligned[years_update, columns_update], #confirm the years and columns for each year
  E13CBetn01r04_aligned[years_update, columns_update])) {
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
