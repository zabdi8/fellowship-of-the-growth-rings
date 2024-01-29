#Load packages####
library(dplR)
library(signal)
library(treeclim)
library(ggplot2)

#Load the data####
E13CBetn01r02
E13CBetn01r02 <- E13CBetn01r02[-1,] #Remove the core year for all samples (2005 for this)

#Data Analysis####
##Statistics####
E13CBetn01r02_stats <- rwl.stats(E13CBetn01r02) #summary and stats
print(E13CBetn01r02_stats)

E13CBetn01r02_ms <- sens2(E13CBetn01r02) #calculates the mean sensitivity
print(E13CBetn01r02_ms)

E13CBetn01r02_report <- rwl.report(E13CBetn01r02)  #report on rwl
print(E13CBetn01r02_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(E13CBetn01r02) #creates a segment plot
spag.plot(E13CBetn01r02, zfac=0.01,) #creates a spaghetti plot

##Analysis####
E13CBetn01r02_inter <- interseries.cor(E13CBetn01r02, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "pearson") #correlation between series and master
print(E13CBetn01r02_inter)

###General correlation####
corr.rwl.seg(rwl = E13CBetn01r02, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "E13CBetn01r02", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title
E13CBetn01r02r1
###Series vs Master####
####E13CBetn01r02####
#E13CBetn01r02r1
E13CBetn01r02r1 <- series.rwl.plot(E13CBetn01r02, series = "E13CBetn01r02r1", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE)
  title(sub= "E13CBetn01r02r1", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r02r2
E13CBetn01r02r2 <- series.rwl.plot(E13CBetn01r02, series = "E13CBetn01r02r2", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r02r2", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r01r3
E13CBetn01r02r3 <- series.rwl.plot(E13CBetn01r02, series = "E13CBetn01r02r3", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r02r3", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r02r4
E13CBetn01r02r4 <- series.rwl.plot(E13CBetn01r02, series = "E13CBetn01r02r4", 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= "E13CBetn01r02r4", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#Alignment
E13CBetn01r02_aligned <- E13CBetn01r02

#corrections on E13CBetn01r01r1 based on image
E13CBetn01r02_aligned["2007", "E13CBetn01r02r1"] <- 270.872
E13CBetn01r02_aligned["2008", "E13CBetn01r02r1"] <- 152.827
E13CBetn01r02_aligned["2009", "E13CBetn01r02r1"] <- 42.878
E13CBetn01r02_aligned["2010", "E13CBetn01r02r1"] <- 41.463
E13CBetn01r02_aligned["2011", "E13CBetn01r02r1"] <- 80.442
E13CBetn01r02_aligned["2014", "E13CBetn01r02r1"] <- 34.023
E13CBetn01r02_aligned["2015", "E13CBetn01r02r1"] <- 44.078
E13CBetn01r02_aligned["2016", "E13CBetn01r02r1"] <- 44.339
E13CBetn01r02_aligned["2017", "E13CBetn01r02r1"] <- 21.799
E13CBetn01r02_aligned["2018", "E13CBetn01r02r1"] <- 81.807
E13CBetn01r02_aligned["2019", "E13CBetn01r02r1"] <- 18.246

#re run with corrected values
E13CBetn01r02r1_aligned <- series.rwl.plot(E13CBetn01r02_aligned, series = "E13CBetn01r02r1", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r01r1 (aligned)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#corrections on E13CBetn01r02r2 based on image

E13CBetn01r02_aligned["2009", "E13CBetn01r02r2"] <- 38.413
E13CBetn01r02_aligned["2010", "E13CBetn01r02r2"] <- 30.470
E13CBetn01r02_aligned["2011", "E13CBetn01r02r2"] <- 99.793
E13CBetn01r02_aligned["2015", "E13CBetn01r02r2"] <- 37.211
E13CBetn01r02_aligned["2016", "E13CBetn01r02r2"] <- 43.533
E13CBetn01r02_aligned["2017", "E13CBetn01r02r2"] <- 21.830
E13CBetn01r02_aligned["2018", "E13CBetn01r02r2"] <- 31.211
E13CBetn01r02_aligned["2019", "E13CBetn01r02r2"] <- 6.703


view(E13CBetn01r02_aligned)
#rerun with corrected values
E13CBetn01r02r2_aligned <- series.rwl.plot(E13CBetn01r02_aligned, series = "E13CBetn01r02r2", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r02r2 (aligned)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#corrections on E13CBetn01r02r3 based on image

E13CBetn01r02_aligned["2014", "E13CBetn01r02r3"] <- 18.983
E13CBetn01r02_aligned["2015", "E13CBetn01r02r3"] <- 66.173
E13CBetn01r02_aligned["2016", "E13CBetn01r02r3"] <- 73.214
E13CBetn01r02_aligned["2017", "E13CBetn01r02r3"] <- 42.105
E13CBetn01r02_aligned["2018", "E13CBetn01r02r3"] <- 138.130

view(E13CBetn01r02_aligned)
#rerun with corrected values
E13CBetn01r02r3_aligned <- series.rwl.plot(E13CBetn01r02_aligned, series = "E13CBetn01r02r3", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r02r3 (aligned)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#corrections on E13CBetn01r02r4 based on image

E13CBetn01r02_aligned["2012", "E13CBetn01r02r4"] <- 82.511
E13CBetn01r02_aligned["2013", "E13CBetn01r02r4"] <- 52.918
E13CBetn01r02_aligned["2015", "E13CBetn01r02r4"] <- 66.431
E13CBetn01r02_aligned["2016", "E13CBetn01r02r4"] <- 68.844
E13CBetn01r02_aligned["2017", "E13CBetn01r02r4"] <- 28.995
E13CBetn01r02_aligned["2018", "E13CBetn01r02r4"] <- 197.859
E13CBetn01r02_aligned["2019", "E13CBetn01r02r4"] <- 13.572
  
view(E13CBetn01r02_aligned)
#rerun with aligned values
E13CBetn01r02r4_aligned <- series.rwl.plot(E13CBetn01r02_aligned, series = "E13CBetn01r02r4", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= "E13CBetn01r01r4 (aligned)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#statistics with aligned df####

E13CBetn01r02_aligned_stats <- rwl.stats(E13CBetn01r02_aligned) #summary and stats
print(E13CBetn01r02_aligned_stats)

E13CBetn01r02_aligned_ms <- sens2(E13CBetn01r02_aligned) #calculates the mean sensitivity
print(E13CBetn01r02_aligned_ms)

E13CBetn01r02_aligned_report_aligned <- rwl.report(E13CBetn01r02_aligned)  #report on rwl
print(E13CBetn01r02_aligned_report_aligned)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(E13CBetn01r02_aligned) #creates a segment plot
spag.plot(E13CBetn01r02_aligned, zfac=0.01,) #creates a spaghetti plot

##Analysis with aligned df####

E13CBetn01r02_inter_aligned <- interseries.cor(E13CBetn01r02_aligned, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "pearson") #correlation between series and master
print(E13CBetn01r02_inter_aligned)

###General correlation####
corr.rwl.seg(rwl = E13CBetn01r02_aligned, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "E13CBetn01r02_aligned", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

#export data

write.rwl(E13CBetn01r02_aligned, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/r/E13CBetn01r02_aligned.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Update the e13c_bet_n_01_R version with the aligned data.


#substitute with the aligned data
years_E13CBetn01r02 <- 2007:2020 #year gap for update
columns_E13CBetn01r02 <- c("E13CBetn01r02r1", "E13CBetn01r02r2", "E13CBetn01r02r3", "E13CBetn01r02r4") #columns to update

for (col in columns_E13CBetn01r02) {
  e13c_bet_n_01_R_aligned[as.character(years_E13CBetn01r02), col] <- E13CBetn01r02_aligned[as.character(years_E13CBetn01r02), col]
}

#confirm that the results are the same for both aligned data frames

#tell the columns that should have been updated
confirm_newcolumns <- c("E13CBetn01r02r1", "E13CBetn01r02r2", "E13CBetn01r02r3", "E13CBetn01r02r4")

#check they that were updated
if (identical(
  e13c_bet_n_01_R_aligned[2006:2020, confirm_newcolumns], #confirm the years and columns for each year
  E13CBetn01r02_aligned[2006:2020, confirm_newcolumns])) {
  cat(paste("The new data from", paste(confirm_newcolumns, collapse = ", "), "has successfully been updated."))
} else {
  cat("The data is not the same for the specified years.\n")
}

#write the new aligned rwl file
write.rwl(e13c_bet_n_01_R_aligned, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/r/e13c_bet_n_01_R_aligned", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )
