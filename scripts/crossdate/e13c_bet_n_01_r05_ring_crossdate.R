#Load packages####
library(dplR)
library(dplyr)
library(signal)
library(treeclim)
library(ggplot2)
library(utils)
library(tibble)

#Load the data####
rwl <- E13CBetn01r05
rwl <- rwl[-1,] #Remove the core year for all samples (2001 for this)
view(rwl)
view(E13CBetn01r05)

#Data Analysis####

#check if you have the same data frame that you want to work with
if (identical(rwl, E13CBetn01r05)) {
  print("Both data frames are exactly the same.")
} else {
  print("Data frames are not exactly the same.")
}

##Statistics####
E13CBetn01r05_stats <- rwl.stats(rwl) #summary and stats
print(E13CBetn01r05_stats)

E13CBetn01r05_ms <- sens2(rwl) #calculates the mean sensitivity
print(E13CBetn01r05_ms)

E13CBetn01r05_report <- rwl.report(rwl)  #report on rwl
print(E13CBetn01r05_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(rwl) #creates a segment plot
spag.plot(rwl, zfac=0.01,) #creates a spaghetti plot

###Analysis####
E13CBetn01r05_inter <- interseries.cor(rwl, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "pearson") #correlation between series and master
print(E13CBetn01r05_inter)

###General correlation####
corr.rwl.seg(rwl = rwl, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "E13CBetn01r05", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

###Series vs Master####
####E13CBetn01r05####
 
#E13CBetn01r04r1
serie <- "E13CBetn01r05r1"
E13CBetn01r05r1 <- series.rwl.plot(rwl, series = serie, 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE)
  title(sub= serie, adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r05r2
serie <- "E13CBetn01r05r2"
E13CBetn01r05r2 <- series.rwl.plot(rwl, series = serie, 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= serie, adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r05r3
serie <- "E13CBetn01r05r3"
E13CBetn01r05r3 <- series.rwl.plot(rwl, series = serie, 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= serie, adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#E13CBetn01r05r4
serie <- "E13CBetn01r05r4"
E13CBetn01r05r4 <- series.rwl.plot(rwl, series = serie, 
                series.yrs = as.numeric(names(series)),
                seg.length = 2, bin.floor = 0, n=NULL,
                prewhiten = FALSE, biweight = FALSE, 
                floor.plus1 = FALSE
                )
title(sub= serie, adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

##Alignment####
#corrections on E13CBetn01r01r1 based on image
rwl_aligned <- E13CBetn01r05_aligned
serie <- "E13CBetn01r05r1"


rwl_aligned["2015", serie] <- 25.549
rwl_aligned["2016", serie] <- 60.498
rwl_aligned["2017", serie] <- 73.760
rwl_aligned["2018", serie] <- 12.902
rwl_aligned["2019", serie] <- 10.773
rwl_aligned["2020", serie] <- 51.618

#re run with corrected values
E13CBetn01r05r1_aligned <- series.rwl.plot(rwl_aligned, series = serie, 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= paste(serie, "(aligned)"), adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title
view(rwl_aligned)

#corrections on E13CBetn01r05r2 based on image
serie <- "E13CBetn01r05r2"

rwl_aligned["2002", serie] <- 231.562
rwl_aligned["2004", serie] <- 101.594
rwl_aligned["2011", serie] <- 3.039
rwl_aligned["2012", serie] <- 17.664
rwl_aligned["2013", serie] <- 2.721
rwl_aligned["2014", serie] <- 5.306
rwl_aligned["2015", serie] <- 2.416
rwl_aligned["2016", serie] <- 8.309
rwl_aligned["2017", serie] <- 12.337
rwl_aligned["2018", serie] <- 1.921
rwl_aligned["2019", serie] <- 0.818
rwl_aligned["2020", serie] <- 5.163

#rerun with corrected values

E13CBetn01r05r2_aligned <- series.rwl.plot(rwl_aligned, series = serie, 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= paste(serie, "(aligned)"), adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title
view(rwl_aligned)

#corrections on E13CBetn01r05r3 based on image
serie <- "E13CBetn01r05r3"

rwl_aligned["2002", serie] <- 248.977
rwl_aligned["2003", serie] <- 148.440
rwl_aligned["2004", serie] <- 50.071
rwl_aligned["2005", serie] <- 8.215
rwl_aligned["2013", serie] <- 36.385
rwl_aligned["2014", serie] <- 76.281
rwl_aligned["2015", serie] <- 73.921
rwl_aligned["2016", serie] <- 101.501
rwl_aligned["2017", serie] <- 123.129
rwl_aligned["2018", serie] <- 50.936
rwl_aligned["2019", serie] <- 48.913
rwl_aligned["2020", serie] <- 133.011

#rerun with corrected values

E13CBetn01r05r2_aligned <- series.rwl.plot(rwl_aligned, series = serie, 
                                           series.yrs = as.numeric(names(series)),
                                           seg.length = 2, bin.floor = 0, n=NULL,
                                           prewhiten = FALSE, biweight = FALSE, 
                                           floor.plus1 = FALSE
                                           )
title(sub= paste(serie, "(aligned)"), adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title
view(rwl_aligned)

#corrections on E13CBetn01r05r4 based on image

serie <- "E13CBetn01r05r4"

rwl_aligned["2016", serie] <- 24.186
rwl_aligned["2017", serie] <- 29.878
rwl_aligned["2018", serie] <- 10.853
rwl_aligned["2019", serie] <- 9.535
rwl_aligned["2020", serie] <- 19.579


#rerun with aligned values
E13CBetn01r05r4_aligned <- series.rwl.plot(rwl_aligned, series = serie, 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE
)
title(sub= paste(serie, "(aligned)"), adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title
view(rwl_aligned)

E13CBetn01r05_aligned <- rwl_aligned

#test if the df was correctly copied

if (identical(rwl_aligned, E13CBetn01r05_aligned)) {
  print("Both data frames are exactly the same.")
} else {
  print("Data frames are not exactly the same.")
}

#statistics with aligned df####

E13CBetn01r05_aligned_stats <- rwl.stats(rwl_aligned) #summary and stats
print(E13CBetn01r04_aligned_stats)

E13CBetn01r05_aligned_ms <- sens2(rwl_aligned) #calculates the mean sensitivity
print(E13CBetn01r05_aligned_ms)

E13CBetn01r05_aligned_report_aligned <- rwl.report(rwl_aligned)  #report on rwl
print(E13CBetn01r05_aligned_report_aligned)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(rwl_aligned) #creates a segment plot
spag.plot(rwl_aligned, zfac=0.01,) #creates a spaghetti plot

##Analysis with aligned df####

E13CBetn01r05_inter_aligned <- interseries.cor(rwl_aligned, n = NULL, prewhiten = FALSE, biweight = FALSE, method = "pearson") #correlation between series and master
print(E13CBetn01r05_inter_aligned)

###General correlation####
corr.rwl.seg(rwl = rwl_aligned, seg.length = 6, bin.floor = 0, n = NULL, prewhiten = FALSE, pcrit = 0.05, biweight = FALSE, method = c("spearman"), make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) #coorelation analysis

title(main = "E13CBetn01r05_aligned", adj = 0.48, line = 4, font.main = 2, cex.main = 1.6) #add title

#export data

write.rwl(E13CBetn01r05_aligned, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/r/E13CBetn01r05_aligned.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#Update the e13c_bet_n_01_R version with the aligned data.


#substitute with the aligned data
years_update <- 2002:2020 #year gap for update
colnames(rwl_aligned) #get the names of the columns
columns_update <- as.character(colnames(rwl_aligned)) #columns to update

for (col in columns_update) {
  e13c_bet_n_01_R_aligned[as.character(years_update), col] <- rwl_aligned[as.character(years_update), col]
}

#confirm that the results are the same for both aligned data frames

#check they that were updated
if (identical(
  e13c_bet_n_01_R_aligned[years_update, columns_update], #confirm the years and columns for each year
  rwl_aligned[years_update, columns_update])) {
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
write.csv(e13c_bet_n_01_R_aligned, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/r/e13c_bet_n_01_R_aligned.csv"
          )
