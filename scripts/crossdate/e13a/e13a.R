#loading packages
library(dplR)

#load rwl files
e13a_bet_n_01 <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_01.rwl")
e13a_bet_n_02 <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02.rwl")
e13a_bet_n_03 <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03.rwl")

# merge the data

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13a_bet_n_01$row_names <- rownames(e13a_bet_n_01)
e13a_bet_n_02$row_names <- rownames(e13a_bet_n_02)
e13a_bet_n_03$row_names <- rownames(e13a_bet_n_03)

# Merge the data frames using Reduce and merge
e13a <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                        list(e13a_bet_n_01,
                             e13a_bet_n_02,
                             e13a_bet_n_03)
                        )

# Set row names and remove the extra column
rownames(e13a) <- e13a[[common_column]]
e13a[[common_column]] <- NULL

colnames(e13a) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13a <- e13a %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13a)
colnames(e13a)
head(e13a)

write.rwl(e13a, "data/ring_data/aligned/e13a/e13a.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#reduce the names for easy view
e13a_short <- e13a
colnames(e13a_short)
new_colnames <- sub("^E13ABetn", "", colnames(e13a_short))

# Assign the new column names to the data frame
colnames(e13a_short) <- new_colnames

head(e13a_short)
colnames(e13a_short)

if (ncol(e13a) == ncol(e13a_short)) { #just check if both have the same number of columns  
  print("Both data frames have the same number of columns.")
} else {
  print("The number of columns in the data frames are different.")
}

#Data Analysis####
##Statistics####
e13a_stats <- rwl.stats(e13a) #summary and stats
print(e13a_stats)

e13a_ms <- sens2(e13a) #calculates the mean sensitivity
print(e13a_ms)

e13a_report <- rwl.report(e13a)  #report on rwl
print(e13a_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
#filter observations out from 2000s
e13a_filtered <- subset(e13a, rownames(e13a) > 2000)
e13a_short_filtered <- subset(e13a_short, rownames(e13a_short) > 2000)

seg.plot(e13a_filtered) #creates a segment plot
title(main = "e13aBetn", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13a_filtered, zfac=0.05, cex = 0.3) #creates a spaghetti plot
title(main = "e13aBetn", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title


#average####

#load rwl files
e13a_bet_n_01_average <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_01_average.rwl")
e13a_bet_n_02_average <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02_average.rwl")
e13a_bet_n_03_average <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03_average.rwl")

# merge the data

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13a_bet_n_01_average$row_names <- rownames(e13a_bet_n_01_average)
e13a_bet_n_02_average$row_names <- rownames(e13a_bet_n_02_average)
e13a_bet_n_03_average$row_names <- rownames(e13a_bet_n_03_average)

# Merge the data frames using Reduce and merge
e13a_average <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
               list(e13a_bet_n_01_average,
                    e13a_bet_n_02_average,
                    e13a_bet_n_03_average)
               )

# Set row names and remove the extra column
rownames(e13a_average) <- e13a_average[[common_column]]
e13a_average[[common_column]] <- NULL

colnames(e13a_average) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13a <- e13a %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13a_average)
colnames(e13a_average)
head(e13a_average)

write.rwl(e13a_average, "data/ring_data/aligned/e13a/e13a_average.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#reduce the names for easy view
e13a_ave_short <- e13a_average
colnames(e13a_ave_short)
new_colnames <- sub("^E13ABetn", "", colnames(e13a_ave_short))

# Assign the new column names to the data frame
colnames(e13a_ave_short) <- new_colnames

head(e13a_ave_short)
colnames(e13a_ave_short)

if (ncol(e13a_average) == ncol(e13a_ave_short)) { #just check if both have the same number of columns  
  print("Both data frames have the same number of columns.")
} else {
  print("The number of columns in the data frames are different.")
}

#Data Analysis####
##Statistics####
e13a_ave_stats <- rwl.stats(e13a_average) #summary and stats
print(e13a_ave_stats)

e13a_ave_ms <- sens2(e13a_average) #calculates the mean sensitivity
print(e13a_ave_ms)

e13a_ave_report <- rwl.report(e13a_average)  #report on rwl
print(e13a_ave_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
#filter observations out from 2000s
e13a_ave_filtered <- subset(e13a_ave_short, rownames(e13a_ave_short) > 2000)

seg.plot(e13a_ave_filtered) #creates a segment plot
title(main = "e13aBetn (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13a_ave_filtered, zfac=0.02, cex = 0.3) #creates a spaghetti plot
title(main = "e13aBetn (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
