#loading packages
library(dplR)
library(dplyr)


#load rwl files
e13d_bet_n_01 <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01.rwl")
e13d_bet_n_02 <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_02.rwl")
e13d_bet_n_03 <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03.rwl")

# merge the data

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13d_bet_n_01$row_names <- rownames(e13d_bet_n_01)
e13d_bet_n_02$row_names <- rownames(e13d_bet_n_02)
e13d_bet_n_03$row_names <- rownames(e13d_bet_n_03)

# Merge the data frames using Reduce and merge
e13d <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
               list(e13d_bet_n_01,
                    e13d_bet_n_02,
                    e13d_bet_n_03)
)

# Set row names and remove the extra column
rownames(e13d) <- e13d[[common_column]]
e13d[[common_column]] <- NULL

colnames(e13d) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13d <- e13d %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13d)
colnames(e13d)
head(e13d)

write.rwl(e13d, "data/ring_data/aligned/e13d/e13d.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#reduce the names for easy view
e13d_short <- e13d
colnames(e13d_short)
new_colnames <- sub("^E13DBetn", "", colnames(e13d_short))

# Assign the new column names to the data frame
colnames(e13d_short) <- new_colnames

head(e13d_short)
colnames(e13d_short)

if (ncol(e13d) == ncol(e13d_short)) { #just check if both have the same number of columns  
  print("Both data frames have the same number of columns.")
} else {
  print("The number of columns in the data frames are different.")
}

#Data Analysis####
##Statistics####
e13d_stats <- rwl.stats(e13d) #summary and stats
print(e13d_stats)

e13d_ms <- sens2(e13d) #calculates the mean sensitivity
print(e13d_ms)

e13d_report <- rwl.report(e13d)  #report on rwl
print(e13d_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs

seg.plot(e13d_short) #creates a segment plot
title(main = "E13DBetn", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13d_short, zfac=0.03, cex = 0.3) #creates a spaghetti plot
title(main = "E13DBetn", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title


#average####

#load rwl files
e13d_bet_n_01_average <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01_average.rwl")
e13d_bet_n_02_average <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_02_average.rwl")
e13d_bet_n_03_average <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03_average.rwl")

# merge the data

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13d_bet_n_01_average$row_names <- rownames(e13d_bet_n_01_average)
e13d_bet_n_02_average$row_names <- rownames(e13d_bet_n_02_average)
e13d_bet_n_03_average$row_names <- rownames(e13d_bet_n_03_average)

# Merge the data frames using Reduce and merge
e13d_average <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                       list(e13d_bet_n_01_average,
                            e13d_bet_n_02_average,
                            e13d_bet_n_03_average)
)

# Set row names and remove the extra column
rownames(e13d_average) <- e13d_average[[common_column]]
e13d_average[[common_column]] <- NULL

colnames(e13d_average) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13d <- e13d %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13d_average)
colnames(e13d_average)
head(e13d_average)

write.rwl(e13d_average, "data/ring_data/aligned/e13d/e13d_average.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#reduce the names for easy view
e13d_ave_short <- e13d_average
colnames(e13d_ave_short)
new_colnames <- sub("^E13DBetn", "", colnames(e13d_ave_short))

# Assign the new column names to the data frame
colnames(e13d_ave_short) <- new_colnames

head(e13d_ave_short)
colnames(e13d_ave_short)

if (ncol(e13d_average) == ncol(e13d_ave_short)) { #just check if both have the same number of columns  
  print("Both data frames have the same number of columns.")
} else {
  print("The number of columns in the data frames are different.")
}

#Data Analysis####
##Statistics####
e13d_ave_stats <- rwl.stats(e13d_average) #summary and stats
print(e13d_ave_stats)

e13d_ave_ms <- sens2(e13d_average) #calculates the mean sensitivity
print(e13d_ave_ms)

e13d_ave_report <- rwl.report(e13d_average)  #report on rwl
print(e13d_ave_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
#filter observations out from 2000s
seg.plot(e13d_ave_short) #creates a segment plot
title(main = "E13DBetn (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13d_ave_short, zfac=0.01, cex = 0.3) #creates a spaghetti plot
title(main = "E13DBetn (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

