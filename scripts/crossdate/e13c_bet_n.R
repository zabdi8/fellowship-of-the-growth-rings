#loading packages
library(dplR)
library(dplyr)
library(ggplot2)
library(ggvis)
library(gridExtra)  ## required to arrange ggplot2 plots in a grid


#load rwl files
#full rwl
e13c_bet_n_01 <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/e13c_bet_n_01.rwl")
e13c_bet_n_03 <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_03/e13c_bet_n_03.rwl")
e13c_bet_n_07 <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/e13c_bet_n_07.rwl")

# merge the data

#full rwls
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_01$row_names <- rownames(e13c_bet_n_01)
e13c_bet_n_03$row_names <- rownames(e13c_bet_n_03)
e13c_bet_n_07$row_names <- rownames(e13c_bet_n_07)

# Merge the data frames using Reduce and merge
e13c_bet_n <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                        list(e13c_bet_n_01, 
                             e13c_bet_n_03, 
                             e13c_bet_n_07)
                     )

# Set row names and remove the extra column
rownames(e13c_bet_n) <- e13c_bet_n[[common_column]]
e13c_bet_n[[common_column]] <- NULL

colnames(e13c_bet_n) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13c_bet_n_07 <- e13c_bet_n_07 %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13c_bet_n)
colnames(e13c_bet_n)
head(e13c_bet_n)

#export the rwl
write.rwl(e13c_bet_n, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#reduce the names for easy view
e13c_bet_n_short <- e13c_bet_n

new_colnames <- sub("^E13CBetn", "", colnames(e13c_bet_n_short))

# Assign the new column names to the data frame
colnames(e13c_bet_n_short) <- new_colnames

head(e13c_bet_n_short)
colnames(e13c_bet_n_short)

#Data Analysis####
##Statistics####
e13c_bet_n_stats <- rwl.stats(e13c_bet_n) #summary and stats
print(e13c_bet_n_stats)

e13c_bet_n_ms <- sens2(e13c_bet_n) #calculates the mean sensitivity
print(e13c_bet_n_ms)

e13c_bet_n_report <- rwl.report(e13c_bet_n)  #report on rwl
print(e13c_bet_n_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_short) #creates a segment plot
spag.plot(e13c_bet_n_short, zfac=0.05, cex = 0.3) #creates a spaghetti plot
title(main = "e13cbetn", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#average####

#average rwl
e13c_bet_n_01_av <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/e13c_bet_n_01_average.rwl")
e13c_bet_n_03_av <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_03/e13c_bet_n_03_average.rwl")
e13c_bet_n_07_av <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/e13c_bet_n_07_average.rwl")

# merge the data

#full rwls
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_01_av$row_names <- rownames(e13c_bet_n_01_av)
e13c_bet_n_03_av$row_names <- rownames(e13c_bet_n_03_av)
e13c_bet_n_07_av$row_names <- rownames(e13c_bet_n_07_av)

# Merge the data frames using Reduce and merge
e13c_bet_n_ave <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                     list(e13c_bet_n_01_av, 
                          e13c_bet_n_03_av, 
                          e13c_bet_n_07_av)
)

# Set row names and remove the extra column
rownames(e13c_bet_n_ave) <- e13c_bet_n_ave[[common_column]]
e13c_bet_n_ave[[common_column]] <- NULL

colnames(e13c_bet_n_ave) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13c_bet_n_07 <- e13c_bet_n_07 %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13c_bet_n_ave)
colnames(e13c_bet_n_ave)
head(e13c_bet_n_ave)

#export the rwl
write.rwl(e13c_bet_n_ave, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_ave.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#reduce the names for easy view
e13c_bet_n_ave_short <- e13c_bet_n_ave

new_colnames <- sub("^E13CBetn", "", colnames(e13c_bet_n_ave_short))

# Assign the new column names to the data frame
colnames(e13c_bet_n_ave_short) <- new_colnames

head(e13c_bet_n_ave_short)
colnames(e13c_bet_n_ave_short)

#analysis####
##Statistics####
e13c_bet_n_ave_stats <- rwl.stats(e13c_bet_n_ave) #summary and stats
print(e13c_bet_n_ave_stats)

e13c_bet_n_ave_ms <- sens2(e13c_bet_n_ave) #calculates the mean sensitivity
print(e13c_bet_n_ave_ms)

e13c_bet_n_ave_report <- rwl.report(e13c_bet_n_ave)  #report on rwl
print(e13c_bet_n_ave_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_ave) #creates a segment plot
spag.plot(e13c_bet_n_ave, zfac=0.009,) #creates a spaghetti plot

spag.plot(e13c_bet_n_ave_short, zfac=0.009,) #creates a spaghetti plot
title(main = "e13cbetn (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title


