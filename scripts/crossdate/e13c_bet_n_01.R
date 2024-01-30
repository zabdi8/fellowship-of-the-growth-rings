#loading packages
library(dplR)
library(dplyr)
library(ggplot2)
library(ggvis)
library(gridExtra)  ## required to arrange ggplot2 plots in a grid


#load rwl files
e13c_bet_n_01_R <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/r/e13c_bet_n_01_R_aligned")
e13c_bet_n_01_S <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/s/e13c_bet_n_01_S")
e13c_bet_n_01_T <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/t/e13c_bet_n_01_T")
e13c_bet_n_01_U <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/u/e13c_bet_n_01_U")

# merge the data


# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_01_R$row_names <- rownames(e13c_bet_n_01_R)
e13c_bet_n_01_S$row_names <- rownames(e13c_bet_n_01_S)
e13c_bet_n_01_T$row_names <- rownames(e13c_bet_n_01_T)
e13c_bet_n_01_U$row_names <- rownames(e13c_bet_n_01_U)


# Merge the data frames using Reduce and merge
e13c_bet_n_01 <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                          list(e13c_bet_n_01_R, e13c_bet_n_01_S, e13c_bet_n_01_T,e13c_bet_n_01_U))

# Set row names and remove the extra column
rownames(e13c_bet_n_01) <- e13c_bet_n_01[[common_column]]
e13c_bet_n_01[[common_column]] <- NULL

# Remove columns with suffixes .x and .y
e13c_bet_n_01 <- e13c_bet_n_01 %>%
  select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13c_bet_n_01)

#export the df

write.rwl(e13c_bet_n_01, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/e13c_bet_n_01.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#reduce the names for easy view
e13c_bet_n_01_short <- e13c_bet_n_01

new_colnames <- sub("^E13CBetn", "", colnames(e13c_bet_n_01_short))

# Assign the new column names to the data frame
colnames(e13c_bet_n_01_short) <- new_colnames

head(e13c_bet_n_01_short)
colnames(e13c_bet_n_01_short)

#Data Analysis####
##Statistics####
e13c_bet_n_01_stats <- rwl.stats(e13c_bet_n_01) #summary and stats
print(e13c_bet_n_01_stats)

e13c_bet_n_01_ms <- sens2(e13c_bet_n_01) #calculates the mean sensitivity
print(e13c_bet_n_01_ms)

e13c_bet_n_01_report <- rwl.report(e13c_bet_n_01)  #report on rwl
print(e13c_bet_n_01_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_01) #creates a segment plot
spag.plot(e13c_bet_n_01_short, zfac=0.02, cex = 0.3) #creates a spaghetti plot


#average####
#create a Df with the average of each section for e13c_bet_n_01
# Create a new DataFrame with the same row names as the original DataFrame
e13c_bet_n_01_average <- data.frame(RowNames = rownames(e13c_bet_n_01))

# Calculate the average for each set of columns and add them to the new DataFrame
#E13CBetn01r
e13c_bet_n_01_average$E13CBetn01r01 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01r01r1",
                                                                              "E13CBetn01r01r2", 
                                                                              "E13CBetn01r01r3",
                                                                              "E13CBetn01r01r4")], 
                                                  na.rm = TRUE)

e13c_bet_n_01_average$E13CBetn01r02 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01r02r1",
                                                                              "E13CBetn01r02r2",
                                                                              "E13CBetn01r02r3",
                                                                              "E13CBetn01r02r4")],
                                                  na.rm = TRUE)
e13c_bet_n_01_average$E13CBetn01r03 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01r03r1",
                                                                              "E13CBetn01r03r2",
                                                                              "E13CBetn01r03r3",
                                                                              "E13CBetn01r03r4")], 
                                                  na.rm = TRUE)
e13c_bet_n_01_average$E13CBetn01r04 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01r04r1",
                                                                              "E13CBetn01r04r2",
                                                                              "E13CBetn01r04r3",
                                                                              "E13CBetn01r04r4")],
                                                  na.rm = TRUE)
e13c_bet_n_01_average$E13CBetn01r05 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01r05r1",
                                                                              "E13CBetn01r05r2",
                                                                              "E13CBetn01r05r3",
                                                                              "E13CBetn01r05r4")], 
                                                  na.rm = TRUE)

#E13CBetn01s 
e13c_bet_n_01_average$E13CBetn01s01 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01s01r1",
                                                                  "E13CBetn01s01r2", 
                                                                  "E13CBetn01s01r3",
                                                                  "E13CBetn01s01r4")], 
                                                na.rm = TRUE)

e13c_bet_n_01_average$E13CBetn01s02 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01s02r1",
                                                                  "E13CBetn01s02r2",
                                                                  "E13CBetn01s02r3",
                                                                  "E13CBetn01s02r4")],
                                                na.rm = TRUE)
e13c_bet_n_01_average$E13CBetn01s03 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01s03r1",
                                                                  "E13CBetn01s03r2",
                                                                  "E13CBetn01s03r3",
                                                                  "E13CBetn01s03r4")], 
                                                na.rm = TRUE)
e13c_bet_n_01_average$E13CBetn01s04 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01s04r1",
                                                                  "E13CBetn01s04r2",
                                                                  "E13CBetn01s04r3",
                                                                  "E13CBetn01s04r4")],
                                                na.rm = TRUE)
#E13CBetn01t 
e13c_bet_n_01_average$E13CBetn01t01 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01t01r1",
                                                                  "E13CBetn01t01r2", 
                                                                  "E13CBetn01t01r3",
                                                                  "E13CBetn01t01r4")], 
                                                na.rm = TRUE)

e13c_bet_n_01_average$E13CBetn01t02 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01t02r1",
                                                                  "E13CBetn01t02r2",
                                                                  "E13CBetn01t02r3",
                                                                  "E13CBetn01t02r4")],
                                                na.rm = TRUE)
e13c_bet_n_01_average$E13CBetn01t03 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01t03r1",
                                                                  "E13CBetn01t03r2",
                                                                  "E13CBetn01t03r3",
                                                                  "E13CBetn01t03r4")], 
                                                na.rm = TRUE)
e13c_bet_n_01_average$E13CBetn01t04 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01t04r1",
                                                                  "E13CBetn01t04r2",
                                                                  "E13CBetn01t04r3",
                                                                  "E13CBetn01t04r4")],
                                                na.rm = TRUE)
e13c_bet_n_01_average$E13CBetn01t05 <- rowMeans(e13c_bet_n_01[, c("E13CBetn01t05r1",
                                                                  "E13CBetn01t05r2",
                                                                  "E13CBetn01t05r3",
                                                                  "E13CBetn01t05r4")],
                                                na.rm = TRUE)

# Replace "NaN" with NA in each column
e13c_bet_n_01_average <- lapply(e13c_bet_n_01_average, function(x) {
  x[is.nan(x)] <- NA
  return(x)
})

# Convert the result back to a data frame
e13c_bet_n_01_average <- as.data.frame(e13c_bet_n_01_average)

# Replace "NaN" with NA for RowNames column
rownames(e13c_bet_n_01_average) <- e13c_bet_n_01_average$RowNames
e13c_bet_n_01_average <- e13c_bet_n_01_average[, -1]
e13c_bet_n_01_average <- round(e13c_bet_n_01_average, digits = 3) #round to 3 digits


view(e13c_bet_n_01_average)

#export the rwl
write.rwl(e13c_bet_n_01_average, "data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/e13c_bet_n_01_average.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#analysis####
##Statistics####
e13c_bet_n_01_average_stats <- rwl.stats(e13c_bet_n_01_average) #summary and stats
print(e13c_bet_n_01_average_stats)

e13c_bet_n_01_average_ms <- sens2(e13c_bet_n_01_average) #calculates the mean sensitivity
print(e13c_bet_n_01_average_ms)

e13c_bet_n_01_average_report <- rwl.report(e13c_bet_n_01_average)  #report on rwl
print(e13c_bet_n_01_average_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_01_average) #creates a segment plot
spag.plot(e13c_bet_n_01_average, zfac=0.005,) #creates a spaghetti plot
title(main = "e13cbetn01 (Average)", adj = 0.48, line = 5, font.main = 2, cex.main = 1.2) #add title
