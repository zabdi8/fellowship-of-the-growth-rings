#loading packages
library(dplR)
library(dplyr)
library(ggplot2)
library(ggvis)
library(gridExtra)  ## required to arrange ggplot2 plots in a grid


#load rwl files
e13c_bet_n_03_R <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_03/r/e13c_bet_n_03_r")
e13c_bet_n_03_S <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_03/s/e13c_bet_n_03_s")
e13c_bet_n_03_T <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_03/t/e13c_bet_n_03_t")


# merge the data


# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_03_R$row_names <- rownames(e13c_bet_n_03_R)
e13c_bet_n_03_S$row_names <- rownames(e13c_bet_n_03_S)
e13c_bet_n_03_T$row_names <- rownames(e13c_bet_n_03_T)



# Merge the data frames using Reduce and merge
e13c_bet_n_03 <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                        list(e13c_bet_n_03_R, e13c_bet_n_03_S, e13c_bet_n_03_T))

# Set row names and remove the extra column
rownames(e13c_bet_n_03) <- e13c_bet_n_03[[common_column]]
e13c_bet_n_03[[common_column]] <- NULL

# Remove columns with suffixes .x and .y
e13c_bet_n_03 <- e13c_bet_n_03 %>%
  select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13c_bet_n_03)
colnames(e13c_bet_n_03)

#reduce the names for easy view
e13c_bet_n_03_short <- e13c_bet_n_03

new_colnames <- sub("^E13CBetn03", "", colnames(e13c_bet_n_03_short))

# Assign the new column names to the data frame
colnames(e13c_bet_n_03_short) <- new_colnames

head(e13c_bet_n_03_short)
colnames(e13c_bet_n_03_short)

#Data Analysis####
##Statistics####
e13c_bet_n_03_stats <- rwl.stats(e13c_bet_n_03) #summary and stats
print(e13c_bet_n_03_stats)

e13c_bet_n_03_ms <- sens2(e13c_bet_n_03) #calculates the mean sensitivity
print(e13c_bet_n_03_ms)

e13c_bet_n_03_report <- rwl.report(e13c_bet_n_03)  #report on rwl
print(e13c_bet_n_03_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_03) #creates a segment plot
spag.plot(e13c_bet_n_03_short, zfac=0.02, cex = 0.3) #creates a spaghetti plot
title(main = "e13cbetn03", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.6) #add title

rwl <- e13c_bet_n_03_short
serie <- "s01r1"
e13c_bet_n_03_short_complete <- series.rwl.plot(rwl, series = serie, 
                                            series.yrs = as.numeric(names(series)),
                                            seg.length = 2, bin.floor = 0, n=NULL,
                                            prewhiten = FALSE, biweight = FALSE, 
                                            floor.plus1 = FALSE)
title(sub= "E13CBetn01r01 (complete)", adj = 0.812, line = -4.1, font.sub = 2, cex.sub = 0.9) #add title

#average####
#create a Df with the average of each section for e13c_bet_n_01
# Create a new DataFrame with the same row names as the original DataFrame
e13c_bet_n_03_average <- data.frame(RowNames = rownames(e13c_bet_n_03))

# Calculate the average for each set of columns and add them to the new DataFrame
colnames(e13c_bet_n_03)
#E13CBetn01r
e13c_bet_n_03_average$E13CBetn03r01 <- rowMeans(e13c_bet_n_03[, c("E13CBetn03r01r1",
                                                                  "E13CBetn03r01r2", 
                                                                  "E13CBetn03r01r3",
                                                                  "E13CBetn03r01r4")], 
                                                na.rm = TRUE)

e13c_bet_n_03_average$E13CBetn01r02 <- rowMeans(e13c_bet_n_03[, c("E13CBetn03r02r1",
                                                                  "E13CBetn03r02r2",
                                                                  "E13CBetn03r02r3",
                                                                  "E13CBetn03r02r4")],
                                                na.rm = TRUE)
e13c_bet_n_03_average$E13CBetn01r04 <- rowMeans(e13c_bet_n_03[, c("E13CBetn03r04r1",
                                                                  "E13CBetn03r04r2",
                                                                  "E13CBetn03r04r3",
                                                                  "E13CBetn03r04r4")], 
                                                na.rm = TRUE)


#E13CBetn03s 
e13c_bet_n_03_average$E13CBetn03s01 <- rowMeans(e13c_bet_n_03[, c("E13CBetn03s01r1",
                                                                  "E13CBetn03s01r2", 
                                                                  "E13CBetn03s01r3",
                                                                  "E13CBetn03s01r4")], 
                                                na.rm = TRUE)

e13c_bet_n_03_average$E13CBetn03s05 <- rowMeans(e13c_bet_n_03[, c("E13CBetn03s05r1",
                                                                  "E13CBetn03s05r2",
                                                                  "E13CBetn03s05r3",
                                                                  "E13CBetn03s05r4")],
                                                na.rm = TRUE)
e13c_bet_n_03_average$E13CBetn01s08 <- rowMeans(e13c_bet_n_03[, c("E13CBetn03s08r1",
                                                                  "E13CBetn03s08r2",
                                                                  "E13CBetn03s08r3",
                                                                  "E13CBetn03s08r4")], 
                                                na.rm = TRUE)

#E13CBetn03t 
e13c_bet_n_03_average$E13CBetn01t01 <- rowMeans(e13c_bet_n_03[, c("E13CBetn03t01r1",
                                                                  "E13CBetn03t01r2", 
                                                                  "E13CBetn03t01r3",
                                                                  "E13CBetn03t01r4")], 
                                                na.rm = TRUE)

e13c_bet_n_03_average$E13CBetn01t03 <- rowMeans(e13c_bet_n_03[, c("E13CBetn03t03r1",
                                                                  "E13CBetn03t03r2",
                                                                  "E13CBetn03t03r3",
                                                                  "E13CBetn03t03r4")],
                                                na.rm = TRUE)
e13c_bet_n_03_average$E13CBetn01t04 <- rowMeans(e13c_bet_n_03[, c("E13CBetn03t04r1",
                                                                  "E13CBetn03t04r2",
                                                                  "E13CBetn03t04r3",
                                                                  "E13CBetn03t04r4")], 
                                                na.rm = TRUE)



# Replace "NaN" with NA in each column
e13c_bet_n_03_average <- lapply(e13c_bet_n_03_average, function(x) {
  x[is.nan(x)] <- NA
  return(x)
})

# Convert the result back to a data frame
e13c_bet_n_03_average <- as.data.frame(e13c_bet_n_03_average)

# Replace "NaN" with NA for RowNames column
rownames(e13c_bet_n_03_average) <- e13c_bet_n_03_average$RowNames
e13c_bet_n_03_average <- e13c_bet_n_03_average[, -1]
e13c_bet_n_03_average <- round(e13c_bet_n_03_average, digits = 3) #round to 3 digits


view(e13c_bet_n_03_average)

#analysis####
##Statistics####
e13c_bet_n_03_average_stats <- rwl.stats(e13c_bet_n_03_average) #summary and stats
print(e13c_bet_n_03_average_stats)

e13c_bet_n_03_average_ms <- sens2(e13c_bet_n_03_average) #calculates the mean sensitivity
print(e13c_bet_n_03_average_ms)

e13c_bet_n_03_average_report <- rwl.report(e13c_bet_n_03_average)  #report on rwl
print(e13c_bet_n_03_average_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_03_average) #creates a segment plot
spag.plot(e13c_bet_n_03_average, zfac=0.01,) #creates a spaghetti plot
