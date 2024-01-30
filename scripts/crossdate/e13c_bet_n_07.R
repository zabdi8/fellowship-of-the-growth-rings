#loading packages
library(dplR)
library(dplyr)
library(ggplot2)
library(ggvis)
library(gridExtra)  ## required to arrange ggplot2 plots in a grid


#load rwl files
e13c_bet_n_07_O <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/o/e13c_bet_n_07_o")
e13c_bet_n_07_P <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/p/e13c_bet_n_07_p")
e13c_bet_n_07_Q <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/q/e13c_bet_n_07_q")
e13c_bet_n_07_R <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/r/e13c_bet_n_07_r")
e13c_bet_n_07_S <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/s/e13c_bet_n_07_s")
e13c_bet_n_07_T <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/t/e13c_bet_n_07_t")



# merge the data


# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13c_bet_n_07_O$row_names <- rownames(e13c_bet_n_07_O)
e13c_bet_n_07_P$row_names <- rownames(e13c_bet_n_07_P)
e13c_bet_n_07_Q$row_names <- rownames(e13c_bet_n_07_Q)
e13c_bet_n_07_R$row_names <- rownames(e13c_bet_n_07_R)
e13c_bet_n_07_S$row_names <- rownames(e13c_bet_n_07_S)
e13c_bet_n_07_T$row_names <- rownames(e13c_bet_n_07_T)




# Merge the data frames using Reduce and merge
e13c_bet_n_07 <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                        list(e13c_bet_n_07_O, 
                             e13c_bet_n_07_P, 
                             e13c_bet_n_07_Q, 
                             e13c_bet_n_07_R, 
                             e13c_bet_n_07_S, 
                             e13c_bet_n_07_T)
                        )

# Set row names and remove the extra column
rownames(e13c_bet_n_07) <- e13c_bet_n_07[[common_column]]
e13c_bet_n_07[[common_column]] <- NULL

colnames(e13c_bet_n_07) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13c_bet_n_07 <- e13c_bet_n_07 %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13c_bet_n_07)
colnames(e13c_bet_n_07)
head(e13c_bet_n_07)

#reduce the names for easy view
e13c_bet_n_07_short <- e13c_bet_n_07

new_colnames <- sub("^E13CBetn07", "", colnames(e13c_bet_n_07_short))

# Assign the new column names to the data frame
colnames(e13c_bet_n_07_short) <- new_colnames

head(e13c_bet_n_07_short)
colnames(e13c_bet_n_07_short)

#Data Analysis####
##Statistics####
e13c_bet_n_07_stats <- rwl.stats(e13c_bet_n_07) #summary and stats
print(e13c_bet_n_07_stats)

e13c_bet_n_07_ms <- sens2(e13c_bet_n_07) #calculates the mean sensitivity
print(e13c_bet_n_07_ms)

e13c_bet_n_07_report <- rwl.report(e13c_bet_n_07)  #report on rwl
print(e13c_bet_n_07_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_07_short) #creates a segment plot
spag.plot(e13c_bet_n_07_short, zfac=0.02, cex = 0.3) #creates a spaghetti plot
title(main = "e13cbetn07", adj = 0.48, line = 5.5, font.main = 2, cex.main = 1.6) #add title

#average####
#create a Df with the average of each section for e13c_bet_n_01
# Create a new DataFrame with the same row names as the original DataFrame
e13c_bet_n_07_average <- data.frame(RowNames = rownames(e13c_bet_n_07))

# Calculate the average for each set of columns and add them to the new DataFrame
colnames(e13c_bet_n_07)

#E13CBetn07o
e13c_bet_n_07_average$E13CBetn07o01 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07o01r1",
                                                                  "E13CBetn07o01r2", 
                                                                  "E13CBetn07o01r3",
                                                                  "E13CBetn07o01r4")], 
                                                na.rm = TRUE)

e13c_bet_n_07_average$E13CBetn07o05 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07o05r1",
                                                                  "E13CBetn07o05r2",
                                                                  "E13CBetn07o05r3",
                                                                  "E13CBetn07o05r4")],
                                                na.rm = TRUE)
e13c_bet_n_07_average$E13CBetn07o08 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07o08r1",
                                                                  "E13CBetn07o08r2",
                                                                  "E13CBetn07o08r3",
                                                                  "E13CBetn07o08r4")], 
                                                na.rm = TRUE)


#E13CBetn07p 
e13c_bet_n_07_average$E13CBetn07p01 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07p01r1",
                                                                  "E13CBetn07p01r2", 
                                                                  "E13CBetn07p01r3",
                                                                  "E13CBetn07p01r4")], 
                                                na.rm = TRUE)

e13c_bet_n_07_average$E13CBetn07p03 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07p03r1",
                                                                  "E13CBetn07p03r2",
                                                                  "E13CBetn07p03r3",
                                                                  "E13CBetn07p03r4")],
                                                na.rm = TRUE)
e13c_bet_n_07_average$E13CBetn07p04 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07p04r1",
                                                                  "E13CBetn07p04r2",
                                                                  "E13CBetn07p04r3",
                                                                  "E13CBetn07p04r4")], 
                                                na.rm = TRUE)

#E13CBetn07q
e13c_bet_n_07_average$E13CBetn07q01 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07q01r1",
                                                                  "E13CBetn07q01r2", 
                                                                  "E13CBetn07q01r3",
                                                                  "E13CBetn07q01r4")], 
                                                na.rm = TRUE)

e13c_bet_n_07_average$E13CBetn07q02 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07q02r1",
                                                                  "E13CBetn07q02r2",
                                                                  "E13CBetn07q02r3",
                                                                  "E13CBetn07q02r4")],
                                                na.rm = TRUE)
e13c_bet_n_07_average$E13CBetn07q04 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07q04r1",
                                                                  "E13CBetn07q04r2",
                                                                  "E13CBetn07q04r3",
                                                                  "E13CBetn07q04r4")], 
                                                na.rm = TRUE)
#E13CBetn07r
e13c_bet_n_07_average$E13CBetn07r01 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07r01r1",
                                                                  "E13CBetn07r01r2", 
                                                                  "E13CBetn07r01r3",
                                                                  "E13CBetn07r01r4")], 
                                                na.rm = TRUE)

e13c_bet_n_07_average$E13CBetn07r04 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07r04r1",
                                                                  "E13CBetn07r04r2",
                                                                  "E13CBetn07r04r3",
                                                                  "E13CBetn07r04r4")],
                                                na.rm = TRUE)
e13c_bet_n_07_average$E13CBetn07r06 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07r06r1",
                                                                  "E13CBetn07r06r2",
                                                                  "E13CBetn07r06r3",
                                                                  "E13CBetn07r06r4")], 
                                                na.rm = TRUE)
#E13CBetn07s
e13c_bet_n_07_average$E13CBetn07s01 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07s01r1",
                                                                  "E13CBetn07s01r2", 
                                                                  "E13CBetn07s01r3",
                                                                  "E13CBetn07s01r4")], 
                                                na.rm = TRUE)

e13c_bet_n_07_average$E13CBetn07s04 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07s04r1",
                                                                  "E13CBetn07s04r2",
                                                                  "E13CBetn07s04r3",
                                                                  "E13CBetn07s04r4")],
                                                na.rm = TRUE)
e13c_bet_n_07_average$E13CBetn07s06 <- rowMeans(e13c_bet_n_07[, c("E13CBetn07s06r1",
                                                                  "E13CBetn07s06r2",
                                                                  "E13CBetn07s06r3",
                                                                  "E13CBetn07s06r4")], 
                                                na.rm = TRUE)



# Replace "NaN" with NA in each column
e13c_bet_n_07_average <- lapply(e13c_bet_n_07_average, function(x) {
  x[is.nan(x)] <- NA
  return(x)
})

# Convert the result back to a data frame
e13c_bet_n_07_average <- as.data.frame(e13c_bet_n_07_average)

# Replace "NaN" with NA for RowNames column
rownames(e13c_bet_n_07_average) <- e13c_bet_n_07_average$RowNames
e13c_bet_n_07_average <- e13c_bet_n_07_average[, -1]
e13c_bet_n_07_average <- round(e13c_bet_n_07_average, digits = 3) #round to 3 digits


view(e13c_bet_n_07_average)

#analysis####
##Statistics####
e13c_bet_n_07_average_stats <- rwl.stats(e13c_bet_n_07_average) #summary and stats
print(e13c_bet_n_07_average_stats)

e13c_bet_n_07_average_ms <- sens2(e13c_bet_n_07_average) #calculates the mean sensitivity
print(e13c_bet_n_07_average_ms)

e13c_bet_n_07_average_report <- rwl.report(e13c_bet_n_07_average)  #report on rwl
print(e13c_bet_n_07_average_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13c_bet_n_07_average) #creates a segment plot
spag.plot(e13c_bet_n_07_average, zfac=0.005,) #creates a spaghetti plot
