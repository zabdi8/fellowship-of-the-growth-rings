#loading packages
library(dplR)

#load rwl files
e13d_bet_n_01_R <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01/e13d_bet_n_01_r.rwl")
e13d_bet_n_01_S <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01/e13d_bet_n_01_s.rwl")
e13d_bet_n_01_T <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01/e13d_bet_n_01_t.rwl")
e13d_bet_n_01_U <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01/e13d_bet_n_01_u.rwl")
e13d_bet_n_01_V <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01/e13d_bet_n_01_v.rwl")

# merge the data

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13d_bet_n_01_R$row_names <- rownames(e13d_bet_n_01_R)
e13d_bet_n_01_S$row_names <- rownames(e13d_bet_n_01_S)
e13d_bet_n_01_T$row_names <- rownames(e13d_bet_n_01_T)
e13d_bet_n_01_U$row_names <- rownames(e13d_bet_n_01_U)
e13d_bet_n_01_V$row_names <- rownames(e13d_bet_n_01_V)

# Merge the data frames using Reduce and merge
e13d_bet_n_01 <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                        list(e13d_bet_n_01_R, 
                             e13d_bet_n_01_S, 
                             e13d_bet_n_01_T,
                             e13d_bet_n_01_U,
                             e13d_bet_n_01_V)
)

# Set row names and remove the extra column
rownames(e13d_bet_n_01) <- e13d_bet_n_01[[common_column]]
e13d_bet_n_01[[common_column]] <- NULL

colnames(e13d_bet_n_01) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13d_bet_n_01 <- e13d_bet_n_01 %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13d_bet_n_01)
colnames(e13d_bet_n_01)
head(e13d_bet_n_01)

write.rwl(e13d_bet_n_01, "data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#reduce the names for easy view
e13d_bet_n_01_short <- e13d_bet_n_01
colnames(e13d_bet_n_01_short)
new_colnames <- sub("^E13DBetn01", "", colnames(e13d_bet_n_01_short))

# Assign the new column names to the data frame
colnames(e13d_bet_n_01_short) <- new_colnames

head(e13d_bet_n_01_short)
colnames(e13d_bet_n_01_short)

if (ncol(e13d_bet_n_01) == ncol(e13d_bet_n_01_short)) { #just check if both have the same number of columns  
  print("Both data frames have the same number of columns.")
} else {
  print("The number of columns in the data frames are different.")
}

#Data Analysis####
##Statistics####
e13d_bet_n_01_stats <- rwl.stats(e13d_bet_n_01) #summary and stats
print(e13d_bet_n_01_stats)

e13d_bet_n_01_ms <- sens2(e13d_bet_n_01) #calculates the mean sensitivity
print(e13d_bet_n_01_ms)

e13d_bet_n_01_report <- rwl.report(e13d_bet_n_01)  #report on rwl
print(e13d_bet_n_01_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13d_bet_n_01_short) #creates a segment plot
title(main = "E13DBetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13d_bet_n_01_short, zfac=0.02, cex = 0.3) #creates a spaghetti plot
title(main = "E13DBetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#average####
#create a Df with the average of each section for e13d_bet_n_01
# Create a new DataFrame with the same row names as the original DataFrame
e13d_bet_n_01_average <- data.frame(RowNames = rownames(e13d_bet_n_01))

# Calculate the average for each set of columns and add them to the new DataFrame
colnames(e13d_bet_n_01)

#E13DBetn01r
e13d_bet_n_01_average$E13DBetn01r01 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01r01r1",
                                                                  "E13DBetn01r01r2", 
                                                                  "E13DBetn01r01r3",
                                                                  "E13DBetn01r01r4")], 
                                                na.rm = TRUE)

e13d_bet_n_01_average$E13DBetn01r07 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01r07r1",
                                                                  "E13DBetn01r07r2",
                                                                  "E13DBetn01r07r3",
                                                                  "E13DBetn01r07r4")], 
                                                na.rm = TRUE)
e13d_bet_n_01_average$E13DBetn01r12 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01r12r1",
                                                                  "E13DBetn01r12r2",
                                                                  "E13DBetn01r12r3",
                                                                  "E13DBetn01r12r4")], 
                                                na.rm = TRUE)


#E13DBetn01s
e13d_bet_n_01_average$E13DBetn01s01 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01s01r1",
                                                                  "E13DBetn01s01r2", 
                                                                  "E13DBetn01s01r3",
                                                                  "E13DBetn01s01r4")], 
                                                na.rm = TRUE)

e13d_bet_n_01_average$E13DBetn01s04 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01s04r1",
                                                                  "E13DBetn01s04r2",
                                                                  "E13DBetn01s04r3",
                                                                  "E13DBetn01s04r4")],
                                                na.rm = TRUE)

e13d_bet_n_01_average$E13DBetn01s08 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01s08r1",
                                                                  "E13DBetn01s08r2",
                                                                  "E13DBetn01s08r3",
                                                                  "E13DBetn01s08r4")], 
                                                na.rm = TRUE)

#E13DBetn01t
e13d_bet_n_01_average$E13DBetn01t01 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01t01r1",
                                                                  "E13DBetn01t01r2", 
                                                                  "E13DBetn01t01r3",
                                                                  "E13DBetn01t01r4")], 
                                                na.rm = TRUE)

e13d_bet_n_01_average$E13DBetn01t02 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01t02r1",
                                                                  "E13DBetn01t02r2", 
                                                                  "E13DBetn01t02r3",
                                                                  "E13DBetn01t02r4")], 
                                                na.rm = TRUE)
e13d_bet_n_01_average$E13DBetn01t03 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01t03r1",
                                                                  "E13DBetn01t03r2", 
                                                                  "E13DBetn01t03r3",
                                                                  "E13DBetn01t03r4")], 
                                                na.rm = TRUE)

e13d_bet_n_01_average$E13DBetn01t04 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01t04r1",
                                                                  "E13DBetn01t04r2",
                                                                  "E13DBetn01t04r3",
                                                                  "E13DBetn01t04r4")],
                                                na.rm = TRUE)

e13d_bet_n_01_average$E13DBetn01t06 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01t06r1",
                                                                  "E13DBetn01t06r2",
                                                                  "E13DBetn01t06r3",
                                                                  "E13DBetn01t06r4")], 
                                                na.rm = TRUE)
#E13DBetn01u
e13d_bet_n_01_average$E13DBetn01u01 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01u01r1",
                                                                  "E13DBetn01u01r2", 
                                                                  "E13DBetn01u01r3",
                                                                  "E13DBetn01u01r4")], 
                                                na.rm = TRUE)
e13d_bet_n_01_average$E13DBetn01u02 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01u02r1",
                                                                  "E13DBetn01u02r2", 
                                                                  "E13DBetn01u02r3",
                                                                  "E13DBetn01u02r4")], 
                                                na.rm = TRUE)
e13d_bet_n_01_average$E13DBetn01u03 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01u03r1",
                                                                  "E13DBetn01u03r2", 
                                                                  "E13DBetn01u03r3",
                                                                  "E13DBetn01u03r4")], 
                                                na.rm = TRUE)
e13d_bet_n_01_average$E13DBetn01u04 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01u04r1",
                                                                  "E13DBetn01u04r2", 
                                                                  "E13DBetn01u04r3",
                                                                  "E13DBetn01u04r4")], 
                                                na.rm = TRUE)
e13d_bet_n_01_average$E13DBetn01u05 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01u05r1",
                                                                  "E13DBetn01u05r2", 
                                                                  "E13DBetn01u05r3",
                                                                  "E13DBetn01u05r4")], 
                                                na.rm = TRUE)
e13d_bet_n_01_average$E13DBetn01u06 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01u06r1",
                                                                  "E13DBetn01u06r2", 
                                                                  "E13DBetn01u06r3",
                                                                  "E13DBetn01u06r4")], 
                                                na.rm = TRUE)

#E13DBetn01v
e13d_bet_n_01_average$E13DBetn01v01 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01v01r1",
                                                                  "E13DBetn01v01r2", 
                                                                  "E13DBetn01v01r3",
                                                                  "E13DBetn01v01r4")], 
                                                na.rm = TRUE)
e13d_bet_n_01_average$E13DBetn01v04 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01v04r1",
                                                                  "E13DBetn01v04r2", 
                                                                  "E13DBetn01v04r3",
                                                                  "E13DBetn01v04r4")], 
                                                na.rm = TRUE)
e13d_bet_n_01_average$E13DBetn01v08 <- rowMeans(e13d_bet_n_01[, c("E13DBetn01v08r1",
                                                                  "E13DBetn01v08r2", 
                                                                  "E13DBetn01v08r3",
                                                                  "E13DBetn01v08r4")], 
                                                na.rm = TRUE)

# Replace "NaN" with NA in each column
e13d_bet_n_01_average <- lapply(e13d_bet_n_01_average, function(x) {
  x[is.nan(x)] <- NA
  return(x)
})

# Convert the result back to a data frame
e13d_bet_n_01_average <- as.data.frame(e13d_bet_n_01_average)

# Replace "NaN" with NA for RowNames column
rownames(e13d_bet_n_01_average) <- e13d_bet_n_01_average$RowNames
e13d_bet_n_01_average <- e13d_bet_n_01_average[, -1]
e13d_bet_n_01_average <- round(e13d_bet_n_01_average, digits = 3) #round to 3 digits


View(e13d_bet_n_01_average)

write.rwl(e13d_bet_n_01_average, "data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01_average.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#analysis####
##Statistics####
e13d_bet_n_01_average_stats <- rwl.stats(e13d_bet_n_01_average) #summary and stats
print(e13d_bet_n_01_average_stats)

e13d_bet_n_01_average_ms <- sens2(e13d_bet_n_01_average) #calculates the mean sensitivity
print(e13d_bet_n_01_average_ms)

e13d_bet_n_01_average_report <- rwl.report(e13d_bet_n_01_average)  #report on rwl
print(e13d_bet_n_01_average_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13d_bet_n_01_average) #creates a segment plot
title(main = "E13DBetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13d_bet_n_01_average, zfac=0.005,) #creates a spaghetti plot
title(main = "E13DBetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#shorten the names
e13d_bet_n_01_ave_short <- e13d_bet_n_01_average

new_colnames <- sub("^E13DBetn01", "", colnames(e13d_bet_n_01_ave_short))

# Assign the new column names to the data frame
colnames(e13d_bet_n_01_ave_short) <- new_colnames

head(e13d_bet_n_01_ave_short)
colnames(e13d_bet_n_01_ave_short)

spag.plot(e13d_bet_n_01_ave_short, zfac=0.005,) #creates a spaghetti plot
title(main = "E13DBetn01 (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

