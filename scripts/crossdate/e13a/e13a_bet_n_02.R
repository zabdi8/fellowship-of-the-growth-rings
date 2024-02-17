#loading packages
library(dplR)

#load rwl files

e13a_bet_n_02_R <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02/e13a_bet_n_02_r.rwl")
e13a_bet_n_02_S <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02/e13a_bet_n_02_s.rwl")
e13a_bet_n_02_T <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02/e13a_bet_n_02_t.rwl")
e13a_bet_n_02_U <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02/e13a_bet_n_02_u.rwl")
e13a_bet_n_02_V <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02/e13a_bet_n_02_v.rwl")




# merge the data


# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13a_bet_n_02_R$row_names <- rownames(e13a_bet_n_02_R)
e13a_bet_n_02_S$row_names <- rownames(e13a_bet_n_02_S)
e13a_bet_n_02_T$row_names <- rownames(e13a_bet_n_02_T)
e13a_bet_n_02_U$row_names <- rownames(e13a_bet_n_02_U)
e13a_bet_n_02_V$row_names <- rownames(e13a_bet_n_02_V)

# Merge the data frames using Reduce and merge
e13a_bet_n_02 <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                        list(e13a_bet_n_02_R, 
                             e13a_bet_n_02_S, 
                             e13a_bet_n_02_T,
                             e13a_bet_n_02_U,
                             e13a_bet_n_02_V)
                        )

# Set row names and remove the extra column
rownames(e13a_bet_n_02) <- e13a_bet_n_02[[common_column]]
e13a_bet_n_02[[common_column]] <- NULL

colnames(e13a_bet_n_02) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13a_bet_n_02 <- e13a_bet_n_02 %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13a_bet_n_02)
colnames(e13a_bet_n_02)
head(e13a_bet_n_02)

write.rwl(e13a_bet_n_02, "data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#reduce the names for easy view
e13a_bet_n_02_short <- e13a_bet_n_02

new_colnames <- sub("^E13ABetn02", "", colnames(e13a_bet_n_02_short))

# Assign the new column names to the data frame
colnames(e13a_bet_n_02_short) <- new_colnames

head(e13a_bet_n_02_short)
colnames(e13a_bet_n_02_short)

#Data Analysis####
##Statistics####
e13a_bet_n_02_stats <- rwl.stats(e13a_bet_n_02) #summary and stats
print(e13a_bet_n_02_stats)

e13a_bet_n_02_ms <- sens2(e13a_bet_n_02) #calculates the mean sensitivity
print(e13a_bet_n_02_ms)

e13a_bet_n_02_report <- rwl.report(e13a_bet_n_02)  #report on rwl
print(e13a_bet_n_02_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13a_bet_n_02_short) #creates a segment plot
title(main = "e13abetn02", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13a_bet_n_02_short, zfac=0.02, cex = 1000) #creates a spaghetti plot
title(main = "e13abetn02", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#average####
#create a Df with the average of each section for e13a_bet_n_01
# Create a new DataFrame with the same row names as the original DataFrame
e13a_bet_n_02_average <- data.frame(RowNames = rownames(e13a_bet_n_02))

# Calculate the average for each set of columns and add them to the new DataFrame
colnames(e13a_bet_n_02)


#e13abetn02r
e13a_bet_n_02_average$E13ABetn02r01 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02r01r1",
                                                                  "E13ABetn02r01r2", 
                                                                  "E13ABetn02r01r3",
                                                                  "E13ABetn02r01r4")], 
                                                na.rm = TRUE)

e13a_bet_n_02_average$E13ABetn02r02 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02r02r1",
                                                                  "E13ABetn02r02r2",
                                                                  "E13ABetn02r02r3",
                                                                  "E13ABetn02r02r4")],
                                                na.rm = TRUE)
e13a_bet_n_02_average$E13ABetn02r03 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02r03r1",
                                                                  "E13ABetn02r03r2",
                                                                  "E13ABetn02r03r3",
                                                                  "E13ABetn02r03r4")], 
                                                na.rm = TRUE)
e13a_bet_n_02_average$E13ABetn02r04 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02r04r1",
                                                                  "E13ABetn02r04r2",
                                                                  "E13ABetn02r04r3",
                                                                  "E13ABetn02r04r4")], 
                                                na.rm = TRUE)
#E13ABetn02s

e13a_bet_n_02_average$E13ABetn02s00 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02s00r1",
                                                                  "E13ABetn02s00r2", 
                                                                  "E13ABetn02s00r3",
                                                                  "E13ABetn02s00r4")], 
                                                na.rm = TRUE)

e13a_bet_n_02_average$E13ABetn02s01 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02s01r1",
                                                                  "E13ABetn02s01r2", 
                                                                  "E13ABetn02s01r3",
                                                                  "E13ABetn02s01r4")], 
                                                na.rm = TRUE)

e13a_bet_n_02_average$E13ABetn02s02 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02s02r1",
                                                                  "E13ABetn02s02r2", 
                                                                  "E13ABetn02s02r3",
                                                                  "E13ABetn02s02r4")], 
                                                na.rm = TRUE)

e13a_bet_n_02_average$E13ABetn02s03 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02s03r1",
                                                                  "E13ABetn02s03r2", 
                                                                  "E13ABetn02s03r3",
                                                                  "E13ABetn02s03r4")], 
                                                na.rm = TRUE)

e13a_bet_n_02_average$E13ABetn02s04 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02s04r1",
                                                                  "E13ABetn02s04r2",
                                                                  "E13ABetn02s04r3",
                                                                  "E13ABetn02s04r4")],
                                                na.rm = TRUE)
e13a_bet_n_02_average$E13ABetn02s05 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02s05r1",
                                                                  "E13ABetn02s05r2", 
                                                                  "E13ABetn02s05r3",
                                                                  "E13ABetn02s05r4")], 
                                                na.rm = TRUE)
e13a_bet_n_02_average$E13ABetn02s06 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02s06r1",
                                                                  "E13ABetn02s06r2",
                                                                  "E13ABetn02s06r3",
                                                                  "E13ABetn02s06r4")], 
                                                na.rm = TRUE)

e13a_bet_n_02_average$E13ABetn02s07 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02s07r1",
                                                                  "E13ABetn02s07r2", 
                                                                  "E13ABetn02s07r3",
                                                                  "E13ABetn02s07r4")], 
                                                na.rm = TRUE)

e13a_bet_n_02_average$E13ABetn02s08 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02s08r1",
                                                                  "E13ABetn02s08r2", 
                                                                  "E13ABetn02s08r3",
                                                                  "E13ABetn02s08r4")], 
                                                na.rm = TRUE)

e13a_bet_n_02_average$E13ABetn02s10 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02s10r1",
                                                                  "E13ABetn02s10r2", 
                                                                  "E13ABetn02s10r3",
                                                                  "E13ABetn02s10r4")], 
                                                na.rm = TRUE)

e13a_bet_n_02_average$E13ABetn02s20 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02s20r1",
                                                                  "E13ABetn02s20r2", 
                                                                  "E13ABetn02s20r3",
                                                                  "E13ABetn02s20r4")], 
                                                na.rm = TRUE)


#E13ABetn02t
e13a_bet_n_02_average$E13ABetn02t01 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02t01r1",
                                                                  "E13ABetn02t01r2", 
                                                                  "E13ABetn02t01r3",
                                                                  "E13ABetn02t01r4")], 
                                                na.rm = TRUE)

e13a_bet_n_02_average$E13ABetn02t05 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02t05r1",
                                                                  "E13ABetn02t05r2",
                                                                  "E13ABetn02t05r3",
                                                                  "E13ABetn02t05r4")],
                                                na.rm = TRUE)
e13a_bet_n_02_average$E13ABetn02t08 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02t08r1",
                                                                  "E13ABetn02t08r2",
                                                                  "E13ABetn02t08r3",
                                                                  "E13ABetn02t08r4")], 
                                                na.rm = TRUE)



#E13ABetn02u
e13a_bet_n_02_average$E13ABetn02u01 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02u01r1",
                                                                  "E13ABetn02u01r2", 
                                                                  "E13ABetn02u01r3",
                                                                  "E13ABetn02u01r4")], 
                                                na.rm = TRUE)

e13a_bet_n_02_average$E13ABetn02u03 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02u03r1",
                                                                  "E13ABetn02u03r2", 
                                                                  "E13ABetn02u03r3",
                                                                  "E13ABetn02u03r4")], 
                                                na.rm = TRUE)

e13a_bet_n_02_average$E13ABetn02u08 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02u08r1",
                                                                  "E13ABetn02u08r2",
                                                                  "E13ABetn02u08r3",
                                                                  "E13ABetn02u08r4")],
                                                na.rm = TRUE)
e13a_bet_n_02_average$E13ABetn02u14 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02u14r1",
                                                                  "E13ABetn02u14r2",
                                                                  "E13ABetn02u14r3",
                                                                  "E13ABetn02u14r4")], 
                                                na.rm = TRUE)

#E13ABetn02v
e13a_bet_n_02_average$E13ABetn02v01 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02v01r1",
                                                                  "E13ABetn02v01r2", 
                                                                  "E13ABetn02v01r3",
                                                                  "E13ABetn02v01r4")], 
                                                na.rm = TRUE)

e13a_bet_n_02_average$E13ABetn02v02 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02v02r1",
                                                                  "E13ABetn02v02r2",
                                                                  "E13ABetn02v02r3",
                                                                  "E13ABetn02v02r4")],
                                                na.rm = TRUE)
e13a_bet_n_02_average$E13ABetn02v04 <- rowMeans(e13a_bet_n_02[, c("E13ABetn02v04r1",
                                                                  "E13ABetn02v04r2",
                                                                  "E13ABetn02v04r3",
                                                                  "E13ABetn02v04r4")], 
                                                na.rm = TRUE)



# Replace "NaN" with NA in each column
e13a_bet_n_02_average <- lapply(e13a_bet_n_02_average, function(x) {
  x[is.nan(x)] <- NA
  return(x)
})

# Convert the result back to a data frame
e13a_bet_n_02_average <- as.data.frame(e13a_bet_n_02_average)

# Replace "NaN" with NA for RowNames column
rownames(e13a_bet_n_02_average) <- e13a_bet_n_02_average$RowNames
e13a_bet_n_02_average <- e13a_bet_n_02_average[, -1]
e13a_bet_n_02_average <- round(e13a_bet_n_02_average, digits = 3) #round to 3 digits


View(e13a_bet_n_02_average)

write.rwl(e13a_bet_n_02_average, "data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02_average.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#analysis####
##Statistics####
e13a_bet_n_02_average_stats <- rwl.stats(e13a_bet_n_02_average) #summary and stats
print(e13a_bet_n_02_average_stats)

e13a_bet_n_02_average_ms <- sens2(e13a_bet_n_02_average) #calculates the mean sensitivity
print(e13a_bet_n_02_average_ms)

e13a_bet_n_02_average_report <- rwl.report(e13a_bet_n_02_average)  #report on rwl
print(e13a_bet_n_02_average_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13a_bet_n_02_average) #creates a segment plot
title(main = "e13abetn02", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13a_bet_n_02_average, zfac=0.009,) #creates a spaghetti plot
title(main = "e13abetn02", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

e13a_bet_n_02_ave_short <- e13a_bet_n_02_average

new_colnames <- sub("^E13ABetn02", "", colnames(e13a_bet_n_02_ave_short))

# Assign the new column names to the data frame
colnames(e13a_bet_n_02_ave_short) <- new_colnames

head(e13a_bet_n_02_ave_short)
colnames(e13a_bet_n_02_ave_short)

spag.plot(e13a_bet_n_02_ave_short, zfac=0.01,) #creates a spaghetti plot
title(main = "E13ABetn02 (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
