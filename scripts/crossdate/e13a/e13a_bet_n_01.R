#loading packages
library(dplR)

#load rwl files
e13a_bet_n_01_R <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_01/e13a_bet_n_01_r.rwl")
e13a_bet_n_01_S <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_01/e13a_bet_n_01_s.rwl")
e13a_bet_n_01_T <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_01/e13a_bet_n_01_t.rwl")




# merge the data


# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13a_bet_n_01_R$row_names <- rownames(e13a_bet_n_01_R)
e13a_bet_n_01_S$row_names <- rownames(e13a_bet_n_01_S)
e13a_bet_n_01_T$row_names <- rownames(e13a_bet_n_01_T)




# Merge the data frames using Reduce and merge
e13a_bet_n_01 <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                        list(e13a_bet_n_01_R, 
                             e13a_bet_n_01_S, 
                             e13a_bet_n_01_T)
                        )

# Set row names and remove the extra column
rownames(e13a_bet_n_01) <- e13a_bet_n_01[[common_column]]
e13a_bet_n_01[[common_column]] <- NULL

colnames(e13a_bet_n_01) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13a_bet_n_01 <- e13a_bet_n_01 %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13a_bet_n_01)
colnames(e13a_bet_n_01)
head(e13a_bet_n_01)

write.rwl(e13a_bet_n_01, "data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_01.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#reduce the names for easy view
e13a_bet_n_01_short <- e13a_bet_n_01

new_colnames <- sub("^E13ABetn01", "", colnames(e13a_bet_n_01_short))

# Assign the new column names to the data frame
colnames(e13a_bet_n_01_short) <- new_colnames

head(e13a_bet_n_01_short)
colnames(e13a_bet_n_01_short)

#Data Analysis####
##Statistics####
e13a_bet_n_01_stats <- rwl.stats(e13a_bet_n_01) #summary and stats
print(e13a_bet_n_01_stats)

e13a_bet_n_01_ms <- sens2(e13a_bet_n_01) #calculates the mean sensitivity
print(e13a_bet_n_01_ms)

e13a_bet_n_01_report <- rwl.report(e13a_bet_n_01)  #report on rwl
print(e13a_bet_n_01_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13a_bet_n_01_short) #creates a segment plot
title(main = "e13abetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13a_bet_n_01_short, zfac=0.02, cex = 0.3) #creates a spaghetti plot
title(main = "e13abetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

# remove all data previous to 2000s
e13a_bet_n_01_filtered <- subset(e13a_bet_n_01_short, rownames(e13a_bet_n_01_short) > 1999)
spag.plot(e13a_bet_n_01_filtered, zfac=0.04, cex = 0.3) #creates a spaghetti plot
title(main = "e13abetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#average####
#create a Df with the average of each section for e13c_bet_n_01
# Create a new DataFrame with the same row names as the original DataFrame
e13a_bet_n_01_average <- data.frame(RowNames = rownames(e13a_bet_n_01))

# Calculate the average for each set of columns and add them to the new DataFrame
colnames(e13a_bet_n_01)

#e13abetn01r
e13a_bet_n_01_average$E13ABetn01r01 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01r01r1",
                                                                  "E13ABetn01r01r2", 
                                                                  "E13ABetn01r01r3",
                                                                  "E13ABetn01r01r4")], 
                                                na.rm = TRUE)

e13a_bet_n_01_average$E13ABetn01r02 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01r02r1",
                                                                  "E13ABetn01r02r2",
                                                                  "E13ABetn01r02r3",
                                                                  "E13ABetn01r02r4")], 
                                                na.rm = TRUE)
e13a_bet_n_01_average$E13ABetn01r03 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01r03r1",
                                                                  "E13ABetn01r03r2",
                                                                  "E13ABetn01r03r3",
                                                                  "E13ABetn01r03r4")], 
                                                na.rm = TRUE)
e13a_bet_n_01_average$E13ABetn01r04 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01r04r1",
                                                                  "E13ABetn01r04r2",
                                                                  "E13ABetn01r04r3",
                                                                  "E13ABetn01r04r4")],
                                                na.rm = TRUE)
e13a_bet_n_01_average$E13ABetn01r05 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01r05r1",
                                                                  "E13ABetn01r05r2",
                                                                  "E13ABetn01r05r3",
                                                                  "E13ABetn01r05r4")], 
                                                na.rm = TRUE)
e13a_bet_n_01_average$E13ABetn01r06 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01r06r1",
                                                                  "E13ABetn01r06r2",
                                                                  "E13ABetn01r06r3",
                                                                  "E13ABetn01r06r4")], 
                                                na.rm = TRUE)

e13a_bet_n_01_average$E13ABetn01r07 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01r07r1",
                                                                  "E13ABetn01r07r2",
                                                                  "E13ABetn01r07r3",
                                                                  "E13ABetn01r07r4")], 
                                                na.rm = TRUE)
e13a_bet_n_01_average$E13ABetn01r08 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01r08r1",
                                                                  "E13ABetn01r08r2",
                                                                  "E13ABetn01r08r3",
                                                                  "E13ABetn01r08r4")], 
                                                na.rm = TRUE)
e13a_bet_n_01_average$E13ABetn01r09 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01r09r1",
                                                                  "E13ABetn01r09r2",
                                                                  "E13ABetn01r09r3",
                                                                  "E13ABetn01r09r4")], 
                                                na.rm = TRUE)

#E13ABetn01s
e13a_bet_n_01_average$E13ABetn01s01 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01s01r1",
                                                                  "E13ABetn01s01r2", 
                                                                  "E13ABetn01s01r3",
                                                                  "E13ABetn01s01r4")], 
                                                na.rm = TRUE)
e13a_bet_n_01_average$E13ABetn01s02 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01s02r1",
                                                                  "E13ABetn01s02r2", 
                                                                  "E13ABetn01s02r3",
                                                                  "E13ABetn01s02r4")], 
                                                na.rm = TRUE)
e13a_bet_n_01_average$E13ABetn01s03 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01s03r1",
                                                                  "E13ABetn01s03r2", 
                                                                  "E13ABetn01s03r3",
                                                                  "E13ABetn01s03r4")], 
                                                na.rm = TRUE)

e13a_bet_n_01_average$E13ABetn01s04 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01s04r1",
                                                                  "E13ABetn01s04r2",
                                                                  "E13ABetn01s04r3",
                                                                  "E13ABetn01s04r4")],
                                                na.rm = TRUE)

e13a_bet_n_01_average$E13ABetn01s06 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01s06r1",
                                                                  "E13ABetn01s06r2",
                                                                  "E13ABetn01s06r3",
                                                                  "E13ABetn01s06r4")], 
                                                na.rm = TRUE)

#E13ABetn01t
e13a_bet_n_01_average$E13ABetn01t01 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01t01r1",
                                                                  "E13ABetn01t01r2", 
                                                                  "E13ABetn01t01r3",
                                                                  "E13ABetn01t01r4")], 
                                                na.rm = TRUE)

e13a_bet_n_01_average$E13ABetn01t02 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01t02r1",
                                                                  "E13ABetn01t02r2", 
                                                                  "E13ABetn01t02r3",
                                                                  "E13ABetn01t02r4")], 
                                                na.rm = TRUE)
e13a_bet_n_01_average$E13ABetn01t03 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01t03r1",
                                                                  "E13ABetn01t03r2", 
                                                                  "E13ABetn01t03r3",
                                                                  "E13ABetn01t03r4")], 
                                                na.rm = TRUE)

e13a_bet_n_01_average$E13ABetn01t04 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01t04r1",
                                                                  "E13ABetn01t04r2",
                                                                  "E13ABetn01t04r3",
                                                                  "E13ABetn01t04r4")],
                                                na.rm = TRUE)
e13a_bet_n_01_average$E13ABetn01t05 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01t05r1",
                                                                  "E13ABetn01t05r2", 
                                                                  "E13ABetn01t05r3",
                                                                  "E13ABetn01t05r4")], 
                                                na.rm = TRUE)
e13a_bet_n_01_average$E13ABetn01t06 <- rowMeans(e13a_bet_n_01[, c("E13ABetn01t06r1",
                                                                  "E13ABetn01t06r2",
                                                                  "E13ABetn01t06r3",
                                                                  "E13ABetn01t06r4")], 
                                                na.rm = TRUE)




# Replace "NaN" with NA in each column
e13a_bet_n_01_average <- lapply(e13a_bet_n_01_average, function(x) {
  x[is.nan(x)] <- NA
  return(x)
})

# Convert the result back to a data frame
e13a_bet_n_01_average <- as.data.frame(e13a_bet_n_01_average)

# Replace "NaN" with NA for RowNames column
rownames(e13a_bet_n_01_average) <- e13a_bet_n_01_average$RowNames
e13a_bet_n_01_average <- e13a_bet_n_01_average[, -1]
e13a_bet_n_01_average <- round(e13a_bet_n_01_average, digits = 3) #round to 3 digits


View(e13a_bet_n_01_average)

write.rwl(e13a_bet_n_01_average, "data/ring_data/aligned/e13a/e13c.bet.n/e13a_bet_n_01_average.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#analysis####
##Statistics####
e13a_bet_n_01_average_stats <- rwl.stats(e13a_bet_n_01_average) #summary and stats
print(e13a_bet_n_01_average_stats)

e13a_bet_n_01_average_ms <- sens2(e13a_bet_n_01_average) #calculates the mean sensitivity
print(e13a_bet_n_01_average_ms)

e13a_bet_n_01_average_report <- rwl.report(e13a_bet_n_01_average)  #report on rwl
print(e13a_bet_n_01_average_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13a_bet_n_01_average) #creates a segment plot
title(main = "e13abetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13a_bet_n_01_average, zfac=0.009,) #creates a spaghetti plot
title(main = "e13abetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#filter data for 2000s

e13a_bet_n_01_average_filtered <- subset(e13a_bet_n_01_average, rownames(e13a_bet_n_01_average) > 1999)
spag.plot(e13a_bet_n_01_average_filtered, zfac=0.01, cex = 0.3) #creates a spaghetti plot
title(main = "e13abetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title


e13a_bet_n_01_ave_short <- e13a_bet_n_01_average

new_colnames <- sub("^E13ABetn01", "", colnames(e13a_bet_n_01_ave_short))

# Assign the new column names to the data frame
colnames(e13a_bet_n_01_ave_short) <- new_colnames

head(e13a_bet_n_01_ave_short)
colnames(e13a_bet_n_01_ave_short)

spag.plot(e13a_bet_n_01_ave_short, zfac=0.01,) #creates a spaghetti plot
title(main = "e13abetn01 (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title


#filter data for 2000s
e13a_bet_n_01_average_short_filtered <- subset(e13a_bet_n_01_ave_short, rownames(e13a_bet_n_01_ave_short) > 1999)
spag.plot(e13a_bet_n_01_average_short_filtered, zfac=0.01, cex = 0.3) #creates a spaghetti plot
title(main = "e13abetn01 (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
