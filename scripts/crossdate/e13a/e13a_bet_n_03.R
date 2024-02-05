#loading packages
library(dplR)
library(dplyr)
library(ggplot2)
library(ggvis)
library(gridExtra)  ## required to arrange ggplot2 plots in a grid


#load rwl files

e13a_bet_n_03_O <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03/e13a_bet_n_03_o.rwl")
e13a_bet_n_03_P <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03/e13a_bet_n_03_p.rwl")
e13a_bet_n_03_R <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03/e13a_bet_n_03_r.rwl")
e13a_bet_n_03_S <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03/e13a_bet_n_03_s.rwl")
e13a_bet_n_03_T <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03/e13a_bet_n_03_t.rwl")
e13a_bet_n_03_U <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03/e13a_bet_n_03_u.rwl")
e13a_bet_n_03_V <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03/e13a_bet_n_03_v.rwl")




# merge the data


# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13a_bet_n_03_O$row_names <- rownames(e13a_bet_n_03_O)
e13a_bet_n_03_P$row_names <- rownames(e13a_bet_n_03_P)
e13a_bet_n_03_R$row_names <- rownames(e13a_bet_n_03_R)
e13a_bet_n_03_S$row_names <- rownames(e13a_bet_n_03_S)
e13a_bet_n_03_T$row_names <- rownames(e13a_bet_n_03_T)
e13a_bet_n_03_U$row_names <- rownames(e13a_bet_n_03_U)
e13a_bet_n_03_V$row_names <- rownames(e13a_bet_n_03_V)

# Merge the data frames using Reduce and merge
e13a_bet_n_03 <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                        list(e13a_bet_n_03_O,
                             e13a_bet_n_03_P,
                             e13a_bet_n_03_R, 
                             e13a_bet_n_03_S, 
                             e13a_bet_n_03_T,
                             e13a_bet_n_03_U,
                             e13a_bet_n_03_V)
                        )

# Set row names and remove the extra column
rownames(e13a_bet_n_03) <- e13a_bet_n_03[[common_column]]
e13a_bet_n_03[[common_column]] <- NULL

colnames(e13a_bet_n_03) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13a_bet_n_03 <- e13a_bet_n_03 %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13a_bet_n_03)
colnames(e13a_bet_n_03)
head(e13a_bet_n_03)

write.rwl(e13a_bet_n_03, "data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#reduce the names for easy view
e13a_bet_n_03_short <- e13a_bet_n_03

new_colnames <- sub("^E13ABetn03", "", colnames(e13a_bet_n_03_short))

# Assign the new column names to the data frame
colnames(e13a_bet_n_03_short) <- new_colnames

head(e13a_bet_n_03_short)
colnames(e13a_bet_n_03_short)

#Data Analysis####
##Statistics####
e13a_bet_n_03_stats <- rwl.stats(e13a_bet_n_03) #summary and stats
print(e13a_bet_n_03_stats)

e13a_bet_n_03_ms <- sens2(e13a_bet_n_03) #calculates the mean sensitivity
print(e13a_bet_n_03_ms)

e13a_bet_n_03_report <- rwl.report(e13a_bet_n_03)  #report on rwl
print(e13a_bet_n_03_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13a_bet_n_03_short) #creates a segment plot
title(main = "E13ABetn03", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13a_bet_n_03_short, zfac=0.02, cex = 1000) #creates a spaghetti plot
title(main = "E13ABetn03", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#average####
#create a Df with the average of each section for e13a_bet_n_01
# Create a new DataFrame with the same row names as the original DataFrame
e13a_bet_n_03_average <- data.frame(RowNames = rownames(e13a_bet_n_03))

# Calculate the average for each set of columns and add them to the new DataFrame
colnames(e13a_bet_n_03)


#E13ABetn03o
e13a_bet_n_03_average$E13ABetn03o01 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03o01r1",
                                                                  "E13ABetn03o01r2", 
                                                                  "E13ABetn03o01r3",
                                                                  "E13ABetn03o01r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03o02 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03o02r1",
                                                                  "E13ABetn03o02r2", 
                                                                  "E13ABetn03o02r3",
                                                                  "E13ABetn03o02r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03o03 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03o03r1",
                                                                  "E13ABetn03o03r2", 
                                                                  "E13ABetn03o03r3",
                                                                  "E13ABetn03o03r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03o04 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03o04r1",
                                                                  "E13ABetn03o04r2", 
                                                                  "E13ABetn03o04r3",
                                                                  "E13ABetn03o04r4")], 
                                                na.rm = TRUE)
#E13ABetn03p
e13a_bet_n_03_average$E13ABetn03p01 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03p01r1",
                                                                  "E13ABetn03p01r2", 
                                                                  "E13ABetn03p01r3",
                                                                  "E13ABetn03p01r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03p02 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03p02r1",
                                                                  "E13ABetn03p02r2", 
                                                                  "E13ABetn03p02r3",
                                                                  "E13ABetn03p02r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03p03 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03p03r1",
                                                                  "E13ABetn03p03r2", 
                                                                  "E13ABetn03p03r3",
                                                                  "E13ABetn03p03r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03p04 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03p04r1",
                                                                  "E13ABetn03p04r2", 
                                                                  "E13ABetn03p04r3",
                                                                  "E13ABetn03p04r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03p05 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03p05r1",
                                                                  "E13ABetn03p05r2", 
                                                                  "E13ABetn03p05r3",
                                                                  "E13ABetn03p05r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03p06 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03p06r1",
                                                                  "E13ABetn03p06r2", 
                                                                  "E13ABetn03p06r3",
                                                                  "E13ABetn03p06r4")], 
                                                na.rm = TRUE)
#E13ABetn03r
e13a_bet_n_03_average$E13ABetn03r01 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03r01r1",
                                                                  "E13ABetn03r01r2", 
                                                                  "E13ABetn03r01r3",
                                                                  "E13ABetn03r01r4")], 
                                                na.rm = TRUE)

e13a_bet_n_03_average$E13ABetn03r02 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03r02r1",
                                                                  "E13ABetn03r02r2",
                                                                  "E13ABetn03r02r3",
                                                                  "E13ABetn03r02r4")],
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03r03 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03r03r1",
                                                                  "E13ABetn03r03r2", 
                                                                  "E13ABetn03r03r3",
                                                                  "E13ABetn03r03r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03r04 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03r04r1",
                                                                  "E13ABetn03r04r2",
                                                                  "E13ABetn03r04r3",
                                                                  "E13ABetn03r04r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03r05 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03r05r1",
                                                                  "E13ABetn03r05r2", 
                                                                  "E13ABetn03r05r3",
                                                                  "E13ABetn03r05r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03r06 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03r06r1",
                                                                  "E13ABetn03r06r2", 
                                                                  "E13ABetn03r06r3",
                                                                  "E13ABetn03r06r4")], 
                                                na.rm = TRUE)
#E13ABetn03s

e13a_bet_n_03_average$E13ABetn03s01 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03s01r1",
                                                                  "E13ABetn03s01r2", 
                                                                  "E13ABetn03s01r3",
                                                                  "E13ABetn03s01r4")], 
                                                na.rm = TRUE)

e13a_bet_n_03_average$E13ABetn03s02 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03s02r1",
                                                                  "E13ABetn03s02r2", 
                                                                  "E13ABetn03s02r3",
                                                                  "E13ABetn03s02r4")], 
                                                na.rm = TRUE)

e13a_bet_n_03_average$E13ABetn03s03 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03s03r1",
                                                                  "E13ABetn03s03r2", 
                                                                  "E13ABetn03s03r3",
                                                                  "E13ABetn03s03r4")], 
                                                na.rm = TRUE)

e13a_bet_n_03_average$E13ABetn03s04 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03s04r1",
                                                                  "E13ABetn03s04r2",
                                                                  "E13ABetn03s04r3",
                                                                  "E13ABetn03s04r4")],
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03s05 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03s05r1",
                                                                  "E13ABetn03s05r2", 
                                                                  "E13ABetn03s05r3",
                                                                  "E13ABetn03s05r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03s06 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03s06r1",
                                                                  "E13ABetn03s06r2",
                                                                  "E13ABetn03s06r3",
                                                                  "E13ABetn03s06r4")], 
                                                na.rm = TRUE)


#E13ABetn03t
e13a_bet_n_03_average$E13ABetn03t01 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03t01r1",
                                                                  "E13ABetn03t01r2", 
                                                                  "E13ABetn03t01r3",
                                                                  "E13ABetn03t01r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03t02 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03t02r1",
                                                                  "E13ABetn03t02r2", 
                                                                  "E13ABetn03t02r3",
                                                                  "E13ABetn03t02r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03t03 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03t03r1",
                                                                  "E13ABetn03t03r2", 
                                                                  "E13ABetn03t03r3",
                                                                  "E13ABetn03t03r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03t04 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03t04r1",
                                                                  "E13ABetn03t04r2", 
                                                                  "E13ABetn03t04r3",
                                                                  "E13ABetn03t04r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03t05 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03t05r1",
                                                                  "E13ABetn03t05r2",
                                                                  "E13ABetn03t05r3",
                                                                  "E13ABetn03t05r4")],
                                                na.rm = TRUE)

#E13ABetn03u
e13a_bet_n_03_average$E13ABetn03u01 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03u01r1",
                                                                  "E13ABetn03u01r2", 
                                                                  "E13ABetn03u01r3",
                                                                  "E13ABetn03u01r4")], 
                                                na.rm = TRUE)

e13a_bet_n_03_average$E13ABetn03u02 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03u02r1",
                                                                  "E13ABetn03u02r2", 
                                                                  "E13ABetn03u02r3",
                                                                  "E13ABetn03u02r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03u03 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03u03r1",
                                                                  "E13ABetn03u03r2", 
                                                                  "E13ABetn03u03r3",
                                                                  "E13ABetn03u03r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03u04 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03u04r1",
                                                                  "E13ABetn03u04r2", 
                                                                  "E13ABetn03u04r3",
                                                                  "E13ABetn03u04r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03u05 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03u05r1",
                                                                  "E13ABetn03u05r2", 
                                                                  "E13ABetn03u05r3",
                                                                  "E13ABetn03u05r4")], 
                                                na.rm = TRUE)
#E13ABetn03v
e13a_bet_n_03_average$E13ABetn03v01 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03v01r1",
                                                                  "E13ABetn03v01r2", 
                                                                  "E13ABetn03v01r3",
                                                                  "E13ABetn03v01r4")], 
                                                na.rm = TRUE)

e13a_bet_n_03_average$E13ABetn03v02 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03v02r1",
                                                                  "E13ABetn03v02r2",
                                                                  "E13ABetn03v02r3",
                                                                  "E13ABetn03v02r4")],
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03v03 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03v03r1",
                                                                  "E13ABetn03v03r2", 
                                                                  "E13ABetn03v03r3",
                                                                  "E13ABetn03v03r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03v04 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03v04r1",
                                                                  "E13ABetn03v04r2",
                                                                  "E13ABetn03v04r3",
                                                                  "E13ABetn03v04r4")], 
                                                na.rm = TRUE)
e13a_bet_n_03_average$E13ABetn03v05 <- rowMeans(e13a_bet_n_03[, c("E13ABetn03v05r1",
                                                                  "E13ABetn03v05r2", 
                                                                  "E13ABetn03v05r3",
                                                                  "E13ABetn03v05r4")], 
                                                na.rm = TRUE)


# Replace "NaN" with NA in each column
e13a_bet_n_03_average <- lapply(e13a_bet_n_03_average, function(x) {
  x[is.nan(x)] <- NA
  return(x)
})

# Convert the result back to a data frame
e13a_bet_n_03_average <- as.data.frame(e13a_bet_n_03_average)

# Replace "NaN" with NA for RowNames column
rownames(e13a_bet_n_03_average) <- e13a_bet_n_03_average$RowNames
e13a_bet_n_03_average <- e13a_bet_n_03_average[, -1]
e13a_bet_n_03_average <- round(e13a_bet_n_03_average, digits = 3) #round to 3 digits


View(e13a_bet_n_03_average)

write.rwl(e13a_bet_n_03_average, "data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03_average.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#analysis####
##Statistics####
e13a_bet_n_03_average_stats <- rwl.stats(e13a_bet_n_03_average) #summary and stats
print(e13a_bet_n_03_average_stats)

e13a_bet_n_03_average_ms <- sens2(e13a_bet_n_03_average) #calculates the mean sensitivity
print(e13a_bet_n_03_average_ms)

e13a_bet_n_03_average_report <- rwl.report(e13a_bet_n_03_average)  #report on rwl
print(e13a_bet_n_03_average_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13a_bet_n_03_average) #creates a segment plot
title(main = "E13ABetn03", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13a_bet_n_03_average, zfac=0.009,) #creates a spaghetti plot
title(main = "E13ABetn03", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

e13a_bet_n_03_ave_short <- e13a_bet_n_03_average

new_colnames <- sub("^E13ABetn03", "", colnames(e13a_bet_n_03_ave_short))

# Assign the new column names to the data frame
colnames(e13a_bet_n_03_ave_short) <- new_colnames

head(e13a_bet_n_03_ave_short)
colnames(e13a_bet_n_03_ave_short)

spag.plot(e13a_bet_n_03_ave_short, zfac=0.018,) #creates a spaghetti plot
title(main = "E13ABetn03 (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

