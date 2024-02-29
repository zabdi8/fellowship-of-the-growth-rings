#loading packages
library(dplR)

#load rwl files
e13d_bet_n_03_O <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03/e13d_bet_n_03_o.rwl")
e13d_bet_n_03_R <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03/e13d_bet_n_03_r.rwl")
e13d_bet_n_03_S <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03/e13d_bet_n_03_s.rwl")
e13d_bet_n_03_T <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03/e13d_bet_n_03_t.rwl")
e13d_bet_n_03_U <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03/e13d_bet_n_03_u.rwl")
e13d_bet_n_03_V <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03/e13d_bet_n_03_v.rwl")

# merge the data

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13d_bet_n_03_O$row_names <- rownames(e13d_bet_n_03_O)
e13d_bet_n_03_R$row_names <- rownames(e13d_bet_n_03_R)
e13d_bet_n_03_S$row_names <- rownames(e13d_bet_n_03_S)
e13d_bet_n_03_T$row_names <- rownames(e13d_bet_n_03_T)
e13d_bet_n_03_U$row_names <- rownames(e13d_bet_n_03_U)
e13d_bet_n_03_V$row_names <- rownames(e13d_bet_n_03_V)

# Merge the data frames using Reduce and merge
e13d_bet_n_03 <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                        list(e13d_bet_n_03_O,
                             e13d_bet_n_03_R, 
                             e13d_bet_n_03_S, 
                             e13d_bet_n_03_T,
                             e13d_bet_n_03_U,
                             e13d_bet_n_03_V)
)

# Set row names and remove the extra column
rownames(e13d_bet_n_03) <- e13d_bet_n_03[[common_column]]
e13d_bet_n_03[[common_column]] <- NULL

colnames(e13d_bet_n_03) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13d_bet_n_03 <- e13d_bet_n_03 %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13d_bet_n_03)
colnames(e13d_bet_n_03)
head(e13d_bet_n_03)

write.rwl(e13d_bet_n_03, "data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#reduce the names for easy view
e13d_bet_n_03_short <- e13d_bet_n_03
colnames(e13d_bet_n_03_short)
new_colnames <- sub("^E13DBetn03", "", colnames(e13d_bet_n_03_short))

# Assign the new column names to the data frame
colnames(e13d_bet_n_03_short) <- new_colnames

head(e13d_bet_n_03_short)
colnames(e13d_bet_n_03_short)

if (ncol(e13d_bet_n_03) == ncol(e13d_bet_n_03_short)) { #just check if both have the same number of columns  
  print("Both data frames have the same number of columns.")
} else {
  print("The number of columns in the data frames are different.")
}

#Data Analysis####
##Statistics####
e13d_bet_n_03_stats <- rwl.stats(e13d_bet_n_03) #summary and stats
print(e13d_bet_n_03_stats)

e13d_bet_n_03_ms <- sens2(e13d_bet_n_03) #calculates the mean sensitivity
print(e13d_bet_n_03_ms)

e13d_bet_n_03_report <- rwl.report(e13d_bet_n_03)  #report on rwl
print(e13d_bet_n_03_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13d_bet_n_03_short) #creates a segment plot
title(main = "E13DBetn03", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13d_bet_n_03_short, zfac=0.02, cex = 0.3) #creates a spaghetti plot
title(main = "E13DBetn03", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#average####
#create a Df with the average of each section for e13d_bet_n_03
# Create a new DataFrame with the same row names as the original DataFrame
e13d_bet_n_03_average <- data.frame(RowNames = rownames(e13d_bet_n_03))

# Calculate the average for each set of columns and add them to the new DataFrame
colnames(e13d_bet_n_03)

#E13DBetn03o
e13d_bet_n_03_average$E13DBetn03o01 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03o01r1",
                                                                  "E13DBetn03o01r2", 
                                                                  "E13DBetn03o01r3",
                                                                  "E13DBetn03o01r4")], 
                                                na.rm = TRUE)
e13d_bet_n_03_average$E13DBetn03o02 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03o02r1",
                                                                  "E13DBetn03o02r2", 
                                                                  "E13DBetn03o02r3",
                                                                  "E13DBetn03o02r4")], 
                                                na.rm = TRUE)
e13d_bet_n_03_average$E13DBetn03o03 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03o03r1",
                                                                  "E13DBetn03o03r2", 
                                                                  "E13DBetn03o03r3",
                                                                  "E13DBetn03o03r4")], 
                                                na.rm = TRUE)
e13d_bet_n_03_average$E13DBetn03o04 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03o04r1",
                                                                  "E13DBetn03o04r2", 
                                                                  "E13DBetn03o04r3",
                                                                  "E13DBetn03o04r4")], 
                                                na.rm = TRUE)

#E13DBetn03r
e13d_bet_n_03_average$E13DBetn03r01 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03r01r1",
                                                                  "E13DBetn03r01r2", 
                                                                  "E13DBetn03r01r3",
                                                                  "E13DBetn03r01r4")], 
                                                na.rm = TRUE)

e13d_bet_n_03_average$E13DBetn03r05 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03r05r1",
                                                                  "E13DBetn03r05r2", 
                                                                  "E13DBetn03r05r3",
                                                                  "E13DBetn03r05r4")], 
                                                na.rm = TRUE)
e13d_bet_n_03_average$E13DBetn03r09 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03r09r1",
                                                                  "E13DBetn03r09r2", 
                                                                  "E13DBetn03r09r3",
                                                                  "E13DBetn03r09r4")], 
                                                na.rm = TRUE)

#E13DBetn03s
e13d_bet_n_03_average$E13DBetn03s01 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03s01r1",
                                                                  "E13DBetn03s01r2", 
                                                                  "E13DBetn03s01r3",
                                                                  "E13DBetn03s01r4")], 
                                                na.rm = TRUE)

e13d_bet_n_03_average$E13DBetn03s04 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03s04r1",
                                                                  "E13DBetn03s04r2",
                                                                  "E13DBetn03s04r3",
                                                                  "E13DBetn03s04r4")],
                                                na.rm = TRUE)

e13d_bet_n_03_average$E13DBetn03s07 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03s07r1",
                                                                  "E13DBetn03s07r2", 
                                                                  "E13DBetn03s07r3",
                                                                  "E13DBetn03s07r4")], 
                                                na.rm = TRUE)

#E13DBetn03t
e13d_bet_n_03_average$E13DBetn03t01 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03t01r1",
                                                                  "E13DBetn03t01r2", 
                                                                  "E13DBetn03t01r3",
                                                                  "E13DBetn03t01r4")], 
                                                na.rm = TRUE)

e13d_bet_n_03_average$E13DBetn03t03 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03t03r1",
                                                                  "E13DBetn03t03r2", 
                                                                  "E13DBetn03t03r3",
                                                                  "E13DBetn03t03r4")], 
                                                na.rm = TRUE)

e13d_bet_n_03_average$E13DBetn03t05 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03t05r1",
                                                                  "E13DBetn03t05r2", 
                                                                  "E13DBetn03t05r3",
                                                                  "E13DBetn03t05r4")], 
                                                na.rm = TRUE)

#E13DBetn03u
e13d_bet_n_03_average$E13DBetn03u01 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03u01r1",
                                                                  "E13DBetn03u01r2", 
                                                                  "E13DBetn03u01r3",
                                                                  "E13DBetn03u01r4")], 
                                                na.rm = TRUE)

e13d_bet_n_03_average$E13DBetn03u02 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03u02r1",
                                                                  "E13DBetn03u02r2", 
                                                                  "E13DBetn03u02r3",
                                                                  "E13DBetn03u02r4")], 
                                                na.rm = TRUE)

e13d_bet_n_03_average$E13DBetn03u03 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03u03r1",
                                                                  "E13DBetn03u03r2", 
                                                                  "E13DBetn03u03r3",
                                                                  "E13DBetn03u03r4")], 
                                                na.rm = TRUE)
e13d_bet_n_03_average$E13DBetn03u04 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03u04r1",
                                                                  "E13DBetn03u04r2", 
                                                                  "E13DBetn03u04r3",
                                                                  "E13DBetn03u04r4")], 
                                                na.rm = TRUE)
e13d_bet_n_03_average$E13DBetn03u05 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03u05r1",
                                                                  "E13DBetn03u05r2", 
                                                                  "E13DBetn03u05r3",
                                                                  "E13DBetn03u05r4")], 
                                                na.rm = TRUE)

#E13DBetn03v
e13d_bet_n_03_average$E13DBetn03v01 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03v01r1",
                                                                  "E13DBetn03v01r2", 
                                                                  "E13DBetn03v01r3",
                                                                  "E13DBetn03v01r4")], 
                                                na.rm = TRUE)
e13d_bet_n_03_average$E13DBetn03v02 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03v02r1",
                                                                  "E13DBetn03v02r2", 
                                                                  "E13DBetn03v02r3",
                                                                  "E13DBetn03v02r4")], 
                                                na.rm = TRUE)

e13d_bet_n_03_average$E13DBetn03v03 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03v03r1",
                                                                  "E13DBetn03v03r2", 
                                                                  "E13DBetn03v03r3",
                                                                  "E13DBetn03v03r4")], 
                                                na.rm = TRUE)

e13d_bet_n_03_average$E13DBetn03v04 <- rowMeans(e13d_bet_n_03[, c("E13DBetn03v04r1",
                                                                  "E13DBetn03v04r2", 
                                                                  "E13DBetn03v04r3",
                                                                  "E13DBetn03v04r4")], 
                                                na.rm = TRUE)

# Replace "NaN" with NA in each column
e13d_bet_n_03_average <- lapply(e13d_bet_n_03_average, function(x) {
  x[is.nan(x)] <- NA
  return(x)
})

# Convert the result back to a data frame
e13d_bet_n_03_average <- as.data.frame(e13d_bet_n_03_average)

# Replace "NaN" with NA for RowNames column
rownames(e13d_bet_n_03_average) <- e13d_bet_n_03_average$RowNames
e13d_bet_n_03_average <- e13d_bet_n_03_average[, -1]
e13d_bet_n_03_average <- round(e13d_bet_n_03_average, digits = 3) #round to 3 digits


View(e13d_bet_n_03_average)

write.rwl(e13d_bet_n_03_average, "data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03_average.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#analysis####
##Statistics####
e13d_bet_n_03_average_stats <- rwl.stats(e13d_bet_n_03_average) #summary and stats
print(e13d_bet_n_03_average_stats)

e13d_bet_n_03_average_ms <- sens2(e13d_bet_n_03_average) #calculates the mean sensitivity
print(e13d_bet_n_03_average_ms)

e13d_bet_n_03_average_report <- rwl.report(e13d_bet_n_03_average)  #report on rwl
print(e13d_bet_n_03_average_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13d_bet_n_03_average) #creates a segment plot
title(main = "E13DBetn03", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13d_bet_n_03_average, zfac=0.005,) #creates a spaghetti plot
title(main = "E13DBetn03", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#shorten the names
e13d_bet_n_03_ave_short <- e13d_bet_n_03_average

new_colnames <- sub("^E13DBetn03", "", colnames(e13d_bet_n_03_ave_short))

# Assign the new column names to the data frame
colnames(e13d_bet_n_03_ave_short) <- new_colnames

head(e13d_bet_n_03_ave_short)
colnames(e13d_bet_n_03_ave_short)

spag.plot(e13d_bet_n_03_ave_short, zfac=0.005,) #creates a spaghetti plot
title(main = "E13DBetn03 (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

