#loading packages
library(dplR)

#load rwl files
e13v_bet_n_02_O <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02/e13v_bet_n_02_o.rwl")
e13v_bet_n_02_P <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02/e13v_bet_n_02_p.rwl")
e13v_bet_n_02_Q <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02/e13v_bet_n_02_q.rwl")
e13v_bet_n_02_R <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02/e13v_bet_n_02_r.rwl")
e13v_bet_n_02_S <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02/e13v_bet_n_02_s.rwl")
e13v_bet_n_02_T <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02/e13v_bet_n_02_t.rwl")
e13v_bet_n_02_U <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02/e13v_bet_n_02_u.rwl")
e13v_bet_n_02_V <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02/e13v_bet_n_02_v.rwl")

# merge the data
# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13v_bet_n_02_O$row_names <- rownames(e13v_bet_n_02_O)
e13v_bet_n_02_P$row_names <- rownames(e13v_bet_n_02_P)
e13v_bet_n_02_Q$row_names <- rownames(e13v_bet_n_02_Q)
e13v_bet_n_02_R$row_names <- rownames(e13v_bet_n_02_R)
e13v_bet_n_02_S$row_names <- rownames(e13v_bet_n_02_S)
e13v_bet_n_02_T$row_names <- rownames(e13v_bet_n_02_T)
e13v_bet_n_02_U$row_names <- rownames(e13v_bet_n_02_U)
e13v_bet_n_02_V$row_names <- rownames(e13v_bet_n_02_V)

# Merge the data frames using Reduce and merge
e13v_bet_n_02 <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                        list(e13v_bet_n_02_O,
                             e13v_bet_n_02_P,
                             e13v_bet_n_02_Q,
                             e13v_bet_n_02_R, 
                             e13v_bet_n_02_S, 
                             e13v_bet_n_02_T,
                             e13v_bet_n_02_U,
                             e13v_bet_n_02_V)
                        )

# Set row names and remove the extra column
rownames(e13v_bet_n_02) <- e13v_bet_n_02[[common_column]]
e13v_bet_n_02[[common_column]] <- NULL

colnames(e13v_bet_n_02) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13v_bet_n_02 <- e13v_bet_n_02 %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13v_bet_n_02)
colnames(e13v_bet_n_02)
head(e13v_bet_n_02)

write.rwl(e13v_bet_n_02, "data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#reduce the names for easy view
e13v_bet_n_02_short <- e13v_bet_n_02
colnames(e13v_bet_n_02_short)
new_colnames <- sub("^E13VBetn02", "", colnames(e13v_bet_n_02_short))

# Assign the new column names to the data frame
colnames(e13v_bet_n_02_short) <- new_colnames

head(e13v_bet_n_02_short)
colnames(e13v_bet_n_02_short)

if (ncol(e13v_bet_n_02) == ncol(e13v_bet_n_02_short)) { #just check if both have the same number of columns  
  print("Both data frames have the same number of columns.")
} else {
  print("The number of columns in the data frames are different.")
}

#Data Analysis####
##Statistics####
e13v_bet_n_02_stats <- rwl.stats(e13v_bet_n_02) #summary and stats
print(e13v_bet_n_02_stats)

e13v_bet_n_02_ms <- sens2(e13v_bet_n_02) #calculates the mean sensitivity
print(e13v_bet_n_02_ms)

e13v_bet_n_02_report <- rwl.report(e13v_bet_n_02)  #report on rwl
print(e13v_bet_n_02_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13v_bet_n_02_short) #creates a segment plot
title(main = "E13VBetn02", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13v_bet_n_02_short, zfac=0.02, cex = 0.3) #creates a spaghetti plot
title(main = "E13VBetn02", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#average####
#create a Df with the average of each section for e13v_bet_n_02
# Create a new DataFrame with the same row names as the original DataFrame
e13v_bet_n_02_average <- data.frame(RowNames = rownames(e13v_bet_n_02))

# Calculate the average for each set of columns and add them to the new DataFrame
colnames(e13v_bet_n_02)

#E13VBetn02o
e13v_bet_n_02_average$E13VBetn02o01 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02o01r1",
                                                                  "E13VBetn02o01r2", 
                                                                  "E13VBetn02o01r3",
                                                                  "E13VBetn02o01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02o02 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02o02r1",
                                                                  "E13VBetn02o02r2", 
                                                                  "E13VBetn02o02r3",
                                                                  "E13VBetn02o02r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02o03 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02o03r1",
                                                                  "E13VBetn02o03r2", 
                                                                  "E13VBetn02o03r3",
                                                                  "E13VBetn02o03r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02o04 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02o04r1",
                                                                  "E13VBetn02o04r2", 
                                                                  "E13VBetn02o04r3",
                                                                  "E13VBetn02o04r4")], 
                                                na.rm = TRUE)
e13v_bet_n_02_average$E13VBetn02o05 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02o05r1",
                                                                  "E13VBetn02o05r2", 
                                                                  "E13VBetn02o05r3",
                                                                  "E13VBetn02o05r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02o06 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02o06r1",
                                                                  "E13VBetn02o06r2", 
                                                                  "E13VBetn02o06r3",
                                                                  "E13VBetn02o06r4")], 
                                                na.rm = TRUE)

#E13VBetn02p
e13v_bet_n_02_average$E13VBetn02p01 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02p01r1",
                                                                  "E13VBetn02p01r2", 
                                                                  "E13VBetn02p01r3",
                                                                  "E13VBetn02p01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02p02 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02p02r1",
                                                                  "E13VBetn02p02r2", 
                                                                  "E13VBetn02p02r3",
                                                                  "E13VBetn02p02r4")], 
                                                na.rm = TRUE)
e13v_bet_n_02_average$E13VBetn02p03 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02p03r1",
                                                                  "E13VBetn02p03r2", 
                                                                  "E13VBetn02p03r3",
                                                                  "E13VBetn02p03r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02p04 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02p04r1",
                                                                  "E13VBetn02p04r2", 
                                                                  "E13VBetn02p04r3",
                                                                  "E13VBetn02p04r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02p06 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02p06r1",
                                                                  "E13VBetn02p06r2", 
                                                                  "E13VBetn02p06r3",
                                                                  "E13VBetn02p06r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02p07 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02p07r1",
                                                                  "E13VBetn02p07r2", 
                                                                  "E13VBetn02p07r3",
                                                                  "E13VBetn02p07r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02p08 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02p08r1",
                                                                  "E13VBetn02p08r2", 
                                                                  "E13VBetn02p08r3",
                                                                  "E13VBetn02p08r4")], 
                                                na.rm = TRUE)
#E13VBetn02q
e13v_bet_n_02_average$E13VBetn02q01 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02q01r1",
                                                                  "E13VBetn02q01r2", 
                                                                  "E13VBetn02q01r3",
                                                                  "E13VBetn02q01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02q02 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02q02r1",
                                                                  "E13VBetn02q02r2", 
                                                                  "E13VBetn02q02r3",
                                                                  "E13VBetn02q02r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02q03 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02q03r1",
                                                                  "E13VBetn02q03r2", 
                                                                  "E13VBetn02q03r3",
                                                                  "E13VBetn02q03r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02q04 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02q04r1",
                                                                  "E13VBetn02q04r2", 
                                                                  "E13VBetn02q04r3",
                                                                  "E13VBetn02q04r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02q05 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02q05r1",
                                                                  "E13VBetn02q05r2", 
                                                                  "E13VBetn02q05r3",
                                                                  "E13VBetn02q05r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02q06 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02q06r1",
                                                                  "E13VBetn02q06r2", 
                                                                  "E13VBetn02q06r3",
                                                                  "E13VBetn02q06r4")], 
                                                na.rm = TRUE)

#E13VBetn02r
e13v_bet_n_02_average$E13VBetn02r01 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02r01r1",
                                                                  "E13VBetn02r01r2", 
                                                                  "E13VBetn02r01r3",
                                                                  "E13VBetn02r01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02r03 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02r03r1",
                                                                  "E13VBetn02r03r2", 
                                                                  "E13VBetn02r03r3",
                                                                  "E13VBetn02r03r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02r04 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02r04r1",
                                                                  "E13VBetn02r04r2", 
                                                                  "E13VBetn02r04r3",
                                                                  "E13VBetn02r04r4")], 
                                                na.rm = TRUE)

#E13VBetn02s

e13v_bet_n_02_average$E13VBetn02s01 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02s01r1",
                                                                  "E13VBetn02s01r2", 
                                                                  "E13VBetn02s01r3",
                                                                  "E13VBetn02s01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02s02 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02s02r1",
                                                                  "E13VBetn02s02r2", 
                                                                  "E13VBetn02s02r3",
                                                                  "E13VBetn02s02r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02s03 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02s03r1",
                                                                  "E13VBetn02s03r2", 
                                                                  "E13VBetn02s03r3",
                                                                  "E13VBetn02s03r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02s04 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02s04r1",
                                                                  "E13VBetn02s04r2",
                                                                  "E13VBetn02s04r3",
                                                                  "E13VBetn02s04r4")],
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02s05 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02s05r1",
                                                                  "E13VBetn02s05r2", 
                                                                  "E13VBetn02s05r3",
                                                                  "E13VBetn02s05r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02s06 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02s06r1",
                                                                  "E13VBetn02s06r2", 
                                                                  "E13VBetn02s06r3",
                                                                  "E13VBetn02s06r4")], 
                                                na.rm = TRUE)

#E13VBetn02t
e13v_bet_n_02_average$E13VBetn02t01 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02t01r1",
                                                                  "E13VBetn02t01r2", 
                                                                  "E13VBetn02t01r3",
                                                                  "E13VBetn02t01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02t02 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02t02r1",
                                                                  "E13VBetn02t02r2", 
                                                                  "E13VBetn02t02r3",
                                                                  "E13VBetn02t02r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02t03 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02t03r1",
                                                                  "E13VBetn02t03r2", 
                                                                  "E13VBetn02t03r3",
                                                                  "E13VBetn02t03r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02t04 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02t04r1",
                                                                  "E13VBetn02t04r2", 
                                                                  "E13VBetn02t04r3",
                                                                  "E13VBetn02t04r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02t05 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02t05r1",
                                                                  "E13VBetn02t05r2", 
                                                                  "E13VBetn02t05r3",
                                                                  "E13VBetn02t05r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02t06 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02t06r1",
                                                                  "E13VBetn02t06r2", 
                                                                  "E13VBetn02t06r3",
                                                                  "E13VBetn02t06r4")], 
                                                na.rm = TRUE)

#E13VBetn02u
e13v_bet_n_02_average$E13VBetn02u01 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02u01r1",
                                                                  "E13VBetn02u01r2", 
                                                                  "E13VBetn02u01r3",
                                                                  "E13VBetn02u01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02u02 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02u02r1",
                                                                  "E13VBetn02u02r2", 
                                                                  "E13VBetn02u02r3",
                                                                  "E13VBetn02u02r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02u03 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02u03r1",
                                                                  "E13VBetn02u03r2", 
                                                                  "E13VBetn02u03r3",
                                                                  "E13VBetn02u03r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02u04 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02u04r1",
                                                                  "E13VBetn02u04r2", 
                                                                  "E13VBetn02u04r3",
                                                                  "E13VBetn02u04r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02u05 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02u05r1",
                                                                  "E13VBetn02u05r2", 
                                                                  "E13VBetn02u05r3",
                                                                  "E13VBetn02u05r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02u06 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02u06r1",
                                                                  "E13VBetn02u06r2", 
                                                                  "E13VBetn02u06r3",
                                                                  "E13VBetn02u06r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02u07 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02u07r1",
                                                                  "E13VBetn02u07r2", 
                                                                  "E13VBetn02u07r3",
                                                                  "E13VBetn02u07r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02u08 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02u08r1",
                                                                  "E13VBetn02u08r2", 
                                                                  "E13VBetn02u08r3",
                                                                  "E13VBetn02u08r4")], 
                                                na.rm = TRUE)

#E13VBetn02v
e13v_bet_n_02_average$E13VBetn02v01 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02v01r1",
                                                                  "E13VBetn02v01r2", 
                                                                  "E13VBetn02v01r3",
                                                                  "E13VBetn02v01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02v02 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02v02r1",
                                                                  "E13VBetn02v02r2", 
                                                                  "E13VBetn02v02r3",
                                                                  "E13VBetn02v02r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02v03 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02v03r1",
                                                                  "E13VBetn02v03r2", 
                                                                  "E13VBetn02v03r3",
                                                                  "E13VBetn02v03r4")], 
                                                na.rm = TRUE)

e13v_bet_n_02_average$E13VBetn02v04 <- rowMeans(e13v_bet_n_02[, c("E13VBetn02v04r1",
                                                                  "E13VBetn02v04r2", 
                                                                  "E13VBetn02v04r3",
                                                                  "E13VBetn02v04r4")], 
                                                na.rm = TRUE)
# Replace "NaN" with NA in each column
e13v_bet_n_02_average <- lapply(e13v_bet_n_02_average, function(x) {
  x[is.nan(x)] <- NA
  return(x)
})

# Convert the result back to a data frame
e13v_bet_n_02_average <- as.data.frame(e13v_bet_n_02_average)

# Replace "NaN" with NA for RowNames column
rownames(e13v_bet_n_02_average) <- e13v_bet_n_02_average$RowNames
e13v_bet_n_02_average <- e13v_bet_n_02_average[, -1]
e13v_bet_n_02_average <- round(e13v_bet_n_02_average, digits = 3) #round to 3 digits


View(e13v_bet_n_02_average)

write.rwl(e13v_bet_n_02_average, "data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02_average.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#analysis####
##Statistics####
e13v_bet_n_02_average_stats <- rwl.stats(e13v_bet_n_02_average) #summary and stats
print(e13v_bet_n_02_average_stats)

e13v_bet_n_02_average_ms <- sens2(e13v_bet_n_02_average) #calculates the mean sensitivity
print(e13v_bet_n_02_average_ms)

e13v_bet_n_02_average_report <- rwl.report(e13v_bet_n_02_average)  #report on rwl
print(e13v_bet_n_02_average_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13v_bet_n_02_average) #creates a segment plot
title(main = "E13VBetn02", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13v_bet_n_02_average, zfac=0.005,) #creates a spaghetti plot
title(main = "E13VBetn02", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#shorten the names
e13v_bet_n_02_ave_short <- e13v_bet_n_02_average

new_colnames <- sub("^E13VBetn02", "", colnames(e13v_bet_n_02_ave_short))

# Assign the new column names to the data frame
colnames(e13v_bet_n_02_ave_short) <- new_colnames

head(e13v_bet_n_02_ave_short)
colnames(e13v_bet_n_02_ave_short)

spag.plot(e13v_bet_n_02_ave_short, zfac=0.007,) #creates a spaghetti plot
title(main = "E13VBetn02 (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
