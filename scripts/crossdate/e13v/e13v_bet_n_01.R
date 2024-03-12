#loading packages
library(dplR)

#load rwl files
e13v_bet_n_01_O <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01/e13v_bet_n_01_o.rwl")
e13v_bet_n_01_P <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01/e13v_bet_n_01_p.rwl")
e13v_bet_n_01_Q <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01/e13v_bet_n_01_q.rwl")
e13v_bet_n_01_R <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01/e13v_bet_n_01_r.rwl")
e13v_bet_n_01_S <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01/e13v_bet_n_01_s.rwl")
e13v_bet_n_01_T <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01/e13v_bet_n_01_t.rwl")
e13v_bet_n_01_U <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01/e13v_bet_n_01_u.rwl")

# merge the data


# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13v_bet_n_01_O$row_names <- rownames(e13v_bet_n_01_O)
e13v_bet_n_01_P$row_names <- rownames(e13v_bet_n_01_P)
e13v_bet_n_01_Q$row_names <- rownames(e13v_bet_n_01_Q)
e13v_bet_n_01_R$row_names <- rownames(e13v_bet_n_01_R)
e13v_bet_n_01_S$row_names <- rownames(e13v_bet_n_01_S)
e13v_bet_n_01_T$row_names <- rownames(e13v_bet_n_01_T)
e13v_bet_n_01_U$row_names <- rownames(e13v_bet_n_01_U)

# Merge the data frames using Reduce and merge
e13v_bet_n_01 <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                        list(e13v_bet_n_01_O,
                             e13v_bet_n_01_P,
                             e13v_bet_n_01_Q,
                             e13v_bet_n_01_R, 
                             e13v_bet_n_01_S, 
                             e13v_bet_n_01_T,
                             e13v_bet_n_01_U)
                        )

# Set row names and remove the extra column
rownames(e13v_bet_n_01) <- e13v_bet_n_01[[common_column]]
e13v_bet_n_01[[common_column]] <- NULL

colnames(e13v_bet_n_01) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13v_bet_n_01 <- e13v_bet_n_01 %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13v_bet_n_01)
colnames(e13v_bet_n_01)
head(e13v_bet_n_01)

write.rwl(e13v_bet_n_01, "data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#reduce the names for easy view
e13v_bet_n_01_short <- e13v_bet_n_01
colnames(e13v_bet_n_01_short)
new_colnames <- sub("^E13VBetn01", "", colnames(e13v_bet_n_01_short))

# Assign the new column names to the data frame
colnames(e13v_bet_n_01_short) <- new_colnames

head(e13v_bet_n_01_short)
colnames(e13v_bet_n_01_short)

if (ncol(e13v_bet_n_01) == ncol(e13v_bet_n_01_short)) { #just check if both have the same number of columns  
  print("Both data frames have the same number of columns.")
} else {
  print("The number of columns in the data frames are different.")
}

#Data Analysis####
##Statistics####
e13v_bet_n_01_stats <- rwl.stats(e13v_bet_n_01) #summary and stats
print(e13v_bet_n_01_stats)

e13v_bet_n_01_ms <- sens2(e13v_bet_n_01) #calculates the mean sensitivity
print(e13v_bet_n_01_ms)

e13v_bet_n_01_report <- rwl.report(e13v_bet_n_01)  #report on rwl
print(e13v_bet_n_01_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13v_bet_n_01_short) #creates a segment plot
title(main = "E13VBetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13v_bet_n_01_short, zfac=0.02, cex = 0.3) #creates a spaghetti plot
title(main = "E13VBetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#average####
#create a Df with the average of each section for e13v_bet_n_01
# Create a new DataFrame with the same row names as the original DataFrame
e13v_bet_n_01_average <- data.frame(RowNames = rownames(e13v_bet_n_01))

# Calculate the average for each set of columns and add them to the new DataFrame
colnames(e13v_bet_n_01)

#E13VBetn01o
e13v_bet_n_01_average$E13VBetn01o01 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01o01r1",
                                                                  "E13VBetn01o01r2", 
                                                                  "E13VBetn01o01r3",
                                                                  "E13VBetn01o01r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01o02 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01o02r1",
                                                                  "E13VBetn01o02r2", 
                                                                  "E13VBetn01o02r3",
                                                                  "E13VBetn01o02r4")], 
                                                na.rm = TRUE)

e13v_bet_n_01_average$E13VBetn01o03 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01o03r1",
                                                                  "E13VBetn01o03r2", 
                                                                  "E13VBetn01o03r3",
                                                                  "E13VBetn01o03r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01o04 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01o04r1",
                                                                  "E13VBetn01o04r2", 
                                                                  "E13VBetn01o04r3",
                                                                  "E13VBetn01o04r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01o05 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01o05r1",
                                                                  "E13VBetn01o05r2", 
                                                                  "E13VBetn01o05r3",
                                                                  "E13VBetn01o05r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01o06 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01o06r1",
                                                                  "E13VBetn01o06r2", 
                                                                  "E13VBetn01o06r3",
                                                                  "E13VBetn01o06r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01o07 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01o07r1",
                                                                  "E13VBetn01o07r2", 
                                                                  "E13VBetn01o07r3",
                                                                  "E13VBetn01o07r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01o08 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01o08r1",
                                                                  "E13VBetn01o08r2", 
                                                                  "E13VBetn01o08r3",
                                                                  "E13VBetn01o08r4")], 
                                                na.rm = TRUE)
#E13VBetn01p
e13v_bet_n_01_average$E13VBetn01p01 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01p01r1",
                                                                  "E13VBetn01p01r2", 
                                                                  "E13VBetn01p01r3",
                                                                  "E13VBetn01p01r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01p02 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01p02r1",
                                                                  "E13VBetn01p02r2", 
                                                                  "E13VBetn01p02r3",
                                                                  "E13VBetn01p02r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01p03 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01p03r1",
                                                                  "E13VBetn01p03r2", 
                                                                  "E13VBetn01p03r3",
                                                                  "E13VBetn01p03r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01p04 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01p04r1",
                                                                  "E13VBetn01p04r2", 
                                                                  "E13VBetn01p04r3",
                                                                  "E13VBetn01p04r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01p05 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01p05r1",
                                                                  "E13VBetn01p05r2", 
                                                                  "E13VBetn01p05r3",
                                                                  "E13VBetn01p05r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01p06 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01p06r1",
                                                                  "E13VBetn01p06r2", 
                                                                  "E13VBetn01p06r3",
                                                                  "E13VBetn01p06r4")], 
                                                na.rm = TRUE)
#E13VBetn01q
e13v_bet_n_01_average$E13VBetn01q01 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01q01r1",
                                                                  "E13VBetn01q01r2", 
                                                                  "E13VBetn01q01r3",
                                                                  "E13VBetn01q01r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01q02 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01q02r1",
                                                                  "E13VBetn01q02r2", 
                                                                  "E13VBetn01q02r3",
                                                                  "E13VBetn01q02r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01q03 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01q03r1",
                                                                  "E13VBetn01q03r2", 
                                                                  "E13VBetn01q03r3",
                                                                  "E13VBetn01q03r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01q04 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01q04r1",
                                                                  "E13VBetn01q04r2", 
                                                                  "E13VBetn01q04r3",
                                                                  "E13VBetn01q04r4")], 
                                                na.rm = TRUE)
#E13VBetn01r
e13v_bet_n_01_average$E13VBetn01r01 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01r01r1",
                                                                  "E13VBetn01r01r2", 
                                                                  "E13VBetn01r01r3",
                                                                  "E13VBetn01r01r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01r02 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01r02r1",
                                                                  "E13VBetn01r02r2", 
                                                                  "E13VBetn01r02r3",
                                                                  "E13VBetn01r02r4")], 
                                                na.rm = TRUE)

e13v_bet_n_01_average$E13VBetn01r03 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01r03r1",
                                                                  "E13VBetn01r03r2", 
                                                                  "E13VBetn01r03r3",
                                                                  "E13VBetn01r03r4")], 
                                                na.rm = TRUE)

e13v_bet_n_01_average$E13VBetn01r04 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01r04r1",
                                                                  "E13VBetn01r04r2", 
                                                                  "E13VBetn01r04r3",
                                                                  "E13VBetn01r04r4")], 
                                                na.rm = TRUE)

e13v_bet_n_01_average$E13VBetn01r05 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01r05r1",
                                                                  "E13VBetn01r05r2", 
                                                                  "E13VBetn01r05r3",
                                                                  "E13VBetn01r05r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01r06 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01r06r1",
                                                                  "E13VBetn01r06r2", 
                                                                  "E13VBetn01r06r3",
                                                                  "E13VBetn01r06r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01r07 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01r07r1",
                                                                  "E13VBetn01r07r2", 
                                                                  "E13VBetn01r07r3",
                                                                  "E13VBetn01r07r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01r08 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01r08r1",
                                                                  "E13VBetn01r08r2", 
                                                                  "E13VBetn01r08r3",
                                                                  "E13VBetn01r08r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01r09 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01r09r1",
                                                                  "E13VBetn01r09r2", 
                                                                  "E13VBetn01r09r3",
                                                                  "E13VBetn01r09r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01r10 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01r10r1",
                                                                  "E13VBetn01r10r2", 
                                                                  "E13VBetn01r10r3",
                                                                  "E13VBetn01r10r4")], 
                                                na.rm = TRUE)

#E13VBetn01s
e13v_bet_n_01_average$E13VBetn01s01 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01s01r1",
                                                                  "E13VBetn01s01r2", 
                                                                  "E13VBetn01s01r3",
                                                                  "E13VBetn01s01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_01_average$E13VBetn01s02 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01s02r1",
                                                                  "E13VBetn01s02r2", 
                                                                  "E13VBetn01s02r3",
                                                                  "E13VBetn01s02r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01s03 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01s03r1",
                                                                  "E13VBetn01s03r2", 
                                                                  "E13VBetn01s03r3",
                                                                  "E13VBetn01s03r4")], 
                                                na.rm = TRUE)

e13v_bet_n_01_average$E13VBetn01s04 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01s04r1",
                                                                  "E13VBetn01s04r2",
                                                                  "E13VBetn01s04r3",
                                                                  "E13VBetn01s04r4")],
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01s05 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01s05r1",
                                                                  "E13VBetn01s05r2", 
                                                                  "E13VBetn01s05r3",
                                                                  "E13VBetn01s05r4")], 
                                                na.rm = TRUE)

#E13VBetn01t
e13v_bet_n_01_average$E13VBetn01t01 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01t01r1",
                                                                  "E13VBetn01t01r2", 
                                                                  "E13VBetn01t01r3",
                                                                  "E13VBetn01t01r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01t02 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01t02r1",
                                                                  "E13VBetn01t02r2", 
                                                                  "E13VBetn01t02r3",
                                                                  "E13VBetn01t02r4")], 
                                                na.rm = TRUE)

e13v_bet_n_01_average$E13VBetn01t03 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01t03r1",
                                                                  "E13VBetn01t03r2", 
                                                                  "E13VBetn01t03r3",
                                                                  "E13VBetn01t03r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01t04 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01t04r1",
                                                                  "E13VBetn01t04r2", 
                                                                  "E13VBetn01t04r3",
                                                                  "E13VBetn01t04r4")], 
                                                na.rm = TRUE)

e13v_bet_n_01_average$E13VBetn01t05 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01t05r1",
                                                                  "E13VBetn01t05r2", 
                                                                  "E13VBetn01t05r3",
                                                                  "E13VBetn01t05r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01t06 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01t06r1",
                                                                  "E13VBetn01t06r2", 
                                                                  "E13VBetn01t06r3",
                                                                  "E13VBetn01t06r4")], 
                                                na.rm = TRUE)

#E13VBetn01u
e13v_bet_n_01_average$E13VBetn01u01 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01u01r1",
                                                                  "E13VBetn01u01r2", 
                                                                  "E13VBetn01u01r3",
                                                                  "E13VBetn01u01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_01_average$E13VBetn01u02 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01u02r1",
                                                                  "E13VBetn01u02r2", 
                                                                  "E13VBetn01u02r3",
                                                                  "E13VBetn01u02r4")], 
                                                na.rm = TRUE)

e13v_bet_n_01_average$E13VBetn01u03 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01u03r1",
                                                                  "E13VBetn01u03r2", 
                                                                  "E13VBetn01u03r3",
                                                                  "E13VBetn01u03r4")], 
                                                na.rm = TRUE)
e13v_bet_n_01_average$E13VBetn01u04 <- rowMeans(e13v_bet_n_01[, c("E13VBetn01u04r1",
                                                                  "E13VBetn01u04r2", 
                                                                  "E13VBetn01u04r3",
                                                                  "E13VBetn01u04r4")], 
                                                na.rm = TRUE)


# Replace "NaN" with NA in each column
e13v_bet_n_01_average <- lapply(e13v_bet_n_01_average, function(x) {
  x[is.nan(x)] <- NA
  return(x)
})

# Convert the result back to a data frame
e13v_bet_n_01_average <- as.data.frame(e13v_bet_n_01_average)

# Replace "NaN" with NA for RowNames column
rownames(e13v_bet_n_01_average) <- e13v_bet_n_01_average$RowNames
e13v_bet_n_01_average <- e13v_bet_n_01_average[, -1]
e13v_bet_n_01_average <- round(e13v_bet_n_01_average, digits = 3) #round to 3 digits


View(e13v_bet_n_01_average)

write.rwl(e13v_bet_n_01_average, "data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01_average.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#analysis####
##Statistics####
e13v_bet_n_01_average_stats <- rwl.stats(e13v_bet_n_01_average) #summary and stats
print(e13v_bet_n_01_average_stats)

e13v_bet_n_01_average_ms <- sens2(e13v_bet_n_01_average) #calculates the mean sensitivity
print(e13v_bet_n_01_average_ms)

e13v_bet_n_01_average_report <- rwl.report(e13v_bet_n_01_average)  #report on rwl
print(e13v_bet_n_01_average_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13v_bet_n_01_average) #creates a segment plot
title(main = "E13VBetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13v_bet_n_01_average, zfac=0.005,) #creates a spaghetti plot
title(main = "E13VBetn01", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#shorten the names
e13v_bet_n_01_ave_short <- e13v_bet_n_01_average

new_colnames <- sub("^E13VBetn01", "", colnames(e13v_bet_n_01_ave_short))

# Assign the new column names to the data frame
colnames(e13v_bet_n_01_ave_short) <- new_colnames

head(e13v_bet_n_01_ave_short)
colnames(e13v_bet_n_01_ave_short)

spag.plot(e13v_bet_n_01_ave_short, zfac=0.007,) #creates a spaghetti plot
title(main = "E13VBetn01 (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
