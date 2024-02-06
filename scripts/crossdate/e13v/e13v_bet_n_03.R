#loading packages
library(dplR)
library(dplyr)


#load rwl files
e13v_bet_n_03_O <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03/e13v_bet_n_03_o.rwl")
e13v_bet_n_03_P <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03/e13v_bet_n_03_p.rwl")
e13v_bet_n_03_Q <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03/e13v_bet_n_03_q.rwl")
e13v_bet_n_03_R <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03/e13v_bet_n_03_r.rwl")
e13v_bet_n_03_S <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03/e13v_bet_n_03_s.rwl")
e13v_bet_n_03_T <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03/e13v_bet_n_03_t.rwl")
e13v_bet_n_03_U <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03/e13v_bet_n_03_u.rwl")





# merge the data


# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13v_bet_n_03_O$row_names <- rownames(e13v_bet_n_03_O)
e13v_bet_n_03_P$row_names <- rownames(e13v_bet_n_03_P)
e13v_bet_n_03_Q$row_names <- rownames(e13v_bet_n_03_Q)
e13v_bet_n_03_R$row_names <- rownames(e13v_bet_n_03_R)
e13v_bet_n_03_S$row_names <- rownames(e13v_bet_n_03_S)
e13v_bet_n_03_T$row_names <- rownames(e13v_bet_n_03_T)
e13v_bet_n_03_U$row_names <- rownames(e13v_bet_n_03_U)

# Merge the data frames using Reduce and merge
e13v_bet_n_03 <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                        list(e13v_bet_n_03_O,
                             e13v_bet_n_03_P,
                             e13v_bet_n_03_Q,
                             e13v_bet_n_03_R, 
                             e13v_bet_n_03_S, 
                             e13v_bet_n_03_T,
                             e13v_bet_n_03_U)
)

# Set row names and remove the extra column
rownames(e13v_bet_n_03) <- e13v_bet_n_03[[common_column]]
e13v_bet_n_03[[common_column]] <- NULL

colnames(e13v_bet_n_03) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13v_bet_n_03 <- e13v_bet_n_03 %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13v_bet_n_03)
colnames(e13v_bet_n_03)
head(e13v_bet_n_03)

write.rwl(e13v_bet_n_03, "data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#reduce the names for easy view
e13v_bet_n_03_short <- e13v_bet_n_03
colnames(e13v_bet_n_03_short)
new_colnames <- sub("^E13VBetn03", "", colnames(e13v_bet_n_03_short))

# Assign the new column names to the data frame
colnames(e13v_bet_n_03_short) <- new_colnames

head(e13v_bet_n_03_short)
colnames(e13v_bet_n_03_short)

if (ncol(e13v_bet_n_03) == ncol(e13v_bet_n_03_short)) { #just check if both have the same number of columns  
  print("Both data frames have the same number of columns.")
} else {
  print("The number of columns in the data frames are different.")
}

#Data Analysis####
##Statistics####
e13v_bet_n_03_stats <- rwl.stats(e13v_bet_n_03) #summary and stats
print(e13v_bet_n_03_stats)

e13v_bet_n_03_ms <- sens2(e13v_bet_n_03) #calculates the mean sensitivity
print(e13v_bet_n_03_ms)

e13v_bet_n_03_report <- rwl.report(e13v_bet_n_03)  #report on rwl
print(e13v_bet_n_03_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13v_bet_n_03_short) #creates a segment plot
title(main = "E13VBetn03", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13v_bet_n_03_short, zfac=0.02, cex = 0.3) #creates a spaghetti plot
title(main = "E13VBetn03", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title


#average####
#create a Df with the average of each section for e13v_bet_n_03
# Create a new DataFrame with the same row names as the original DataFrame
e13v_bet_n_03_average <- data.frame(RowNames = rownames(e13v_bet_n_03))

# Calculate the average for each set of columns and add them to the new DataFrame
colnames(e13v_bet_n_03)

#E13VBetn03o
e13v_bet_n_03_average$E13VBetn03o01 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03o01r1",
                                                                  "E13VBetn03o01r2", 
                                                                  "E13VBetn03o01r3",
                                                                  "E13VBetn03o01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_03_average$E13VBetn03o03 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03o03r1",
                                                                  "E13VBetn03o03r2", 
                                                                  "E13VBetn03o03r3",
                                                                  "E13VBetn03o03r4")], 
                                                na.rm = TRUE)

e13v_bet_n_03_average$E13VBetn03o04 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03o04r1",
                                                                  "E13VBetn03o04r2", 
                                                                  "E13VBetn03o04r3",
                                                                  "E13VBetn03o04r4")], 
                                                na.rm = TRUE)

#E13VBetn03p
e13v_bet_n_03_average$E13VBetn03p01 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03p01r1",
                                                                  "E13VBetn03p01r2", 
                                                                  "E13VBetn03p01r3",
                                                                  "E13VBetn03p01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_03_average$E13VBetn03p05 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03p05r1",
                                                                  "E13VBetn03p05r2", 
                                                                  "E13VBetn03p05r3",
                                                                  "E13VBetn03p05r4")], 
                                                na.rm = TRUE)

e13v_bet_n_03_average$E13VBetn03p10 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03p10r1",
                                                                  "E13VBetn03p10r2", 
                                                                  "E13VBetn03p10r3",
                                                                  "E13VBetn03p10r4")], 
                                                na.rm = TRUE)
#E13VBetn03q
e13v_bet_n_03_average$E13VBetn03q01 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03q01r1",
                                                                  "E13VBetn03q01r2", 
                                                                  "E13VBetn03q01r3",
                                                                  "E13VBetn03q01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_03_average$E13VBetn03q05 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03q05r1",
                                                                  "E13VBetn03q05r2", 
                                                                  "E13VBetn03q05r3",
                                                                  "E13VBetn03q05r4")], 
                                                na.rm = TRUE)

e13v_bet_n_03_average$E13VBetn03q08 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03q08r1",
                                                                  "E13VBetn03q08r2", 
                                                                  "E13VBetn03q08r3",
                                                                  "E13VBetn03q08r4")], 
                                                na.rm = TRUE)


#E13VBetn03r
e13v_bet_n_03_average$E13VBetn03r01 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03r01r1",
                                                                  "E13VBetn03r01r2", 
                                                                  "E13VBetn03r01r3",
                                                                  "E13VBetn03r01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_03_average$E13VBetn03r03 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03r03r1",
                                                                  "E13VBetn03r03r2", 
                                                                  "E13VBetn03r03r3",
                                                                  "E13VBetn03r03r4")], 
                                                na.rm = TRUE)

e13v_bet_n_03_average$E13VBetn03r04 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03r04r1",
                                                                  "E13VBetn03r04r2", 
                                                                  "E13VBetn03r04r3",
                                                                  "E13VBetn03r04r4")], 
                                                na.rm = TRUE)

#E13VBetn03s

e13v_bet_n_03_average$E13VBetn03s01 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03s01r1",
                                                                  "E13VBetn03s01r2", 
                                                                  "E13VBetn03s01r3",
                                                                  "E13VBetn03s01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_03_average$E13VBetn03s05 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03s05r1",
                                                                  "E13VBetn03s05r2",
                                                                  "E13VBetn03s05r3",
                                                                  "E13VBetn03s05r4")],
                                                na.rm = TRUE)
e13v_bet_n_03_average$E13VBetn03s10 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03s10r1",
                                                                  "E13VBetn03s10r2", 
                                                                  "E13VBetn03s10r3",
                                                                  "E13VBetn03s10r4")], 
                                                na.rm = TRUE)

#E13VBetn03t
e13v_bet_n_03_average$E13VBetn03t01 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03t01r1",
                                                                  "E13VBetn03t01r2", 
                                                                  "E13VBetn03t01r3",
                                                                  "E13VBetn03t01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_03_average$E13VBetn03t05 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03t05r1",
                                                                  "E13VBetn03t05r2", 
                                                                  "E13VBetn03t05r3",
                                                                  "E13VBetn03t05r4")], 
                                                na.rm = TRUE)

e13v_bet_n_03_average$E13VBetn03t08 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03t08r1",
                                                                  "E13VBetn03t08r2", 
                                                                  "E13VBetn03t08r3",
                                                                  "E13VBetn03t08r4")], 
                                                na.rm = TRUE)

#E13VBetn03u
e13v_bet_n_03_average$E13VBetn03u01 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03u01r1",
                                                                  "E13VBetn03u01r2", 
                                                                  "E13VBetn03u01r3",
                                                                  "E13VBetn03u01r4")], 
                                                na.rm = TRUE)

e13v_bet_n_03_average$E13VBetn03u04 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03u04r1",
                                                                  "E13VBetn03u04r2", 
                                                                  "E13VBetn03u04r3",
                                                                  "E13VBetn03u04r4")], 
                                                na.rm = TRUE)

e13v_bet_n_03_average$E13VBetn03u08 <- rowMeans(e13v_bet_n_03[, c("E13VBetn03u08r1",
                                                                  "E13VBetn03u08r2", 
                                                                  "E13VBetn03u08r3",
                                                                  "E13VBetn03u08r4")], 
                                                na.rm = TRUE)

# Replace "NaN" with NA in each column
e13v_bet_n_03_average <- lapply(e13v_bet_n_03_average, function(x) {
  x[is.nan(x)] <- NA
  return(x)
})

# Convert the result back to a data frame
e13v_bet_n_03_average <- as.data.frame(e13v_bet_n_03_average)

# Replace "NaN" with NA for RowNames column
rownames(e13v_bet_n_03_average) <- e13v_bet_n_03_average$RowNames
e13v_bet_n_03_average <- e13v_bet_n_03_average[, -1]
e13v_bet_n_03_average <- round(e13v_bet_n_03_average, digits = 3) #round to 3 digits


View(e13v_bet_n_03_average)

write.rwl(e13v_bet_n_03_average, "data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03_average.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#analysis####
##Statistics####
e13v_bet_n_03_average_stats <- rwl.stats(e13v_bet_n_03_average) #summary and stats
print(e13v_bet_n_03_average_stats)

e13v_bet_n_03_average_ms <- sens2(e13v_bet_n_03_average) #calculates the mean sensitivity
print(e13v_bet_n_03_average_ms)

e13v_bet_n_03_average_report <- rwl.report(e13v_bet_n_03_average)  #report on rwl
print(e13v_bet_n_03_average_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
seg.plot(e13v_bet_n_03_average) #creates a segment plot
title(main = "E13VBetn03", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13v_bet_n_03_average, zfac=0.005,) #creates a spaghetti plot
title(main = "E13VBetn03", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

#shorten the names
e13v_bet_n_03_ave_short <- e13v_bet_n_03_average

new_colnames <- sub("^E13VBetn03", "", colnames(e13v_bet_n_03_ave_short))

# Assign the new column names to the data frame
colnames(e13v_bet_n_03_ave_short) <- new_colnames

head(e13v_bet_n_03_ave_short)
colnames(e13v_bet_n_03_ave_short)

spag.plot(e13v_bet_n_03_ave_short, zfac=0.007,) #creates a spaghetti plot
title(main = "E13VBetn03 (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
