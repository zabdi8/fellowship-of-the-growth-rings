#loading packages
library(dplR)

#load rwl files
e13v_bet_n_01 <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01.rwl")
e13v_bet_n_02 <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02.rwl")
e13v_bet_n_03 <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03.rwl")

# merge the data

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13v_bet_n_01$row_names <- rownames(e13v_bet_n_01)
e13v_bet_n_02$row_names <- rownames(e13v_bet_n_02)
e13v_bet_n_03$row_names <- rownames(e13v_bet_n_03)

# Merge the data frames using Reduce and merge
e13v <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
               list(e13v_bet_n_01,
                    e13v_bet_n_02,
                    e13v_bet_n_03)
               )

# Set row names and remove the extra column
rownames(e13v) <- e13v[[common_column]]
e13v[[common_column]] <- NULL

colnames(e13v) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13v <- e13v %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13v)
colnames(e13v)
head(e13v)

write.rwl(e13v, "data/ring_data/aligned/e13v/e13v.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
          )

#reduce the names for easy view
e13v_short <- e13v
colnames(e13v_short)
new_colnames <- sub("^E13VBetn", "", colnames(e13v_short))

# Assign the new column names to the data frame
colnames(e13v_short) <- new_colnames

head(e13v_short)
colnames(e13v_short)

if (ncol(e13v) == ncol(e13v_short)) { #just check if both have the same number of columns  
  print("Both data frames have the same number of columns.")
} else {
  print("The number of columns in the data frames are different.")
}

#Data Analysis####
##Statistics####
e13v_stats <- rwl.stats(e13v) #summary and stats
print(e13v_stats)

e13v_ms <- sens2(e13v) #calculates the mean sensitivity
print(e13v_ms)

#remove values columns with only row with values
# Check which columns have only one non-NA value
cols_to_keep <- colSums(!is.na(e13v)) > 1

# Subset the dataframe to keep only the columns with more than one non-NA value
e13v_filtered <- e13v[, cols_to_keep]

e13v_report <- rwl.report(e13v_filtered)  #report on rwl
print(e13v_report)


##Cross-dating and alignment####

#Check the alignment of the series
#graphs

seg.plot(e13v_short) #creates a segment plot
title(main = "E13VBetn", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13v_short, zfac=0.07, cex = 0.3) #creates a spaghetti plot
title(main = "E13VBetn", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title


#average####

#load rwl files
e13v_bet_n_01_average <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01_average.rwl")
e13v_bet_n_02_average <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02_average.rwl")
e13v_bet_n_03_average <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03_average.rwl")

# merge the data

# Specify the common column for merging
common_column <- "row_names"

# Add row names as a column for each data frame
e13v_bet_n_01_average$row_names <- rownames(e13v_bet_n_01_average)
e13v_bet_n_02_average$row_names <- rownames(e13v_bet_n_02_average)
e13v_bet_n_03_average$row_names <- rownames(e13v_bet_n_03_average)

# Merge the data frames using Reduce and merge
e13v_average <- Reduce(function(x, y) merge(x, y, by = common_column, all = TRUE),
                       list(e13v_bet_n_01_average,
                            e13v_bet_n_02_average,
                            e13v_bet_n_03_average)
)

# Set row names and remove the extra column
rownames(e13v_average) <- e13v_average[[common_column]]
e13v_average[[common_column]] <- NULL

colnames(e13v_average) #check that there is no columns with .x or .y

# # # Remove columns with suffixes .x and .y
#  e13v <- e13v %>%
#    select(-matches("\\.x$"), -matches("\\.y$"))

# View the resulting data frame
View(e13v_average)
colnames(e13v_average)
head(e13v_average)

write.rwl(e13v_average, "data/ring_data/aligned/e13v/e13v_average.rwl", 
          format = "compact",
          e13c_bet_n_01_rwl.hdr,
          append = FALSE,
          prec = 0.001
)

#reduce the names for easy view
e13v_ave_short <- e13v_average
colnames(e13v_ave_short)
new_colnames <- sub("^E13VBetn", "", colnames(e13v_ave_short))

# Assign the new column names to the data frame
colnames(e13v_ave_short) <- new_colnames

head(e13v_ave_short)
colnames(e13v_ave_short)

if (ncol(e13v_average) == ncol(e13v_ave_short)) { #just check if both have the same number of columns  
  print("Both data frames have the same number of columns.")
} else {
  print("The number of columns in the data frames are different.")
}

#Data Analysis####
##Statistics####
e13v_ave_stats <- rwl.stats(e13v_average) #summary and stats
print(e13v_ave_stats)

e13v_ave_ms <- sens2(e13v_average) #calculates the mean sensitivity
print(e13v_ave_ms)

# e13v_ave_report <- rwl.report(e13v_average)  #report on rwl missing values
# print(e13v_ave_report)

##Cross-dating and alignment####

#Check the alignment of the series
#graphs
#filter observations out from 2000s
seg.plot(e13v_ave_short) #creates a segment plot
title(main = "E13VBetn (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title

spag.plot(e13v_ave_short, zfac=0.02, cex = 0.3) #creates a spaghetti plot
title(main = "E13VBetn (Average)", adj = 0.48, line = 5.2, font.main = 2, cex.main = 1.2) #add title
