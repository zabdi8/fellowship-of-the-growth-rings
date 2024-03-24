#load packages
library(dplR)

#Stats
#E13 FULL####
#E13a
e13a_01_rbar <- rwi.stats(e13a_bet_n_01, min.corr.overlap = 0)
e13a_02_rbar <- rwi.stats(e13a_bet_n_02, min.corr.overlap = 0)
e13a_03_rbar <- rwi.stats(e13a_bet_n_03, min.corr.overlap = 0)

e13a_rbar <- rbind(data.frame(individual = "e13a_01", e13a_01_rbar),
                   data.frame(individual = "e13a_02", e13a_02_rbar),
                   data.frame(individual = "e13a_03", e13a_03_rbar)
                   )

#E13c
e13c_01_rbar <- rwi.stats(e13c_bet_n_01, min.corr.overlap = 0)
e13c_03_rbar <- rwi.stats(e13c_bet_n_03, min.corr.overlap = 0)
e13c_07_rbar <- rwi.stats(e13c_bet_n_07, min.corr.overlap = 0)

e13c_rbar <- rbind(data.frame(individual = "e13c_01", e13c_01_rbar),
                   data.frame(individual = "e13c_03", e13c_03_rbar),
                   data.frame(individual = "e13c_07", e13c_07_rbar)
                   )

#E13d
e13d_bet_n_01 <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01.rwl")
e13d_bet_n_02 <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_02.rwl")
e13d_bet_n_03 <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03.rwl")

e13d_01_rbar <- rwi.stats(e13d_bet_n_01, min.corr.overlap = 0)
e13d_02_rbar <- rwi.stats(e13d_bet_n_02, min.corr.overlap = 0)
e13d_03_rbar <- rwi.stats(e13d_bet_n_03, min.corr.overlap = 0)

e13d_rbar <- rbind(data.frame(individual = "e13d_01", e13d_01_rbar),
                   data.frame(individual = "e13d_02", e13d_02_rbar),
                   data.frame(individual = "e13d_03", e13d_03_rbar)
                   )

#E13v
e13v_01_rbar <- rwi.stats(e13v_bet_n_01, min.corr.overlap = 0)
e13v_02_rbar <- rwi.stats(e13v_bet_n_02, min.corr.overlap = 0)
e13v_03_rbar <- rwi.stats(e13v_bet_n_03, min.corr.overlap = 0)

e13v_rbar <- rbind(data.frame(individual = "e13v_01", e13v_01_rbar),
                   data.frame(individual = "e13v_02", e13v_02_rbar),
                   data.frame(individual = "e13v_03", e13v_03_rbar)
                   )

#E13 FULL
e13_rbar <- rbind(data.frame(e13a_rbar),
                  data.frame(e13c_rbar),
                  data.frame(e13d_rbar),
                  data.frame(e13v_rbar)
                  )
print(e13_rbar)

#E13 old####
#E13a
e13a_bet_n_01_counts <- colSums(!is.na(e13a_bet_n_01))
# Identify columns with more than 3 non-missing values
e13a_bet_n_01_valid_columns <- e13a_bet_n_01_counts > 3
# Subset the dataframe to include only columns with more than 3 non-missing values
e13a_bet_n_01_valid_data <- e13a_bet_n_01[, e13a_bet_n_01_valid_columns]

e13a_bet_n_02_counts <- colSums(!is.na(e13a_bet_n_02))
# Identify columns with more than 3 non-missing values
e13a_bet_n_02_valid_columns <- e13a_bet_n_02_counts > 3
# Subset the dataframe to include only columns with more than 3 non-missing values
e13a_bet_n_02_valid_data <- e13a_bet_n_02[, e13a_bet_n_02_valid_columns]

e13a_bet_n_03_counts <- colSums(!is.na(e13a_bet_n_03))
# Identify columns with more than 3 non-missing values
e13a_bet_n_03_valid_columns <- e13a_bet_n_03_counts > 3
# Subset the dataframe to include only columns with more than 3 non-missing values
e13a_bet_n_03_valid_data <- e13a_bet_n_03[, e13a_bet_n_03_valid_columns]

e13a_01_old_rbar <- rwi.stats(e13a_bet_n_01_valid_data, min.corr.overlap = 0)
e13a_02_old_rbar <- rwi.stats(e13a_bet_n_02_valid_data, min.corr.overlap = 0)
e13a_03_old_rbar <- rwi.stats(e13a_bet_n_03_valid_data, min.corr.overlap = 0)

e13a_old_rbar <- rbind(data.frame(individual = "e13a_01", e13a_01_old_rbar),
                   data.frame(individual = "e13a_02", e13a_02_old_rbar),
                   data.frame(individual = "e13a_03", e13a_03_old_rbar)
                   )

#E13c
e13c_bet_n_01_counts <- colSums(!is.na(e13c_bet_n_01))
# Identify columns with more than 3 non-missing values
e13c_bet_n_01_valid_columns <- e13c_bet_n_01_counts > 3
# Subset the dataframe to include only columns with more than 3 non-missing values
e13c_bet_n_01_valid_data <- e13c_bet_n_01[, e13c_bet_n_01_valid_columns]

e13c_bet_n_03_counts <- colSums(!is.na(e13c_bet_n_03))
# Identify columns with more than 3 non-missing values
e13c_bet_n_03_valid_columns <- e13c_bet_n_03_counts > 3
# Subset the dataframe to include only columns with more than 3 non-missing values
e13c_bet_n_03_valid_data <- e13c_bet_n_03[, e13c_bet_n_03_valid_columns]

e13c_bet_n_07_counts <- colSums(!is.na(e13c_bet_n_07))
# Identify columns with more than 3 non-missing values
e13c_bet_n_07_valid_columns <- e13c_bet_n_07_counts > 3
# Subset the dataframe to include only columns with more than 3 non-missing values
e13c_bet_n_07_valid_data <- e13c_bet_n_07[, e13c_bet_n_07_valid_columns]



e13c_01_old_rbar <- rwi.stats(e13c_bet_n_01_valid_data, min.corr.overlap = 0)
e13c_03_old_rbar <- rwi.stats(e13c_bet_n_03_valid_data, min.corr.overlap = 0)
e13c_07_old_rbar <- rwi.stats(e13c_bet_n_07_valid_data, min.corr.overlap = 0)

e13c_old_rbar <- rbind(data.frame(individual = "e13c_01", e13c_01_old_rbar),
                       data.frame(individual = "e13c_03", e13c_03_old_rbar),
                       data.frame(individual = "e13c_07", e13c_07_old_rbar)
                       )

#E13d
e13d_bet_n_01_counts <- colSums(!is.na(e13d_bet_n_01))
# Identify columns with more than 3 non-missing values
e13d_bet_n_01_valid_columns <- e13d_bet_n_01_counts > 3
# Subset the dataframe to include only columns with more than 3 non-missing values
e13d_bet_n_01_valid_data <- e13d_bet_n_01[, e13d_bet_n_01_valid_columns]

e13d_bet_n_02_counts <- colSums(!is.na(e13d_bet_n_02))
# Identify columns with more than 3 non-missing values
e13d_bet_n_02_valid_columns <- e13d_bet_n_02_counts > 3
# Subset the dataframe to include only columns with more than 3 non-missing values
e13d_bet_n_02_valid_data <- e13d_bet_n_02[, e13d_bet_n_02_valid_columns]

e13d_bet_n_03_counts <- colSums(!is.na(e13d_bet_n_03))
# Identify columns with more than 3 non-missing values
e13d_bet_n_03_valid_columns <- e13d_bet_n_03_counts > 3
# Subset the dataframe to include only columns with more than 3 non-missing values
e13d_bet_n_03_valid_data <- e13d_bet_n_03[, e13d_bet_n_03_valid_columns]

e13d_01_old_rbar <- rwi.stats(e13d_bet_n_01_valid_data, min.corr.overlap = 0)
e13d_02_old_rbar <- rwi.stats(e13d_bet_n_02_valid_data, min.corr.overlap = 0)
e13d_03_old_rbar <- rwi.stats(e13d_bet_n_03_valid_data, min.corr.overlap = 0)

e13d_old_rbar <- rbind(data.frame(individual = "e13d_01", e13d_01_old_rbar),
                       data.frame(individual = "e13d_02", e13d_02_old_rbar),
                       data.frame(individual = "e13d_03", e13d_03_old_rbar)
                       )

#E13v
e13v_bet_n_01_counts <- colSums(!is.na(e13v_bet_n_01))
# Identify columns with more than 3 non-missing values
e13v_bet_n_01_valid_columns <- e13v_bet_n_01_counts > 3
# Subset the dataframe to include only columns with more than 3 non-missing values
e13v_bet_n_01_valid_data <- e13v_bet_n_01[, e13v_bet_n_01_valid_columns]

e13v_bet_n_02_counts <- colSums(!is.na(e13v_bet_n_02))
# Identify columns with more than 3 non-missing values
e13v_bet_n_02_valid_columns <- e13v_bet_n_02_counts > 3
# Subset the dataframe to include only columns with more than 3 non-missing values
e13v_bet_n_02_valid_data <- e13v_bet_n_02[, e13v_bet_n_02_valid_columns]

e13v_bet_n_03_counts <- colSums(!is.na(e13v_bet_n_03))
# Identify columns with more than 3 non-missing values
e13v_bet_n_03_valid_columns <- e13v_bet_n_03_counts > 3
# Subset the dataframe to include only columns with more than 3 non-missing values
e13v_bet_n_03_valid_data <- e13v_bet_n_03[, e13v_bet_n_03_valid_columns]

e13v_01_old_rbar <- rwi.stats(e13v_bet_n_01_valid_data, min.corr.overlap = 0)
e13v_02_old_rbar <- rwi.stats(e13v_bet_n_02_valid_data, min.corr.overlap = 0)
e13v_03_old_rbar <- rwi.stats(e13v_bet_n_03_valid_data, min.corr.overlap = 0)

e13v_old_rbar <- rbind(data.frame(individual = "e13v_01", e13v_01_old_rbar),
                       data.frame(individual = "e13v_02", e13v_02_old_rbar),
                       data.frame(individual = "e13v_03", e13v_03_old_rbar)
                       )

#E13 FULL
e13_old_rbar <- rbind(data.frame(e13a_old_rbar),
                  data.frame(e13c_old_rbar),
                  data.frame(e13d_old_rbar),
                  data.frame(e13v_old_rbar)
                  )

comparison_e13_old_full <- rbind(data.frame(individual = e13_old_rbar$individual, 
                                            e13_old_rbar = e13_old_rbar$rbar.tot,
                                            e13_rbar = e13_rbar$rbar.tot)
                                 )

      