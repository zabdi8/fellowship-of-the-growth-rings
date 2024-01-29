#Load packages#
library(dplyr)
library(dplR)
library(utils)

#Data Loading####
#first load the data from the measurements from Fiji Software.
#E13C.Bet.n.01#

e13c_bet_n_01_rwl <- read.csv("~/Documents/GitHub/fellowship-of-the-growth-rings/data/ring_data/e13c/e13c.bet.n/e13c_bet_n_01/e13c_bet_n_01.csv")

#convert into rwl format####
rownames(e13c_bet_n_01_rwl) <- e13c_bet_n_01_rwl$year #set the row names as years to make it in the rwl format

e13c_bet_n_01_rwl <- e13c_bet_n_01_rwl[-1] #Remove the "year" column

e13c_bet_n_01_rwl.hdr <- list(site.id = "e13c_bet_n_01_r01",
                            site.name = "BETULA NANA C, VAGAMO REGION",
                            spp.code = "BETN", state.country = "NORWAY",
                            spp = "BETULA NANA", elev = 1300, lat = 61.88333300,
                            long = 9.25000000, first.yr = 2001, last.yr = 2020,
                            lead.invs = "ZABDI LOPEZ", comp.date = "")

write.rwl(rwl.df = e13c_bet_n_01_rwl, fname = "data/ring_data/e13c/e13c.bet.n/e13c_bet_n_01/e13c_bet_n_01.rwl",
          format = "compact", 
          header = e13c_bet_n_01_rwl.hdr, 
          append = FALSE, 
          prec = 0.001)

##e13c_bet_n_01_R (root)####

#Extract every root measurement from e13c_bet_n_01
e13c_bet_n_01_R <- e13c_bet_n_01_rwl[,c("E13CBetn01r01r1", "E13CBetn01r01r2","E13CBetn01r01r3", "E13CBetn01r01r4",
                                        "E13CBetn01r02r1","E13CBetn01r02r2", "E13CBetn01r02r3", "E13CBetn01r02r4",
                                        "E13CBetn01r03r1", "E13CBetn01r03r2", "E13CBetn01r03r3", "E13CBetn01r03r4",
                                        "E13CBetn01r04r1", "E13CBetn01r04r2", "E13CBetn01r04r3", "E13CBetn01r04r4",
                                        "E13CBetn01r05r1", "E13CBetn01r05r2", "E13CBetn01r05r3", "E13CBetn01r05r4")
                                     ]
#write the rwl file
write.rwl(rwl.df = e13c_bet_n_01_R, fname = "data/ring_data/e13c/e13c.bet.n/e13c_bet_n_01/e13c_bet_n_01_R.rwl",
          format = "compact", 
          header = e13c_bet_n_01_rwl.hdr, 
          append = FALSE, 
          prec = 0.001)

##Individual root section####

###E13CBetn01r01####

colnames(e13c_bet_n_01_R)
#split the rwl in sections
#E13CBetn01r01

E13CBetn01r01.names <- c("E13CBetn01r01r1", "E13CBetn01r01r2", "E13CBetn01r01r3", "E13CBetn01r01r4")
E13CBetn01r01 <- na.omit(e13c_bet_n_01_R[,c(E13CBetn01r01.names)])

#E13CBetn01r02
E13CBetn01r02.names <- c("E13CBetn01r02r1", "E13CBetn01r02r2", "E13CBetn01r02r3", "E13CBetn01r02r4")
E13CBetn01r02 <- na.omit(e13c_bet_n_01_R[,c(E13CBetn01r02.names)]) #omit the NAs at the begining of the df

#E13CBetn01r03
E13CBetn01r03.names <- c("E13CBetn01r03r1", "E13CBetn01r03r2", "E13CBetn01r03r3", "E13CBetn01r03r4")
E13CBetn01r03 <- na.omit(e13c_bet_n_01_R[,c(E13CBetn01r03.names)]) #omit the NAs at the begining of the df

#E13CBetn01r04
E13CBetn01r04.names <- c("E13CBetn01r04r1", "E13CBetn01r04r2", "E13CBetn01r04r3", "E13CBetn01r04r4")
E13CBetn01r04 <- na.omit(e13c_bet_n_01_R[,c(E13CBetn01r04.names)]) #omit the NAs at the begining of the df

#E13CBetn01r05
E13CBetn01r05.names <- c("E13CBetn01r05r1", "E13CBetn01r05r2", "E13CBetn01r05r3", "E13CBetn01r05r4")
E13CBetn01r05 <- na.omit(e13c_bet_n_01_R[,c(E13CBetn01r05.names)]) #omit the NAs at the begining of the df

##Create average of R####

# #column names for the R average
# aver_column_names <- c("E13CBetn01r01", "E13CBetn01r02", "E13CBetn01r03", "E13CBetn01r04", "E13CBetn01r05")
# 
# #Create an empty data frame with the same number of rows as e13c_bet_n_01_rwl
# e13c_bet_n_01_R_means <- data.frame(matrix(NA, nrow = nrow(e13c_bet_n_01_rwl), ncol = length(aver_column_names)))
# 
# # Set column names
# colnames(e13c_bet_n_01_R_means) <- aver_column_names
# 
# # Set row names to be the same as e13c_bet_n_01_rwl
# rownames(e13c_bet_n_01_R_means) <- rownames(e13c_bet_n_01_rwl)
# 
# # Loop through the column names and calculate row means
# for (col_name in aver_column_names) {
#   columns_to_average <- grep(col_name, names(e13c_bet_n_01_rwl), value = TRUE)
#   row_means <- rowMeans(e13c_bet_n_01_rwl[, columns_to_average], na.rm = TRUE)
#   # Check if all values are NA, and if so, set the row mean to NA
#   row_means[is.na(row_means)] <- NA
#   e13c_bet_n_01_R_means[col_name] <- row_means
# }
# 
# #write the rwl file
# write.rwl(rwl.df = e13c_bet_n_01_R_means, fname = "data/ring_data/e13c/e13c.bet.n/e13c_bet_n_01/e13c_bet_n_01_R_means.rwl",
#           format = "compact", 
#           header = e13c_bet_n_01_rwl.hdr, 
#           append = FALSE, 
#           prec = 0.001)

