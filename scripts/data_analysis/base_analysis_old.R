#load libraries
library(dplyr)
library(ggplot2)

#E13A####
##E13A_01####
e13a_bet_n_01_average <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_01_average.rwl")
#extract only the columns that have more than 3 years
#First look for the values in each column
e13a_bet_n_01_average_counts <- colSums(!is.na(e13a_bet_n_01_average))

# Identify columns with more than 3 non-missing values
e13a_bet_n_01_average_valid_columns <- e13a_bet_n_01_average_counts > 3

# Subset the dataframe to include only columns with more than 3 non-missing values
e13a_bet_n_01_average_valid_data <- e13a_bet_n_01_average[, e13a_bet_n_01_average_valid_columns]

# Compute correlation using only valid columns
E13A01_old_base_cor_rest <- cor(e13a_bet_n_01_average_valid_data$E13ABetn01s01, 
                                e13a_bet_n_01_average_valid_data[, -which(
                                    names(e13a_bet_n_01_average_valid_data) == "E13ABetn01s01")],
                              use = "pairwise.complete.obs", method = "spearman")
#Check correlations
mean(E13A01_old_base_cor_rest)

# Convert correlation matrix to a data frame
E13A_01_old_cor_df <- as.data.frame(E13A01_old_base_cor_rest)

# Add a column for the variable names
E13A_01_old_cor_df$variable <- rownames(E13A_01_old_cor_df)

# Reshape the data frame to long format for plotting
E13A_01_old_cor_df_long <- pivot_longer(E13A_01_old_cor_df, -variable, names_to = "section", values_to = "e13a01_correlation")

#Extract the category from variable2
E13A_01_old_cor_df_long$category <- ifelse(grepl("^E13ABetn01[oprq]", E13A_01_old_cor_df_long$section), 
                                       "below_ground", "above_ground")
##E13A_02####
e13a_bet_n_02_average <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02_average.rwl")
#extract only the columns that have more than 3 years
#First look for the values in each column
e13a_bet_n_02_old_average_counts <- colSums(!is.na(e13a_bet_n_02_average))

# Identify columns with more than 3 non-missing values
e13a_bet_n_02_old_average_valid_columns <- e13a_bet_n_02_old_average_counts > 3

# Subset the dataframe to include only columns with more than 3 non-missing values
e13a_bet_n_02_average_old_valid_data <- e13a_bet_n_02_average[, e13a_bet_n_02_old_average_valid_columns]

E13A02_old_base_cor_rest <- cor(e13a_bet_n_02_average_old_valid_data$E13ABetn02s01,
                                e13a_bet_n_02_average_old_valid_data[, -which(
                                  names(e13a_bet_n_02_average_old_valid_data) == "E13ABetn02s01")], 
                           use = "pairwise.complete.obs", method = "spearman")
#Check correlations
mean(E13A02_old_base_cor_rest)

# Convert correlation matrix to a data frame
E13A_02_old_cor_df <- as.data.frame(E13A02_old_base_cor_rest)

# Add a column for the variable names
E13A_02_old_cor_df$variable <- rownames(E13A_02_old_cor_df)

# Reshape the data frame to long format for plotting
E13A_02_old_cor_df_long <- pivot_longer(E13A_02_old_cor_df, -variable, names_to = "section", values_to = "e13a02_correlation")

#Extract the category from variable2
E13A_02_old_cor_df_long$category <- ifelse(grepl("^E13ABetn02[oprq]", E13A_02_old_cor_df_long$section), 
                                       "below_ground", "above_ground")

##E13A_03####
e13a_bet_n_03_average <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03_average.rwl")
#Extract only the columns that have more than 3 years
#First look for the values in each column
e13a_bet_n_03_old_average_counts <- colSums(!is.na(e13a_bet_n_03_average))

# Identify columns with more than 3 non-missing values
e13a_bet_n_03_old_average_valid_columns <- e13a_bet_n_03_old_average_counts > 3

# Subset the dataframe to include only columns with more than 3 non-missing values
e13a_bet_n_03_average_old_valid_data <- e13a_bet_n_03_average[, e13a_bet_n_03_old_average_valid_columns]

E13A03_old_base_cor_rest <- cor(e13a_bet_n_03_average_old_valid_data$E13ABetn03s01, 
                                e13a_bet_n_03_average_old_valid_data[, -which(
                                  names(e13a_bet_n_03_average_old_valid_data) == "E13ABetn03s01")], 
                           use = "pairwise.complete.obs", method = "spearman")
#Check correlations
mean(E13A03_old_base_cor_rest)

# Convert correlation matrix to a data frame
E13A_03_old_cor_df <- as.data.frame(E13A03_old_base_cor_rest)

# Add a column for the variable names
E13A_03_old_cor_df$variable <- rownames(E13A_03_old_cor_df)

# Reshape the data frame to long format for plotting
E13A_03_old_cor_df_long <- pivot_longer(E13A_03_old_cor_df, -variable, names_to = "section", values_to = "e13a03_correlation")

#Extract the category from variable2
E13A_03_old_cor_df_long$category <- ifelse(grepl("^E13ABetn03[oprq]", E13A_03_old_cor_df_long$section), 
                                       "below_ground", "above_ground")

##DF's ####
#that contains all three individuals
base_e13a_old_indv <- rbind(
  data.frame(x="E13A_01", category = E13A_01_old_cor_df_long$category, 
             correlation = E13A_01_old_cor_df_long$e13a01_correlation),
  data.frame(x="E13A_02", category = E13A_02_old_cor_df_long$category, 
             correlation = E13A_02_old_cor_df_long$e13a02_correlation),
  data.frame(x="E13A_03", category = E13A_03_old_cor_df_long$category,
             correlation =E13A_03_old_cor_df_long$e13a03_correlation)
  )

#create a df to unify all individuals
base_e13a_old_merge <- base_e13a_old_indv

#add a shape column that will be used to separate from individuals in the boxplot for all individuals
base_e13a_old_merge <- base_e13a_old_merge %>%
  mutate(shape = case_when(
    x == "E13A_01" ~ "16",
    x == "E13A_02" ~ "15",
    x == "E13A_03" ~ "8",
    TRUE ~ NA_character_
  ))

base_e13a_old_indv$shape <- "16"

##plot E13A####
#Create a df that gathers all the information of the three individuals
base_e13a_old <- rbind(
  data.frame(x=base_e13a_old_indv$x, correlation=base_e13a_old_indv$correlation, category = base_e13a_old_indv$category, shape = base_e13a_old_indv$shape),
  data.frame(x="E13A", correlation=base_e13a_old_merge$correlation, category = base_e13a_old_merge$category, shape = base_e13a_old_merge$shape)
  )

#plot the df in boxplots
category_palette <- c("below_ground" = "#F1BB7B", "above_ground" = "#0B775E")

ggplot(base_e13a_old, aes(x=x, y=correlation, color = category)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha = 0.6) +
  scale_color_manual(values = category_palette) +
  labs(x = "Individuals (E13A)", y = "Base correlation to body parts", color = "Position") +
  theme_minimal()+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_cartesian(ylim = c(-1, +1)
                  )
ggsave("figures/rings/base_correlation/old_sections/base_to_e13a_old.jpg", width = 7, height = 4)

#E13C####
##E13C_01####
e13c_bet_n_01_average <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/e13c_bet_n_01_average.rwl")
#Extract only the columns that have more than 3 years
#First look for the values in each column
e13c_bet_n_01_old_average_counts <- colSums(!is.na(e13c_bet_n_01_average))

# Identify columns with more than 3 non-missing values
e13c_bet_n_01_old_average_valid_columns <- e13c_bet_n_01_old_average_counts > 3

# Subset the dataframe to include only columns with more than 3 non-missing values
e13c_bet_n_01_average_old_valid_data <- e13c_bet_n_01_average[, e13c_bet_n_01_old_average_valid_columns]


E13C01_old_base_cor_rest <- cor(e13c_bet_n_01_average_old_valid_data$E13CBetn01s01,
                           e13c_bet_n_01_average_old_valid_data[, -which(
                             names(e13c_bet_n_01_average_old_valid_data) == "E13CBetn01s01")], 
                           use = "pairwise.complete.obs", method = "spearman")

#Check correlations
mean(E13C01_old_base_cor_rest)

# Convert correlation matrix to a data frame
E13C_01_old_cor_df <- as.data.frame(E13C01_old_base_cor_rest)

# Add a column for the variable names
E13C_01_old_cor_df$variable <- rownames(E13C_01_old_cor_df)

# Reshape the data frame to long format for plotting
E13C_01_old_cor_df_long <- pivot_longer(E13C_01_old_cor_df, -variable, names_to = "section", values_to = "e13c01_correlation")

#Extract the category from variable2
E13C_01_old_cor_df_long$category <- ifelse(grepl("^E13CBetn01[opqr]", E13C_01_old_cor_df_long$section), 
                                       "below_ground", "above_ground")
##E13C_03####
e13c_bet_n_03_average <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_03/e13c_bet_n_03_average.rwl")
#Extract only the columns that have more than 3 years
#First look for the values in each column
e13c_bet_n_03_old_average_counts <- colSums(!is.na(e13c_bet_n_03_average))

# Identify columns with more than 3 non-missing values
e13c_bet_n_03_old_average_valid_columns <- e13c_bet_n_03_old_average_counts > 3

# Subset the dataframe to include only columns with more than 3 non-missing values
e13c_bet_n_03_average_old_valid_data <- e13c_bet_n_03_average[, e13c_bet_n_03_old_average_valid_columns]


E13C03_old_base_cor_rest <- cor(e13c_bet_n_03_average_old_valid_data$E13CBetn03s01,
                                e13c_bet_n_03_average_old_valid_data[, -which(
                                  names(e13c_bet_n_03_average_old_valid_data) == "E13CBetn03s01")], 
                                use = "pairwise.complete.obs", method = "spearman")

#Check correlations
mean(E13C03_old_base_cor_rest)

# Convert correlation matrix to a data frame
E13C_03_old_cor_df <- as.data.frame(E13C03_old_base_cor_rest)

# Add a column for the variable names
E13C_03_old_cor_df$variable <- rownames(E13C_03_old_cor_df)

# Reshape the data frame to long format for plotting
E13C_03_old_cor_df_long <- pivot_longer(E13C_03_old_cor_df, -variable, names_to = "section", values_to = "e13c03_correlation")

#Extract the category from variable2
E13C_03_old_cor_df_long$category <- ifelse(grepl("^E13CBetn03[oprq]", E13C_03_old_cor_df_long$section), 
                                       "below_ground", "above_ground")

##E13C_07####
e13c_bet_n_07_average <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/e13c_bet_n_07_average.rwl")
#Extract only the columns that have more than 3 years
#First look for the values in each column
e13c_bet_n_07_old_average_counts <- colSums(!is.na(e13c_bet_n_07_average))

# Identify columns with more than 3 non-missing values
e13c_bet_n_07_old_average_valid_columns <- e13c_bet_n_07_old_average_counts > 3

# Subset the dataframe to include only columns with more than 3 non-missing values
e13c_bet_n_07_average_old_valid_data <- e13c_bet_n_07_average[, e13c_bet_n_07_old_average_valid_columns]


E13C07_old_base_cor_rest <- cor(e13c_bet_n_07_average_old_valid_data$E13CBetn07s01,
                                e13c_bet_n_07_average_old_valid_data[, -which(
                                  names(e13c_bet_n_07_average_old_valid_data) == "E13CBetn07s01")], 
                                use = "pairwise.complete.obs", method = "spearman")

#Check correlations
mean(E13C07_old_base_cor_rest)

# Convert correlation matrix to a data frame
E13C_07_old_cor_df <- as.data.frame(E13C07_old_base_cor_rest)

# Add a column for the variable names
E13C_07_old_cor_df$variable <- rownames(E13C_07_old_cor_df)

# Reshape the data frame to long format for plotting
E13C_07_old_cor_df_long <- pivot_longer(E13C_07_old_cor_df, -variable, names_to = "section", values_to = "e13c07_correlation")

#Extract the category from variable2
E13C_07_old_cor_df_long$category <- ifelse(grepl("^E13CBetn07[oprq]", E13C_07_old_cor_df_long$section), 
                                       "below_ground", "above_ground")

##DF's ####
#that contains all three individuals
base_e13c_old_indv <- rbind(
  data.frame(x="E13C_01", category = E13C_01_old_cor_df_long$category, 
             correlation = E13C_01_old_cor_df_long$e13c01_correlation),
  data.frame(x="E13C_03", category = E13C_03_old_cor_df_long$category, 
             correlation = E13C_03_old_cor_df_long$e13c03_correlation),
  data.frame(x="E13C_07", category = E13C_07_old_cor_df_long$category,
             correlation = E13C_07_old_cor_df_long$e13c07_correlation)
  )

#create a df to unify all individuals
base_e13c_old_merge <- base_e13c_old_indv

#add a shape column that will be used to separate from individuals in the boxplot for all individuals
base_e13c_old_merge <- base_e13c_old_merge %>%
  mutate(shape = case_when(
    x == "E13C_01" ~ "16",
    x == "E13C_03" ~ "15",
    x == "E13C_07" ~ "8",
    TRUE ~ NA_character_
  ))

base_e13c_old_indv$shape <- "16"

##plot E13C####
#Create a df that gathers all the information of the three individuals
base_e13c_old <- rbind(
  data.frame(x=base_e13c_old_indv$x, correlation=base_e13c_old_indv$correlation, 
             category = base_e13c_old_indv$category, shape = base_e13c_old_indv$shape),
  data.frame(x="E13C", correlation=base_e13c_old_merge$correlation, category = base_e13c_old_merge$category, 
             shape = base_e13c_old_merge$shape)
  )

#plot the df in boxplots

ggplot(base_e13c_old, aes(x=x, y=correlation, color = category)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha = 0.6) +
  scale_color_manual(values = category_palette) +
  labs(x = "Individuals (E13C)", y = "Base correlation to body parts", color = "Position") +
  theme_minimal() +
  coord_cartesian(ylim = c(-1, +1))

ggsave("figures/rings/base_correlation/old_sections/base_to_e13c_old.jpg", width = 7, height = 4)

#E13D####
##E13D_01####
e13d_bet_n_01_average <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01_average.rwl")
#Extract only the columns that have more than 3 years
#First look for the values in each column
e13d_bet_n_01_old_average_counts <- colSums(!is.na(e13d_bet_n_01_average))

# Identify columns with more than 3 non-missing values
e13d_bet_n_01_old_average_valid_columns <- e13d_bet_n_01_old_average_counts > 3

# Subset the dataframe to include only columns with more than 3 non-missing values
e13d_bet_n_01_average_old_valid_data <- e13d_bet_n_01_average[, e13d_bet_n_01_old_average_valid_columns]


E13D01_old_base_cor_rest <- cor(e13d_bet_n_01_average_old_valid_data$E13DBetn01s01,
                                e13d_bet_n_01_average_old_valid_data[, -which(
                                  names(e13d_bet_n_01_average_old_valid_data) == "E13DBetn01s01")], #avoids correlating with itself
                                use = "pairwise.complete.obs", method = "spearman")

#Check correlations
mean(E13D01_old_base_cor_rest)

# Convert correlation matrix to a data frame
E13D_01_old_cor_df <- as.data.frame(E13D01_old_base_cor_rest)

# Add a column for the variable names
E13D_01_old_cor_df$variable <- rownames(E13D_01_old_cor_df)

# Reshape the data frame to long format for plotting
E13D_01_old_cor_df_long <- pivot_longer(E13D_01_old_cor_df, -variable, names_to = "section", values_to = "e13d01_correlation")

#Extract the category from variable2
E13D_01_old_cor_df_long$category <- ifelse(grepl("^E13DBetn01[opqr]", E13D_01_old_cor_df_long$section), 
                                       "below_ground", "above_ground")
##E13D_02####
e13d_bet_n_02_average <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_02_average.rwl")
#Extract only the columns that have more than 3 years
#First look for the values in each column
e13d_bet_n_02_old_average_counts <- colSums(!is.na(e13d_bet_n_02_average))

# Identify columns with more than 3 non-missing values
e13d_bet_n_02_old_average_valid_columns <- e13d_bet_n_02_old_average_counts > 3

# Subset the dataframe to include only columns with more than 3 non-missing values
e13d_bet_n_02_average_old_valid_data <- e13d_bet_n_02_average[, e13d_bet_n_02_old_average_valid_columns]


E13D02_old_base_cor_rest <- cor(e13d_bet_n_02_average_old_valid_data$E13DBetn02s01,
                                e13d_bet_n_02_average_old_valid_data[, -which(
                                  names(e13d_bet_n_02_average_old_valid_data) == "E13DBetn02s01")], #avoids correlating with itself
                                use = "pairwise.complete.obs", method = "spearman")

#Check correlations
mean(E13D02_old_base_cor_rest)

# Convert correlation matrix to a data frame
E13D_02_old_cor_df <- as.data.frame(E13D02base_cor_rest)

# Add a column for the variable names
E13D_02_old_cor_df$variable <- rownames(E13D_02_old_cor_df)

# Reshape the data frame to long format for plotting
E13D_02_old_cor_df_long <- pivot_longer(E13D_02_old_cor_df, -variable, names_to = "section", values_to = "e13d02_correlation")

#Extract the category from variable2
E13D_02_old_cor_df_long$category <- ifelse(grepl("^E13DBetn02[oprq]", E13D_02_old_cor_df_long$section), 
                                       "below_ground", "above_ground")

##E13D_03####
e13d_bet_n_03_average <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03_average.rwl")
#Extract only the columns that have more than 3 years
#First look for the values in each column
e13d_bet_n_03_old_average_counts <- colSums(!is.na(e13d_bet_n_03_average))

# Identify columns with more than 3 non-missing values
e13d_bet_n_03_old_average_valid_columns <- e13d_bet_n_03_old_average_counts > 3

# Subset the dataframe to include only columns with more than 3 non-missing values
e13d_bet_n_03_average_old_valid_data <- e13d_bet_n_03_average[, e13d_bet_n_03_old_average_valid_columns]


E13D03_old_base_cor_rest <- cor(e13d_bet_n_03_average_old_valid_data$E13DBetn03s01,
                                e13d_bet_n_03_average_old_valid_data[, -which(
                                  names(e13d_bet_n_03_average_old_valid_data) == "E13DBetn03s01")], #avoids correlating with itself
                                use = "pairwise.complete.obs", method = "spearman")

#Check correlations
mean(E13D03_old_base_cor_rest)

# Convert correlation matrix to a data frame
E13D_03_old_cor_df <- as.data.frame(E13D03base_cor_rest)

# Add a column for the variable names
E13D_03_old_cor_df$variable <- rownames(E13D_03_old_cor_df)

# Reshape the data frame to long format for plotting
E13D_03_old_cor_df_long <- pivot_longer(E13D_03_old_cor_df, -variable, names_to = "section", values_to = "e13d03_correlation")

#Extract the category from variable2
E13D_03_old_cor_df_long$category <- ifelse(grepl("^E13DBetn03[oprq]", E13D_03_old_cor_df_long$section), 
                                           "below_ground", "above_ground")

##DF's ####
#that contains all three individuals
base_e13d_old_indv <- rbind(
  data.frame(x="E13D_01", category = E13D_01_old_cor_df_long$category, 
             correlation = E13D_01_old_cor_df_long$e13d01_correlation),
  data.frame(x="E13D_02", category = E13D_02_old_cor_df_long$category, 
             correlation = E13D_02_old_cor_df_long$e13d02_correlation),
  data.frame(x="E13D_03", category = E13D_03_old_cor_df_long$category,
             correlation = E13D_03_old_cor_df_long$e13d03_correlation)
  )

#create a df to unify all individuals
base_e13d_old_merge <- base_e13d_old_indv

#add a shape column that will be used to separate from individuals in the boxplot for all individuals
base_e13d_old_merge <- base_e13d_old_merge %>%
  mutate(shape = case_when(
    x == "E13D_01" ~ "16",
    x == "E13D_02" ~ "15",
    x == "E13D_03" ~ "8",
    TRUE ~ NA_character_
    ))

base_e13d_old_indv$shape <- "16"

##plot E13D####
#Create a df that gathers all the information of the three individuals
base_e13d_old <- rbind(
  data.frame(x=base_e13d_old_indv$x, correlation=base_e13d_old_indv$correlation, 
             category = base_e13d_old_indv$category, shape = base_e13d_old_indv$shape),
  data.frame(x="E13D", correlation=base_e13d_old_merge$correlation, 
             category = base_e13d_old_merge$category, shape = base_e13d_old_merge$shape)
  )

#plot the df in boxplots
ggplot(base_e13d_old, aes(x=x, y=correlation, color = category)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha = 0.6) +
  scale_color_manual(values = category_palette) +
  labs(x = "Individuals (E13D)", y = "Base correlation to body parts", color = "Position") +
  theme_minimal()+
  coord_cartesian(ylim = c(-1, +1))
  
ggsave("figures/rings/base_correlation/old_sections/base_to_e13d_old.jpg", width = 7, height = 4)

#E13V####
##E13V_01####
e13v_bet_n_01_average <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01_average.rwl")
#Extract only the columns that have more than 3 years
#First look for the values in each column
e13v_bet_n_01_old_average_counts <- colSums(!is.na(e13v_bet_n_01_average))

# Identify columns with more than 3 non-missing values
e13v_bet_n_01_old_average_valid_columns <- e13v_bet_n_01_old_average_counts > 3

# Subset the dataframe to include only columns with more than 3 non-missing values
e13v_bet_n_01_average_old_valid_data <- e13v_bet_n_01_average[, e13v_bet_n_01_old_average_valid_columns]


E13V01_old_base_cor_rest <- cor(e13v_bet_n_01_average_old_valid_data$E13VBetn01s01,
                                e13v_bet_n_01_average_old_valid_data[, -which(
                                  names(e13v_bet_n_01_average_old_valid_data) == "E13VBetn01s01")], #avoids correlating with itself
                                use = "pairwise.complete.obs", method = "spearman")

#Check correlations
mean(E13V01_old_base_cor_rest)

# Convert correlation matrix to a data frame
E13V_01_old_cor_df <- as.data.frame(E13V01_old_base_cor_rest)

# Add a column for the variable names
E13V_01_old_cor_df$variable <- rownames(E13V_01_old_cor_df)

# Reshape the data frame to long format for plotting
E13V_01_old_cor_df_long <- pivot_longer(E13V_01_old_cor_df, -variable, names_to = "section", values_to = "e13v01_correlation")

#Extract the category from variable2
E13V_01_old_cor_df_long$category <- ifelse(grepl("^E13VBetn01[opqr]", E13V_01_old_cor_df_long$section), 
                                       "below_ground", "above_ground")
##E13V_02####
e13v_bet_n_02_average <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02_average.rwl")
#Extract only the columns that have more than 3 years
#First look for the values in each column
e13v_bet_n_02_old_average_counts <- colSums(!is.na(e13v_bet_n_02_average))

# Identify columns with more than 3 non-missing values
e13v_bet_n_02_old_average_valid_columns <- e13v_bet_n_02_old_average_counts > 3

# Subset the dataframe to include only columns with more than 3 non-missing values
e13v_bet_n_02_average_old_valid_data <- e13v_bet_n_02_average[, e13v_bet_n_02_old_average_valid_columns]


E13V02_old_base_cor_rest <- cor(e13v_bet_n_02_average_old_valid_data$E13VBetn02s01,
                                e13v_bet_n_02_average_old_valid_data[, -which(
                                  names(e13v_bet_n_02_average_old_valid_data) == "E13VBetn02s01")], #avoids correlating with itself
                                use = "pairwise.complete.obs", method = "spearman")

#Check correlations
mean(E13V02_old_base_cor_rest)

# Convert correlation matrix to a data frame
E13V_02_old_cor_df <- as.data.frame(E13V02_old_base_cor_rest)

# Add a column for the variable names
E13V_02_old_cor_df$variable <- rownames(E13V_02_old_cor_df)

# Reshape the data frame to long format for plotting
E13V_02_old_cor_df_long <- pivot_longer(E13V_02_old_cor_df, -variable, names_to = "section", values_to = "e13v02_correlation")

#Extract the category from variable2
E13V_02_old_cor_df_long$category <- ifelse(grepl("^E13VBetn02[opqr]", E13V_02_old_cor_df_long$section), 
                                           "below_ground", "above_ground")

##E13V_03####
e13v_bet_n_03_average <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03_average.rwl")
#Extract only the columns that have more than 3 years
#First look for the values in each column
e13v_bet_n_03_old_average_counts <- colSums(!is.na(e13v_bet_n_03_average))

# Identify columns with more than 3 non-missing values
e13v_bet_n_03_old_average_valid_columns <- e13v_bet_n_03_old_average_counts > 3

# Subset the dataframe to include only columns with more than 3 non-missing values
e13v_bet_n_03_average_old_valid_data <- e13v_bet_n_03_average[, e13v_bet_n_03_old_average_valid_columns]


E13V03_old_base_cor_rest <- cor(e13v_bet_n_03_average_old_valid_data$E13VBetn03s01,
                                e13v_bet_n_03_average_old_valid_data[, -which(
                                  names(e13v_bet_n_03_average_old_valid_data) == "E13VBetn03s01")], #avoids correlating with itself
                                use = "pairwise.complete.obs", method = "spearman")

#Check correlations
mean(E13V03_old_base_cor_rest)

# Convert correlation matrix to a data frame
E13V_03_old_cor_df <- as.data.frame(E13V03_old_base_cor_rest)

# Add a column for the variable names
E13V_03_old_cor_df$variable <- rownames(E13V_03_old_cor_df)

# Reshape the data frame to long format for plotting
E13V_03_old_cor_df_long <- pivot_longer(E13V_03_old_cor_df, -variable, names_to = "section", values_to = "e13v03_correlation")

#Extract the category from variable2
E13V_03_old_cor_df_long$category <- ifelse(grepl("^E13VBetn03[opqr]", E13V_03_old_cor_df_long$section), 
                                           "below_ground", "above_ground")

##DF's ####
#that contains all three individuals
base_e13v_old_indv <- rbind(
  data.frame(x="E13V_01", category = E13V_01_old_cor_df_long$category, 
             correlation = E13V_01_old_cor_df_long$e13v01_correlation),
  data.frame(x="E13V_02", category = E13V_02_old_cor_df_long$category, 
             correlation = E13V_02_old_cor_df_long$e13v02_correlation),
  data.frame(x="E13V_03", category = E13V_03_old_cor_df_long$category,
             correlation =E13V_03_old_cor_df_long$e13v03_correlation)
  )


#create a df to unify all individuals
base_e13v_old_merge <- base_e13v_old_indv

#add a shape column that will be used to separate from individuals in the boxplot for all individuals
base_e13v_old_merge <- base_e13v_old_merge %>%
  mutate(shape = case_when(
    x == "E13V_01" ~ "16",
    x == "E13V_02" ~ "15",
    x == "E13V_03" ~ "8",
    TRUE ~ NA_character_
  ))

base_e13v_old_indv$shape <- "16"

##plot E13V####
#Create a df that gathers all the information of the three individuals
base_e13v_old <- rbind(data.frame(x=base_e13v_old_indv$x, correlation=base_e13v_old_indv$correlation, 
                                  category = base_e13v_old_indv$category, shape = base_e13v_old_indv$shape),
                       data.frame(x="E13V", correlation=base_e13v_old_merge$correlation,
                                  category = base_e13v_old_merge$category, shape = base_e13v_old_merge$shape)
                       )

#plot the df in boxplots
ggplot(base_e13v_old, aes(x=x, y=correlation, color = category)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha = 0.6) +
  scale_color_manual(values = category_palette) +
  labs(x = "Individuals (E13V)", y = "Base correlation to body parts", color = "Position") +
  theme_minimal()+
  coord_cartesian(ylim = c(-1,1))

ggsave("figures/rings/base_correlation/old_sections/base_to_e13v_old.jpg", width = 7, height = 4)

#All areas####

#per area
base_e13_old_site <- rbind(
  data.frame(x="e13a", correlation=base_e13a_old$correlation, category = base_e13a_old$category,shape = base_e13a_old$shape),
  data.frame(x="e13c", correlation=base_e13c_old$correlation, category = base_e13c_old$category, shape = base_e13c_old$shape),
  data.frame(x="e13d", correlation=base_e13d_old$correlation, category = base_e13d_old$category, shape = base_e13d_old$shape),
  data.frame(x="e13v", correlation=base_e13v_old$correlation, category = base_e13v_old$category, shape = base_e13v_old$shape)
  )

#plot the df in boxplots
ggplot(base_e13_old_site, aes(x=x, y=correlation, color = category)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha = 0.6) +
  scale_color_manual(values = category_palette) +
  labs(x = "Sites", y = "Base correlation to body parts") +
  theme_minimal()+
  coord_cartesian(ylim = c(-1,1))

ggsave("figures/rings/base_correlation/old_sections/base_to_sites_old.jpg", width = 7, height = 4)

#individual per area
base_e13_old_all <- rbind(
  data.frame(x = base_e13a_old$x, correlation = base_e13a_old$correlation, category = base_e13a_old$category, shape = base_e13a_old$shape),
  data.frame(x = base_e13c_old$x, correlation = base_e13c_old$correlation, category = base_e13c_old$category, shape = base_e13c_old$shape),
  data.frame(x=base_e13d_old$x, correlation=base_e13d_old$correlation, category = base_e13d_old$category, shape = base_e13d_old$shape),
  data.frame(x=base_e13v_old$x, correlation=base_e13v_old$correlation, category = base_e13v_old$category, shape = base_e13v_old$shape)
)

#plot the df in boxplots
ggplot(base_e13_old_all, aes(x=x, y=correlation, color = category)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha = 0.6) +
  scale_color_manual(values = category_palette) +
  labs(x = "Sites", y = "Base correlation to body parts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_cartesian(ylim = c(-1,1))

ggsave("figures/rings/base_correlation/old_sections/base_to_sites_individuals_old.jpg", width = 7, height = 4)


#Analysis of above and below correlations####
##E13A####
#separate the data in above and below ground and include the base in both DFs
# Select columns for below the ground and the base of the stem
e13a_bet_n_01_old_average_below <- base_e13a_old[base_e13a_old$category == "below_ground", ]
mean(e13a_bet_n_01_old_average_below$correlation)

# Select columns starting for above the ground and the base
e13a_bet_n_01_old_average_above <- base_e13a_old[base_e13a_old$category == "above_ground", ]

#Plot correlation

color_palette <- c("below_ground" = "#F1BB7B", "above_ground" = "#0B775E")
site_palette <- c("E13A_01" = "#E69F00",
                  "E13A_02"=  "#E69F00",
                  "E13A_03" = "#E69F00",
                  "E13A" = "#E69F00",
                  "e13a" = "#E69F00",
                  "E13C_01" ="#56B4E9",
                  "E13C_03" = "#56B4E9",
                  "E13C_07" = "#56B4E9",
                  "E13C" = "#56B4E9",
                  "e13c" = "#56B4E9",
                  "E13D_01" = "#009E73",
                  "E13D_02" = "#009E73",
                  "E13D_03" = "#009E73",
                  "E13D" = "#009E73",
                  "e13d" = "#009E73",
                  "E13V_01" = "#F0E442",
                  "E13V_02" = "#F0E442",
                  "E13V_03" = "#F0E442",
                  "E13V" = "#F0E442",
                  "e13v" = "#F0E442")

#E13a
ggplot(base_e13a_old[base_e13a_old$x != "E13A", ], aes(x=category, y=correlation, color = x)) +
  geom_boxplot(width=0.1, fill = "white", color = "black") +
  geom_jitter(width=0.05, alpha = 0.5) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  #scale_shape_manual(values = as.factor(base_e13a$shape))+
  labs(x = "Position from ground", y = "Base correlation to body parts", color = "Individual") +
  theme_minimal()+
  coord_cartesian(ylim = c(-1,1))

ggsave("figures/rings/base_correlation/old_sections/base_to_position_e13a_old.jpg", width = 7, height = 4)

#E13c
ggplot(base_e13c_old[base_e13c_old$x != "E13C",], aes(x=category, y=correlation, color = x)) +
  geom_boxplot(width=0.1, fill = "white", color = "black") +
  geom_jitter(width=0.05, alpha = 0.5) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  #scale_shape_manual(values = as.factor(base_e13a$shape))+
  labs(x = "Position from ground", y = "Base correlation to body parts", color = "Individual") +
  theme_minimal()+
  coord_cartesian(ylim = c(-1,1))

ggsave("figures/rings/base_correlation/old_sections/base_to_position_e13c_old.jpg", width = 7, height = 4)

#E13d
ggplot(base_e13d_old[base_e13d_old$x != "E13D",], aes(x=category, y=correlation, color = x)) +
  geom_boxplot(width=0.1, fill = "white", color = "black") +
  geom_jitter(width=0.05, alpha = 0.5) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  #scale_shape_manual(values = as.factor(base_e13a$shape))+
  labs(x = "Position from ground", y = "Base correlation to body parts", color = "Individual") +
  theme_minimal()+
  coord_cartesian(ylim = c(-1,1))

ggsave("figures/rings/base_correlation/old_sections/base_to_position_e13d_old.jpg", width = 7, height = 4)

#E13v
ggplot(base_e13v_old[base_e13v_old$x != "E13V",], aes(x=category, y=correlation, color = x)) +
  geom_boxplot(width=0.1, fill = "white", color = "black") +
  geom_jitter(width=0.05, alpha = 0.6) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  #scale_shape_manual(values = as.factor(base_e13a$shape))+
  labs(x = "Position from ground", y = "Base correlation to body parts", color = "Individual") +
  theme_minimal()+
  coord_cartesian(ylim = c(-1,1))

ggsave("figures/rings/base_correlation/old_sections/base_to_position_e13v_old.jpg", width = 7, height = 4)

#all sites
#category (all sites comparing above vs below)
ggplot(base_e13_old_site, aes(x=category, y=correlation, color = x)) +
  geom_boxplot(width=0.25, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha = 0.6) +
  scale_color_manual(values = site_palette) +
  #scale_shape_manual(values = as.factor(base_e13a$shape))+
  labs(x = "Position from ground", y = "Base correlation to body parts", color = "Individual") +
  theme_minimal()+
  coord_cartesian(ylim = c(-1,1))

ggsave("figures/rings/base_correlation/old_sections/base_to_position_old.jpg", width = 7, height = 4)

#above

ggplot(base_e13_old_site[base_e13_old_site$category == "above_ground", ], 
       aes(x=x, y=correlation, color = x)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha= 0.5) +
  scale_color_manual(values = site_palette) +
  labs(x = "Sites", y = "Base correlation to body parts above ground", color = "Sites") +
  theme_minimal()+
  coord_cartesian(ylim = c(-1,1))+
  guides(color = F)

ggsave("figures/rings/base_correlation/old_sections/base_to_position_above_old.jpg", width = 7, height = 4)

#below

ggplot(base_e13_old_site[base_e13_old_site$category == "below_ground", ], 
       aes(x=x, y=correlation, color = x)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha= 0.5) +
  scale_color_manual(values = site_palette) +
  labs(x = "Sites", y = "Base correlation to body parts below ground", color = "Sites") +
  theme_minimal()+
  coord_cartesian(ylim = c(-1,1))+
  guides(color = F)

ggsave("figures/rings/base_correlation/old_sections/base_to_position_below_old.jpg", width = 7, height = 4)

#all above
ggplot(base_e13_old_all[base_e13_old_all$category == "above_ground", ],
       aes(x=x, y=correlation, color = x)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha= 0.5) +
  scale_color_manual(values = site_palette) +
  labs(x = "Sites", y = "Base correlation to body parts above ground", color = "Sites") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_cartesian(ylim = c(-1,1))+
  guides(color = F)

ggsave("figures/rings/base_correlation/old_sections/base_to_position_above_individuals_old.jpg", width = 7, height = 4)

#all below
ggplot(base_e13_old_all[base_e13_old_all$category == "below_ground", ],
       aes(x=x, y=correlation, color = x)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha= 0.5) +
  scale_color_manual(values = site_palette) +
  labs(x = "Sites", y = "Base correlation to body parts below ground", color = "Sites") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_cartesian(ylim = c(-1,1))+
  guides(color = F)

ggsave("figures/rings/base_correlation/old_sections/base_to_position_below_individuals_old.jpg", 
       width = 7, height = 4)