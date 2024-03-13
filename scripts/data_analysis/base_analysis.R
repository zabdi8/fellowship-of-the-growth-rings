#E13A####
##E13A_01####
E13A01base_cor_rest <- cor(e13a_bet_n_01_average$E13ABetn01s01, e13a_bet_n_01_average, 
                              use = "pairwise.complete.obs", method = "spearman")
#Check correlations
mean(E13A01base_cor_rest)

# Convert correlation matrix to a data frame
E13A_01_cor_df <- as.data.frame(E13A01base_cor_rest)

# Add a column for the variable names
E13A_01_cor_df$variable <- rownames(E13A_01_cor_df)

# Reshape the data frame to long format for plotting
E13A_01_cor_df_long <- pivot_longer(E13A_01_cor_df, -variable, names_to = "section", values_to = "e13a01_correlation")

#Extract the category from variable2
E13A_01_cor_df_long$category <- ifelse(grepl("^E13ABetn01[oprq]", E13A_01_cor_df_long$section), 
                                       "below_ground", "above_ground")
##E13A_02####
e13a_bet_n_02_average <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02_average.rwl")
E13A02base_cor_rest <- cor(e13a_bet_n_02_average$E13ABetn02s01, e13a_bet_n_02_average, 
                           use = "pairwise.complete.obs", method = "spearman")
#Check correlations
mean(E13A02base_cor_rest)

# Convert correlation matrix to a data frame
E13A_02_cor_df <- as.data.frame(E13A02base_cor_rest)

# Add a column for the variable names
E13A_02_cor_df$variable <- rownames(E13A_02_cor_df)

# Reshape the data frame to long format for plotting
E13A_02_cor_df_long <- pivot_longer(E13A_02_cor_df, -variable, names_to = "section", values_to = "e13a02_correlation")

#Extract the category from variable2
E13A_02_cor_df_long$category <- ifelse(grepl("^E13ABetn02[oprq]", E13A_02_cor_df_long$section), 
                                       "below_ground", "above_ground")

##E13A_03####
e13a_bet_n_03_average <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03_average.rwl")
E13A03base_cor_rest <- cor(e13a_bet_n_03_average$E13ABetn03s01, e13a_bet_n_03_average, 
                           use = "pairwise.complete.obs", method = "spearman")
#Check correlations
mean(E13A03base_cor_rest)

# Convert correlation matrix to a data frame
E13A_03_cor_df <- as.data.frame(E13A03base_cor_rest)

# Add a column for the variable names
E13A_03_cor_df$variable <- rownames(E13A_03_cor_df)

# Reshape the data frame to long format for plotting
E13A_03_cor_df_long <- pivot_longer(E13A_03_cor_df, -variable, names_to = "section", values_to = "e13a03_correlation")

#Extract the category from variable2
E13A_03_cor_df_long$category <- ifelse(grepl("^E13ABetn03[oprq]", E13A_03_cor_df_long$section), 
                                       "below_ground", "above_ground")


###plot####
base_e13 <- rbind(
  data.frame(x="E13A_01", y=E13A_01_cor_df_long$e13a01_correlation, category = E13A_01_cor_df_long$category),
  data.frame(x="E13A_02", y=E13A_02_cor_df_long$e13a02_correlation, category = E13A_02_cor_df_long$category),
  data.frame(x="E13A_03", y=E13A_03_cor_df_long$e13a03_correlation, category = E13A_03_cor_df_long$category)
  )

ggplot(base_e13, aes(x=x, y=y, color = category)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1) +
  labs(x = "Individuals", y = "Base correlation to body parts") +
  theme_minimal() +
  facet_wrap(~x, scales="free_x")

