#load libraries
library(dplyr)
library(ggplot2)

#E13A####
##E13A_01####
e13a_bet_n_01_average <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_01_average.rwl")
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

##DF's ####
#that contains all three individuals
base_e13_indv <- rbind(
  data.frame(x="E13A_01", category = E13A_01_cor_df_long$category, 
             correlation = E13A_01_cor_df_long$e13a01_correlation),
  data.frame(x="E13A_02", category = E13A_02_cor_df_long$category, 
             correlation = E13A_02_cor_df_long$e13a02_correlation),
  data.frame(x="E13A_03", category = E13A_03_cor_df_long$category,
             correlation =E13A_03_cor_df_long$e13a03_correlation)
  )

#create a df to unify all individuals
base_e13_merge <- base_e13_indv

#add a shape column that will be used to separate from individuals in the boxplot for all individuals
base_e13_merge <- base_e13_merge %>%
  mutate(shape = case_when(
    x == "E13A_01" ~ "16",
    x == "E13A_02" ~ "15",
    x == "E13A_03" ~ "8",
    TRUE ~ NA_character_
  ))

base_e13_indv$shape <- "16"

##plot E13A####
#Create a df that gathers all the information of the three individuals
base_e13a <- rbind(
  data.frame(x=base_e13_indv$x, correlation=base_e13_indv$correlation, category = base_e13_indv$category, shape = base_e13_indv$shape),
  data.frame(x="E13A", correlation=base_e13_merge$correlation, category = base_e13_merge$category, shape = base_e13_merge$shape)
  )

#plot the df in boxplots
category_palette <- c("below_ground" = "#F1BB7B", "above_ground" = "#0B775E")

ggplot(base_e13a, aes(x=x, y=correlation, color = category)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1) +
  scale_color_manual(values = category_palette) +
  labs(x = "Individuals (E13a)", y = "Base correlation to body parts") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("figures/rings/base_correlation/base_to_e13a.jpg", width = 7, height = 4)

#E13C####
##E13C_01####
e13c_bet_n_01_average <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_01/e13c_bet_n_01_average.rwl")

E13C01base_cor_rest <- cor(e13c_bet_n_01_average$E13CBetn01s01, e13c_bet_n_01_average, 
                           use = "pairwise.complete.obs", method = "spearman")

#Check correlations
mean(E13C01base_cor_rest)

# Convert correlation matrix to a data frame
E13C_01_cor_df <- as.data.frame(E13C01base_cor_rest)

# Add a column for the variable names
E13C_01_cor_df$variable <- rownames(E13C_01_cor_df)

# Reshape the data frame to long format for plotting
E13C_01_cor_df_long <- pivot_longer(E13C_01_cor_df, -variable, names_to = "section", values_to = "e13c01_correlation")

#Extract the category from variable2
E13C_01_cor_df_long$category <- ifelse(grepl("^E13CBetn01[opqr]", E13C_01_cor_df_long$section), 
                                       "below_ground", "above_ground")
##E13C_03####
e13c_bet_n_03_average <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_03/e13c_bet_n_03_average.rwl")
E13C03base_cor_rest <- cor(e13c_bet_n_03_average$E13CBetn03s01, e13c_bet_n_03_average, 
                           use = "pairwise.complete.obs", method = "spearman")
#Check correlations
mean(E13C03base_cor_rest, na.rm = T)

# Convert correlation matrix to a data frame
E13C_03_cor_df <- as.data.frame(E13C03base_cor_rest)

# Add a column for the variable names
E13C_03_cor_df$variable <- rownames(E13C_03_cor_df)

# Reshape the data frame to long format for plotting
E13C_03_cor_df_long <- pivot_longer(E13C_03_cor_df, -variable, names_to = "section", values_to = "e13c03_correlation")

#Extract the category from variable2
E13C_03_cor_df_long$category <- ifelse(grepl("^E13CBetn03[oprq]", E13C_03_cor_df_long$section), 
                                       "below_ground", "above_ground")

##E13C_07####
e13c_bet_n_07_average <- read.rwl("data/ring_data/aligned/e13c/e13c.bet.n/e13c_bet_n_07/e13c_bet_n_07_average.rwl")
E13C07base_cor_rest <- cor(e13c_bet_n_07_average$E13CBetn07s01, e13c_bet_n_07_average, 
                           use = "pairwise.complete.obs", method = "spearman")
#Check correlations
mean(E13C07base_cor_rest)

# Convert correlation matrix to a data frame
E13C_07_cor_df <- as.data.frame(E13C07base_cor_rest)

# Add a column for the variable names
E13C_07_cor_df$variable <- rownames(E13C_07_cor_df)

# Reshape the data frame to long format for plotting
E13C_07_cor_df_long <- pivot_longer(E13C_07_cor_df, -variable, names_to = "section", values_to = "e13c07_correlation")

#Extract the category from variable2
E13C_07_cor_df_long$category <- ifelse(grepl("^E13CBetn07[oprq]", E13C_07_cor_df_long$section), 
                                       "below_ground", "above_ground")

##DF's ####
#that contains all three individuals
base_e13c_indv <- rbind(
  data.frame(x="E13C_01", category = E13C_01_cor_df_long$category, 
             correlation = E13C_01_cor_df_long$e13c01_correlation),
  data.frame(x="E13C_03", category = E13C_03_cor_df_long$category, 
             correlation = E13C_03_cor_df_long$e13c03_correlation),
  data.frame(x="E13C_07", category = E13C_07_cor_df_long$category,
             correlation = E13C_07_cor_df_long$e13c07_correlation)
  )

#create a df to unify all individuals
base_e13c_merge <- base_e13c_indv

#add a shape column that will be used to separate from individuals in the boxplot for all individuals
base_e13c_merge <- base_e13c_merge %>%
  mutate(shape = case_when(
    x == "E13C_01" ~ "16",
    x == "E13C_03" ~ "15",
    x == "E13C_07" ~ "8",
    TRUE ~ NA_character_
  ))

base_e13c_indv$shape <- "16"

##plot E13C####
#Create a df that gathers all the information of the three individuals
base_e13c <- rbind(
  data.frame(x=base_e13c_indv$x, correlation=base_e13c_indv$correlation, category = base_e13c_indv$category, shape = base_e13c_indv$shape),
  data.frame(x="E13C", correlation=base_e13c_merge$correlation, category = base_e13c_merge$category, shape = base_e13c_merge$shape)
)

#plot the df in boxplots

ggplot(base_e13c, aes(x=x, y=correlation, color = category)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1) +
  scale_color_manual(values = category_palette) +
  labs(x = "Individuals (E13C)", y = "Base correlation to body parts") +
  theme_minimal() 

ggsave("figures/rings/base_correlation/base_to_e13c.jpg", width = 7, height = 4)

#E13D####
##E13D_01####
e13d_bet_n_01_average <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01_average.rwl")
E13D01base_cor_rest <- cor(e13d_bet_n_01_average$E13DBetn01s01, e13d_bet_n_01_average, 
                           use = "pairwise.complete.obs", method = "spearman")

#Check correlations
mean(E13D01base_cor_rest)

# Convert correlation matrix to a data frame
E13D_01_cor_df <- as.data.frame(E13D01base_cor_rest)

# Add a column for the variable names
E13D_01_cor_df$variable <- rownames(E13D_01_cor_df)

# Reshape the data frame to long format for plotting
E13D_01_cor_df_long <- pivot_longer(E13D_01_cor_df, -variable, names_to = "section", values_to = "e13d01_correlation")

#Extract the category from variable2
E13D_01_cor_df_long$category <- ifelse(grepl("^E13DBetn01[opqr]", E13D_01_cor_df_long$section), 
                                       "below_ground", "above_ground")
##E13D_02####
e13d_bet_n_02_average <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_02_average.rwl")
E13D02base_cor_rest <- cor(e13d_bet_n_02_average$E13DBetn02s01, e13d_bet_n_02_average, 
                           use = "pairwise.complete.obs", method = "spearman")
#Check correlations
mean(E13D02base_cor_rest)

# Convert correlation matrix to a data frame
E13D_02_cor_df <- as.data.frame(E13D02base_cor_rest)

# Add a column for the variable names
E13D_02_cor_df$variable <- rownames(E13D_02_cor_df)

# Reshape the data frame to long format for plotting
E13D_02_cor_df_long <- pivot_longer(E13D_02_cor_df, -variable, names_to = "section", values_to = "e13d02_correlation")

#Extract the category from variable2
E13D_02_cor_df_long$category <- ifelse(grepl("^E13DBetn02[oprq]", E13D_02_cor_df_long$section), 
                                       "below_ground", "above_ground")

##E13C_03####
e13d_bet_n_03_average <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_03_average.rwl")
E13D03base_cor_rest <- cor(e13d_bet_n_03_average$E13DBetn03s01, e13d_bet_n_03_average, 
                           use = "pairwise.complete.obs", method = "spearman")
#Check correlations
mean(E13D03base_cor_rest, na.rm = T)

# Convert correlation matrix to a data frame
E13D_03_cor_df <- as.data.frame(E13D03base_cor_rest)

# Add a column for the variable names
E13D_03_cor_df$variable <- rownames(E13D_03_cor_df)

# Reshape the data frame to long format for plotting
E13D_03_cor_df_long <- pivot_longer(E13D_03_cor_df, -variable, names_to = "section", values_to = "e13d03_correlation")

#Extract the category from variable2
E13D_03_cor_df_long$category <- ifelse(grepl("^E13DBetn03[oprq]", E13D_03_cor_df_long$section), 
                                       "below_ground", "above_ground")

##DF's ####
#that contains all three individuals
base_e13d_indv <- rbind(
  data.frame(x="E13D_01", category = E13D_01_cor_df_long$category, 
             correlation = E13D_01_cor_df_long$e13d01_correlation, category = E13D_01_cor_df_long$category),
  data.frame(x="E13D_02", category = E13D_02_cor_df_long$category, 
             correlation = E13D_02_cor_df_long$e13d02_correlation, category = E13D_02_cor_df_long$category),
  data.frame(x="E13D_03", category = E13D_03_cor_df_long$category,
             correlation = E13D_03_cor_df_long$e13d03_correlation, category = E13D_03_cor_df_long$category)
)

#create a df to unify all individuals
base_e13d_merge <- base_e13d_indv

#add a shape column that will be used to separate from individuals in the boxplot for all individuals
base_e13d_merge <- base_e13d_merge %>%
  mutate(shape = case_when(
    x == "E13D_01" ~ "16",
    x == "E13D_02" ~ "15",
    x == "E13D_03" ~ "8",
    TRUE ~ NA_character_
    ))

base_e13d_indv$shape <- "16"

##plot E13D####
#Create a df that gathers all the information of the three individuals
base_e13d <- rbind(
  data.frame(x=base_e13d_indv$x, correlation=base_e13d_indv$correlation, category = base_e13d_indv$category, shape = base_e13d_indv$shape),
  data.frame(x="E13D", correlation=base_e13d_merge$correlation, category = base_e13d_merge$category, shape = base_e13d_merge$shape)
  )

#plot the df in boxplots
ggplot(base_e13d, aes(x=x, y=correlation, color = category)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1) +
  scale_color_manual(values = category_palette) +
  labs(x = "Individuals (E13C)", y = "Base correlation to body parts") +
  theme_minimal() 

ggsave("figures/rings/base_correlation/base_to_e13d.jpg", width = 7, height = 4)

#E13V####
##E13V_01####
e13v_bet_n_01_average <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01_average.rwl")
E13V01base_cor_rest <- cor(e13v_bet_n_01_average$E13VBetn01s01, e13v_bet_n_01_average, 
                           use = "pairwise.complete.obs", method = "spearman")

#Check correlations
mean(E13V01base_cor_rest)

# Convert correlation matrix to a data frame
E13V_01_cor_df <- as.data.frame(E13V01base_cor_rest)

# Add a column for the variable names
E13V_01_cor_df$variable <- rownames(E13V_01_cor_df)

# Reshape the data frame to long format for plotting
E13V_01_cor_df_long <- pivot_longer(E13V_01_cor_df, -variable, names_to = "section", values_to = "e13v01_correlation")

#Extract the category from variable2
E13V_01_cor_df_long$category <- ifelse(grepl("^E13VBetn01[opqr]", E13V_01_cor_df_long$section), 
                                       "below_ground", "above_ground")
##E13V_02####
e13v_bet_n_02_average <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_02_average.rwl")
E13V02base_cor_rest <- cor(e13v_bet_n_02_average$E13VBetn02s01, e13v_bet_n_02_average, 
                           use = "pairwise.complete.obs", method = "spearman")

#Check correlations
mean(E13V02base_cor_rest,na.rm = T)

# Convert correlation matrix to a data frame
E13V_02_cor_df <- as.data.frame(E13V02base_cor_rest)

# Add a column for the variable names
E13V_02_cor_df$variable <- rownames(E13V_02_cor_df)

# Reshape the data frame to long format for plotting
E13V_02_cor_df_long <- pivot_longer(E13V_02_cor_df, -variable, names_to = "section", values_to = "e13v02_correlation")

#Extract the category from variable2
E13V_02_cor_df_long$category <- ifelse(grepl("^E13VBetn02[oprq]", E13V_02_cor_df_long$section), 
                                       "below_ground", "above_ground")

##E13V_03####
e13v_bet_n_03_average <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_03_average.rwl")
E13V03base_cor_rest <- cor(e13v_bet_n_03_average$E13VBetn03s01, e13v_bet_n_03_average, 
                           use = "pairwise.complete.obs", method = "spearman")
#Check correlations
mean(E13V03base_cor_rest, na.rm = T)

# Convert correlation matrix to a data frame
E13V_03_cor_df <- as.data.frame(E13V03base_cor_rest)

# Add a column for the variable names
E13V_03_cor_df$variable <- rownames(E13V_03_cor_df)

# Reshape the data frame to long format for plotting
E13V_03_cor_df_long <- pivot_longer(E13V_03_cor_df, -variable, names_to = "section", values_to = "e13v03_correlation")

#Extract the category from variable2
E13V_03_cor_df_long$category <- ifelse(grepl("^E13VBetn03[oprq]", E13V_03_cor_df_long$section), 
                                       "below_ground", "above_ground")

##DF's ####
#that contains all three individuals
base_e13v_indv <- rbind(
  data.frame(x="E13V_01", category = E13V_01_cor_df_long$category, 
             correlation = E13V_01_cor_df_long$e13v01_correlation, category = E13V_01_cor_df_long$category),
  data.frame(x="E13V_02", category = E13V_02_cor_df_long$category, 
             correlation = E13V_02_cor_df_long$e13v02_correlation, category = E13V_02_cor_df_long$category),
  data.frame(x="E13V_03", category = E13V_03_cor_df_long$category,
             correlation =E13V_03_cor_df_long$e13v03_correlation, category = E13V_03_cor_df_long$category)
)

#create a df to unify all individuals
base_e13v_merge <- base_e13v_indv

#add a shape column that will be used to separate from individuals in the boxplot for all individuals
base_e13v_merge <- base_e13v_merge %>%
  mutate(shape = case_when(
    x == "E13V_01" ~ "16",
    x == "E13V_02" ~ "15",
    x == "E13V_03" ~ "8",
    TRUE ~ NA_character_
  ))

base_e13v_indv$shape <- "16"

##plot E13V####
#Create a df that gathers all the information of the three individuals
base_e13v <- rbind(
  data.frame(x=base_e13v_indv$x, correlation=base_e13v_indv$correlation, category = base_e13v_indv$category, shape = base_e13v_indv$shape),
  data.frame(x="E13V", correlation=base_e13v_merge$correlation, category = base_e13v_merge$category, shape = base_e13v_merge$shape)
)

#plot the df in boxplots
ggplot(base_e13v, aes(x=x, y=correlation, color = category)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1) +
  scale_color_manual(values = category_palette) +
  labs(x = "Individuals (E13V)", y = "Base correlation to body parts") +
  theme_minimal() 
ggsave("figures/rings/base_correlation/base_to_e13v.jpg", width = 7, height = 4)

#All areas####

#per area
base_e13_site <- rbind(
  data.frame(x="e13a", correlation=base_e13a$correlation, category = base_e13a$category, shape = base_e13a$shape),
  data.frame(x="e13c", correlation=base_e13c$correlation, category = base_e13c$category, shape = base_e13c$shape),
  data.frame(x="e13d", correlation=base_e13d$correlation, category = base_e13d$category, shape = base_e13d$shape),
  data.frame(x="e13v", correlation=base_e13v$correlation, category = base_e13v$category, shape = base_e13v$shape)
  )

#plot the df in boxplots
ggplot(base_e13_site, aes(x=x, y=correlation, color = category)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha = 0.6) +
  scale_color_manual(values = category_palette) +
  labs(x = "Sites", y = "Base correlation to body parts") +
  theme_minimal() 

ggsave("figures/rings/base_correlation/base_to_sites.jpg", width = 7, height = 4)

#individual per area
base_e13_all <- rbind(
  data.frame(x=base_e13a$x, correlation=base_e13a$correlation, category = base_e13a$category, shape = base_e13a$shape),
  data.frame(x=base_e13c$x, correlation=base_e13c$correlation, category = base_e13c$category, shape = base_e13c$shape),
  data.frame(x=base_e13d$x, correlation=base_e13d$correlation, category = base_e13d$category, shape = base_e13d$shape),
  data.frame(x=base_e13v$x, correlation=base_e13v$correlation, category = base_e13v$category, shape = base_e13v$shape)
)

#plot the df in boxplots
ggplot(base_e13_all, aes(x=x, y=correlation, color = category)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1) +
  scale_color_manual(values = category_palette) +
  labs(x = "Sites", y = "Base correlation to body parts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("figures/rings/base_correlation/base_to_sites_individuals.jpg", width = 7, height = 4)


#Analysis of above and below correlations####
##E13A####
#separate the data in above and below ground and include the base in both DFs
# Select columns for below the ground and the base of the stem
e13a_bet_n_01_average_below <- base_e13a[base_e13a$category == "below_ground", ]
mean(e13a_bet_n_01_average_below$correlation)

# Select columns starting for above the ground and the base
e13a_bet_n_01_average_above <- base_e13a[base_e13a$category == "above_ground", ]

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
ggplot(base_e13a, aes(x=category, y=correlation, color = x)) +
  geom_boxplot(width=0.2, fill = "white", color = "black") +
  geom_jitter(width=0.09, alpha = 0.5) +
  #scale_color_manual(values = site_palette) +
  #scale_shape_manual(values = as.factor(base_e13a$shape))+
  labs(x = "Position from ground", y = "Base correlation to body parts", color = "Individual") +
  theme_minimal()

ggsave("figures/rings/base_correlation/base_to_positon_e13a.jpg", width = 7, height = 4)

#E13c
ggplot(base_e13c, aes(x=category, y=correlation, color = x)) +
  geom_boxplot(width=0.2, fill = "white", color = "black") +
  geom_jitter(width=0.09, alpha = 0.5) +
  #scale_color_manual(values = category_palette) +
  #scale_shape_manual(values = as.factor(base_e13a$shape))+
  labs(x = "Individuals (E13A)", y = "Base correlation to body parts", color = "Individual") +
  theme_minimal()

ggsave("figures/rings/base_correlation/base_to_positon_e13c.jpg", width = 7, height = 4)

#E13d
ggplot(base_e13d, aes(x=category, y=correlation, color = x)) +
  geom_boxplot(width=0.2, fill = "white", color = "black") +
  geom_jitter(width=0.09, alpha = 0.5) +
  #scale_color_manual(values = category_palette) +
  #scale_shape_manual(values = as.factor(base_e13a$shape))+
  labs(x = "Individuals (E13A)", y = "Base correlation to body parts", color = "Individual") +
  theme_minimal()

ggsave("figures/rings/base_correlation/base_to_positon_e13d.jpg", width = 7, height = 4)

#E13v
ggplot(base_e13v, aes(x=category, y=correlation, color = x)) +
  geom_boxplot(width=0.2, fill = "white", color = "black") +
  geom_jitter(width=0.09, alpha = 0.5) +
  #scale_color_manual(values = category_palette) +
  #scale_shape_manual(values = as.factor(base_e13a$shape))+
  labs(x = "Individuals (E13A)", y = "Base correlation to body parts", color = "Individual") +
  theme_minimal()

ggsave("figures/rings/base_correlation/base_to_positon_e13v.jpg", width = 7, height = 4)

#all sites
#category (all sites comparing above vs below)
ggplot(base_e13_site, aes(x=category, y=correlation, color = x)) +
  geom_boxplot(width=0.25, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha = 0.6) +
  scale_color_manual(values = site_palette) +
  #scale_shape_manual(values = as.factor(base_e13a$shape))+
  labs(x = "Position from ground", y = "Base correlation to body parts", color = "Individual") +
  theme_minimal()

ggsave("figures/rings/base_correlation/base_to_positon.jpg", width = 7, height = 4)

#above

ggplot(base_e13_site[base_e13_site$category == "above_ground", ], 
       aes(x=x, y=correlation, color = x)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha= 0.5) +
  scale_color_manual(values = site_palette) +
  labs(x = "Sites", y = "Base correlation to body parts above ground", color = "Sites") +
  theme_minimal() 

ggsave("figures/rings/base_correlation/base_to_positon_above.jpg", width = 7, height = 4)

#below

ggplot(base_e13_site[base_e13_site$category == "below_ground", ], 
       aes(x=x, y=correlation, color = x)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha= 0.5) +
  scale_color_manual(values = site_palette) +
  labs(x = "Sites", y = "Base correlation to body parts below ground", color = "Sites") +
  theme_minimal() 

ggsave("figures/rings/base_correlation/base_to_positon_below.jpg", width = 7, height = 4)

#all above
ggplot(base_e13_all[base_e13_site$category == "above_ground", ],
       aes(x=x, y=correlation, color = x)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha= 0.5) +
  scale_color_manual(values = site_palette) +
  labs(x = "Sites", y = "Base correlation to body parts above ground", color = "Sites") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("figures/rings/base_correlation/base_to_positon_above_individuals.jpg", width = 7, height = 4)

#all below
ggplot(base_e13_all[base_e13_site$category == "below_ground", ],
       aes(x=x, y=correlation, color = x)) +
  geom_boxplot(width=.2, fill = "white", color = "black") +
  geom_jitter(width=0.1, alpha= 0.5) +
  scale_color_manual(values = site_palette) +
  labs(x = "Sites", y = "Base correlation to body parts below ground", color = "Sites") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("figures/rings/base_correlation/base_to_positon_below_individuals.jpg", width = 7, height = 4)
