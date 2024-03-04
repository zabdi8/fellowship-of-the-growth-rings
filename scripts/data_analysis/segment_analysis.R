#Analize data by parts and compare it between them.
#load packages
library(ggplot2)
library(dplR)

#E13v####
#first load data
e13v_bet_n_01 <- read.rwl("data/ring_data/aligned/e13v/e13v.bet.n/e13v_bet_n_01.rwl")

#Roots##
extract_a13v_01_roots <- grep("^E13VBetn01[o0o1p0p1q0q1r0r1]", names(e13v_bet_n_01), value = TRUE)
e13v_01_roots <- e13v_bet_n_01[, extract_a13v_01_roots]
colnames(e13v_01_roots)

#Stem##
extract_a13v_01_stem <- grep("^E13VBetn01[s0s1]", names(e13v_bet_n_01), value = TRUE)
e13v_01_stem <- e13v_bet_n_01[, extract_a13v_01_stem]
colnames(e13v_01_stem)

#Branches##
extract_a13v_01_branch <- grep("^E13VBetn01[t0t1u0u1v0v1w0w1]", names(e13v_bet_n_01), value = TRUE)
e13v_01_branch <- e13v_bet_n_01[, extract_a13v_01_branch]
colnames(e13v_01_branch)

#stats
e13v_01_base_inter <- interseries.cor(e13v_bet_n_01_s01[,1:4])
e13v_01_inter <- interseries.cor(e13v_bet_n_01)
e13v_01_roots_inter <- interseries.cor(e13v_01_roots)
e13v_01_stem_inter <- interseries.cor(e13v_01_stem)
e13v_01_branch_inter <- interseries.cor(e13v_01_branch)

mean(e13v_01_base_inter[,1])
mean(e13v_01_inter[,1])
mean(e13v_01_roots_inter[, 1])
mean(e13v_01_stem_inter[, 1])
mean(e13v_01_branch_inter[, 1])

##ploting the data####

###first-order autocorrelation####
# Combine data frames
ar1 <- rbind(
  data.frame(x="e13v_01_roots", y=e13v_01_roots_stats$ar1),
  data.frame(x="e13v_01_stem", y=e13v_01_stem_stats$ar1),
  data.frame(x="e13v_01_branch", y=e13v_01_branch_stats$ar1),
  data.frame(x="e13v_01", y=e13v_bet_n_01_stats$ar1)
  )

# Plot
ggplot(ar1, aes(x=x, y=y)) +
  geom_boxplot(width=.2) +
  geom_jitter(width=0.1) +
  labs(y=expression(phi[1]), x="") +
  theme_minimal() +
  facet_wrap(~x, scales="free_x")

###cofecha####
#binding
intercor <- rbind(
  data.frame(x="e13v_01_roots", y=e13v_01_roots_inter$res.cor),
  data.frame(x="e13v_01_stem", y=e13v_01_stem_inter$res.cor),
  data.frame(x="e13v_01_branch", y=e13v_01_branch_inter$res.cor),
  data.frame(x="e13v_01", y=e13v_01_inter$res.cor)
  )
ggplot(intercor, aes(x=x, y=y)) +
  geom_boxplot(width=.2) +
  geom_jitter(width=0.1) +
  labs(y=expression(phi[1]), x="") +
  theme_minimal() +
  facet_wrap(~x, scales="free_x")


#E13d####
#first load data
e13d_bet_n_01 <- read.rwl("data/ring_data/aligned/e13d/e13d.bet.n/e13d_bet_n_01.rwl")

#Roots##
extract_a13d_01_roots <- grep("o0|o1|p0|p1|q0|q1|r0|r1", names(e13d_bet_n_01), value = TRUE)
e13d_01_roots <- e13d_bet_n_01[, extract_a13v_01_roots]
colnames(e13d_01_roots)

#Stem##
extract_a13v_01_stem <- grep("s0", names(e13d_bet_n_01), value = TRUE)
e13d_01_stem <- e13d_bet_n_01[, extract_a13v_01_stem]
colnames(e13d_01_stem)

#Branches##
extract_a13v_01_branch <- grep("t0|u0|v0|w0", names(e13d_bet_n_01), value = TRUE)

e13d_01_branch <- e13d_bet_n_01[, extract_a13v_01_branch]

#stats
e13d_01_inter <- interseries.cor(e13d_bet_n_01, method = "spearman")
e13d_01_roots_inter <- interseries.cor(e13d_01_roots, method = "spearman")
e13d_01_stem_inter <- interseries.cor(e13d_01_stem, method = "spearman")
e13d_01_branch_inter <- interseries.cor(e13d_01_branch, method = "spearman")

mean(e13d_01_inter[,1])
mean(e13d_01_roots_inter[, 1])
mean(e13d_01_stem_inter[, 1])
mean(e13d_01_branch_inter[, 1])

# Combine data frames
ar1 <- rbind(
  data.frame(x="e13d_01_roots", y=e13d_01_roots_stats$ar1),
  data.frame(x="e13d_01_stem", y=e13d_01_stem_stats$ar1),
  data.frame(x="e13d_01_branch", y=e13d_01_branch_stats$ar1),
  data.frame(x="e13d_01", y=e13d_bet_n_01_stats$ar1)
  )

# Plot
ggplot(ar1, aes(x=x, y=y)) +
  geom_boxplot(width=.2) +
  geom_jitter(width=0.1) +
  labs(y=expression(phi[1]), x="") +
  theme_minimal() +
  facet_wrap(~x, scales="free_x")


test <- series.rwl.plot(e13d_bet_n_01, series = "E13DBetn01s01r2", 
                                   series.yrs = as.numeric(names(series)),
                                   seg.length = 2, bin.floor = 0, n=NULL,
                                   prewhiten = FALSE, biweight = FALSE, 
                                   floor.plus1 = FALSE)
