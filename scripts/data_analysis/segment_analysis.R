#Analize data by parts and compare it between them.
#load packages
library(ggplot2)
library(dplR)

#E13a####
##E13a1####
#first load data
e13a_bet_n_01 <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_01.rwl")

#Roots##
extract_a13a_01_roots <- grep("^E13ABetn01[o0o1p0p1q0q1r0r1]", names(e13a_bet_n_01), value = TRUE)
e13a_01_roots <- e13a_bet_n_01[, extract_a13a_01_roots]
colnames(e13a_01_roots)

head(e13a_01_roots)

#Stem##
extract_a13a_01_stem <- grep("^E13ABetn01[s0s1]", names(e13a_bet_n_01), value = TRUE)
e13a_01_stem <- e13a_bet_n_01[, extract_a13a_01_stem]
colnames(e13a_01_stem)

#Branches##
extract_a13a_01_branch <- grep("^E13ABetn01[t0t1u0u1v0v1w0w1]", names(e13a_bet_n_01), value = TRUE)
e13a_01_branch <- e13a_bet_n_01[, extract_a13a_01_branch]
colnames(e13a_01_branch)

#stats
e13a_01_base_rbar <- rwi.stats(e13a_bet_n_01_s01[,1:4])
e13a_01_rbar <- rwi.stats(e13a_bet_n_01, min.corr.overlap = 0)
e13a_01_roots_rbar <- rwi.stats(e13a_01_roots, min.corr.overlap = 0)
e13a_01_stem_rbar <- rwi.stats(e13a_01_stem, min.corr.overlap = 0)
e13a_01_branch_rbar <- rwi.stats(e13a_01_branch, min.corr.overlap = 0)

View(e13a_01_stem[27:47,])

print(e13a_01_rbar[,7])
print(e13a_01_base_rbar[,7])
print(e13a_01_roots_rbar[, 7])
print(e13a_01_stem_rbar[, 7])
print(e13a_01_branch_rbar[, 7])

##E13a2####
#first load data
e13a_bet_n_02 <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_02.rwl")

#Roots##
extract_a13a_02_roots <- grep("^E13ABetn02[o0o1p0p1q0q1r0r1]", names(e13a_bet_n_02), value = TRUE)
e13a_02_roots <- e13a_bet_n_02[, extract_a13a_02_roots]
colnames(e13a_02_roots)

#Stem##
extract_a13a_02_stem <- grep("^E13ABetn02[s0s1]", names(e13a_bet_n_02), value = TRUE)
e13a_02_stem <- e13a_bet_n_02[, extract_a13a_02_stem]
colnames(e13a_02_stem)

#Branches##
extract_a13a_02_branch <- grep("^E13ABetn02[t0t1u0u1v0v1w0w1]", names(e13a_bet_n_02), value = TRUE)
e13a_02_branch <- e13a_bet_n_02[, extract_a13a_02_branch]
colnames(e13a_02_branch)

#stats
e13a_02_base_rbar <- rwi.stats(e13a_bet_n_02_s01[,1:4])
e13a_02_rbar <- rwi.stats(e13a_bet_n_02)
e13a_02_roots_rbar <- rwi.stats(e13a_02_roots)
e13a_02_stem_rbar <- rwi.stats(e13a_02_stem)
e13a_02_branch_rbar <- rwi.stats(e13a_02_branch)

mean(e13a_02_rbar[,7])
mean(e13a_02_base_rbar[,7])
mean(e13a_02_roots_rbar[,7])
mean(e13a_02_stem_rbar[,7])
mean(e13a_02_branch_rbar[,7])

##E13a3####
#first load data
e13a_bet_n_03 <- read.rwl("data/ring_data/aligned/e13a/e13a.bet.n/e13a_bet_n_03.rwl")

#Roots##
extract_a13a_03_roots <- grep("^E13ABetn03[o0o1p0p1q0q1r0r1]", names(e13a_bet_n_03), value = TRUE)
e13a_03_roots <- e13a_bet_n_03[, extract_a13a_03_roots]
colnames(e13a_03_roots)

#Stem##
extract_a13a_03_stem <- grep("^E13ABetn03[s0s1]", names(e13a_bet_n_03), value = TRUE)
e13a_03_stem <- e13a_bet_n_03[, extract_a13a_03_stem]
colnames(e13a_03_stem)

#Branches##
extract_a13a_03_branch <- grep("^E13ABetn03[t0t1u0u1v0v1w0w1]", names(e13a_bet_n_03), value = TRUE)
e13a_03_branch <- e13a_bet_n_03[, extract_a13a_03_branch]
colnames(e13a_03_branch)

#stats
e13a_03_base_rbar <- rwi.stats(e13a_bet_n_03[,1:4])
e13a_03_rbar <- rwi.stats(e13a_bet_n_03)
e13a_03_roots_rbar <- rwi.stats(e13a_03_roots)
e13a_03_stem_rbar <- rwi.stats(e13a_03_stem)
e13a_03_branch_rbar <- rwi.stats(e13a_03_branch)

mean(e13a_03_rbar[,7])
mean(e13a_03_base_rbar[,7])
mean(e13a_03_roots_rbar[,7])
mean(e13a_03_stem_rbar[,7])
mean(e13a_03_branch_rbar[,7])

##E13a####
#first load data
e13a <- read.rwl("data/ring_data/aligned/e13a/e13a.rwl")

#Roots##
extract_a13a_roots <- grep("^E13ABetn0[123][o0o1p0p1q0q1r0r1]", names(e13a_filtered), value = TRUE)
e13a_roots <- e13a_filtered[, extract_a13a_roots]
colnames(e13a_roots)

#Stem##
extract_a13a_stem <- grep("^E13ABetn0[123][s0s1]", names(e13a_filtered), value = TRUE)
e13a_stem <- e13a_filtered[, extract_a13a_stem]
colnames(e13a_stem)

#Branches##
extract_a13a_branch <- grep("^E13ABetn0[123][t0t1u0u1v0v1w0w1]", names(e13a_filtered), value = TRUE)
e13a_branch <- e13a_filtered[, extract_a13a_branch]
colnames(e13a_branch)

#stats
e13a_base_rbar <- rwi.stats(e13a_filtered[,1:4])
e13a_rbar <- rwl.stats(e13a_filtered)
e13a_roots_rbar <- rwi.stats(e13a_roots)
e13a_stem_rbar <- rwi.stats(e13a_stem)
e13a_branch_rbar <- rwi.stats(e13a_branch)

mean(e13a_rbar[,7])
mean(e13a_base_rbar[,7])
mean(e13a_roots_rbar[,7])
mean(e13a_stem_rbar[,7])
mean(e13a_branch_rbar[,7])

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
e13v_01_base_rbar <- rwi.stats(e13v_bet_n_01_s01[,1:4], min.corr.overlap = 0)
e13v_01_rbar <- rwi.stats(e13v_bet_n_01, min.corr.overlap = 0)
e13v_01_roots_rbar <- rwi.stats(e13v_01_roots, min.corr.overlap = 0)
e13v_01_stem_rbar <- rwi.stats(e13v_01_stem, min.corr.overlap = 0)
e13v_01_branch_rbar <- rwi.stats(e13v_01_branch, min.corr.overlap = 0)

print(e13v_01_base_rbar$rbar.tot)
print(e13v_01_rbar$rbar.tot)
print(e13v_01_roots_rbar$rbar.tot)
print(e13v_01_stem_rbar$rbar.tot)
print(e13v_01_branch_rbar$rbar.tot)

##ploting the data####

###first-order autocorrelation####
# Combine data frames
ar1 <- rbind(
  data.frame(x="e13v_01_roots", y=e13v_01_roots_stats$ar1),
  data.frame(x="e13v_01_stem", y=e13v_01_stem_stats$ar1),
  data.frame(x="e13v_01_branch", y=e13v_01_branch_stats$ar1),
  data.frame(x="e13v_01", y=e13v_bet_n_01_stats$ar1)
  )

# Plot####
ggplot(ar1, aes(x=x, y=y)) +
  geom_boxplot(width=.2) +
  geom_jitter(width=0.1) +
  labs(y=expression(phi[1]), x="") +
  theme_minimal() +
  facet_wrap(~x, scales="free_x")

###cofecha####
#binding
rbarcor <- rbind(
  data.frame(x="e13v_01_roots", y=e13a_roots_rbar$rbar.tot),
  data.frame(x="e13v_01_stem", y=e13a_01_stem_rbar$),
  data.frame(x="e13v_01_branch", y=e13v_01_branch_rbar$res.cor),
  data.frame(x="e13v_01", y=e13v_01_rbar$res.cor)
  )
ggplot(rbarcor, aes(x=x, y=y)) +
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
e13d_01_rbar <- rwi.stats(e13d_bet_n_01, method = "spearman")
e13d_01_roots_rbar <- rwi.stats(e13d_01_roots, method = "spearman")
e13d_01_stem_rbar <- rwi.stats(e13d_01_stem, method = "spearman")
e13d_01_branch_rbar <- rwi.stats(e13d_01_branch, method = "spearman")

mean(e13d_01_rbar[,7])
mean(e13d_01_roots_rbar[, 1])
mean(e13d_01_stem_rbar[, 1])
mean(e13d_01_branch_rbar[, 1])

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


#reports
print(e13a_report)
print(e13v_report)

spag.plot(e13v_bet_n_01_short, zfac = 0.03)
