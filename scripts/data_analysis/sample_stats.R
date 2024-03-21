#load packages
library(dplR)

#Stats
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

#E13
e13_rbar <- rbind(data.frame(e13a_rbar),
                  data.frame(e13c_rbar),
                  data.frame(e13d_rbar),
                  data.frame(e13v_rbar)
                  )

view(e13_rbar)
