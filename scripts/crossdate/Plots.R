#Analizing all the graphs
##spaghetti plots####
###e13a####
#full
pdf("figures/spaghetti_plots/e13a/e13a.pdf", width = 11, height = 8, paper = "a4r")
spag.plot(e13a_filtered, zfac = 0.09, cex = 3) # creates a spaghetti plot
title(main = "e13abetn", adj = 0.5, line = 6, font.main = 2, cex.main = 1.2) # add title
dev.off()  # close the PDF device

#Average
pdf("figures/spaghetti_plots/e13a/e13a_average.pdf", width = 11, height = 8, paper = "a4r")
spag.plot(e13a_ave_filtered, zfac = 0.03, cex = 3) # creates a spaghetti plot
title(main = "e13abetn (Average)", adj = 0.5, line = 6, font.main = 2, cex.main = 1.2) # add title
dev.off()  # close the PDF device

###e13c####
#full
pdf("figures/spaghetti_plots/e13c/e13c.pdf", width = 11, height = 8, paper = "a4r")
spag.plot(e13c_bet_n_short, zfac=0.02, cex = 0.3) #creates a spaghetti plot
title(main = "e13cbetn", adj = 0.5, line = 6, font.main = 2, cex.main = 1.2) #add title
dev.off()  # close the PDF device

#average
pdf("figures/spaghetti_plots/e13c/e13c_average.pdf", width = 11, height = 8, paper = "a4r")
spag.plot(e13c_bet_n_ave_short, zfac=0.02, cex = 0.3) #creates a spaghetti plot
title(main = "e13cbetn", adj = 0.5, line = 6, font.main = 2, cex.main = 1.2) #add title
dev.off()  # close the PDF device

###e13d####
#full
pdf("figures/spaghetti_plots/e13d/e13d.pdf", width = 11, height = 8, paper = "a4r")
spag.plot(e13d_short, zfac=0.05, cex = 0.3)+ #creates a spaghetti plot
title(main = "e13dbetn", adj = 0.5, line = 6, font.main = 2, cex.main = 1.2) #add title
dev.off()  # close the PDF device

#average
pdf("figures/spaghetti_plots/e13d/e13d_average.pdf", width = 11, height = 8, paper = "a4r")
spag.plot(e13d_ave_short, zfac=0.02, cex = 0.3)+ #creates a spaghetti plot
  title(main = "e13dbetn (average)", adj = 0.5, line = 6, font.main = 2, cex.main = 1.2) #add title
dev.off()  # close the PDF device

#e13v
#full
pdf("figures/spaghetti_plots/e13v/e13v.pdf", width = 11, height = 8, paper = "a4r")
spag.plot(e13v_short, zfac=0.05, cex = 0.3) #creates a spaghetti plot
title(main = "e13vbetn", adj = 0.5, line = 6, font.main = 2, cex.main = 1.2) #add title
dev.off()  # close the PDF device

#average
pdf("figures/spaghetti_plots/e13v/e13v_average.pdf", width = 11, height = 8, paper = "a4r")
spag.plot(e13v_ave_short, zfac=0.02, cex = 0.3) #creates a spaghetti plot
title(main = "e13vbetn", adj = 0.5, line = 6, font.main = 2, cex.main = 1.2) #add title
dev.off()  # close the PDF device
