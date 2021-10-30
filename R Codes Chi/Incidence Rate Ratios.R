# ------------------------------- #
# Analysis for the hotspots study #
# ------------------------------- #

library(metafor)
library(foreign)
library(readxl)

setwd("~/Google Drive/Unimelb/Masters/Data Science Project /Meta Analysis/Meta Analysis R Code")

# hotspots <- read.dta("hotspots_R.dta")
hotspots = read_excel("ExtractedData.xlsx")

## Restricting access (figure 3 in Pirkis)
restrict.1 <- subset(hotspots, StudyID %in% c(1, 2, 5, 7, 8, 9, 11, 13, 14, 15, 16, 36,37))
restrict.2 <- subset(hotspots, StudyID %in% c(1, 5, 7, 8, 9, 13, 14, 15, 16, 17, 18))

mod.1a <- rma.glmm(x1i = e1, x2i = e0, t1i = t1, t2i = t0,
                  measure = "IRR", model = "CM.EL", data = restrict.1)  # k = 13 

mod.1b <- rma.glmm(x1i = e1, x2i = e0, t1i = t1, t2i = t0,
                  measure = "IRR", model = "CM.EL", data = restrict.2)  # k = 11

mod.1a
predict(mod.1a, trans = exp)
2*pnorm(-abs(-4.4032)) # z-val from mod.1a (a random effects model)

mod.1b
predict(mod.1b, trans = exp)
2*pnorm(-abs(-5.0956)) # z-val from mod.1b (a random effects model)

postscript(file = "Figures/Restrictions.eps", height = 6, width = 7) # Didn't get this file from Sandy, probably just a title though 
forest(mod.1a, atransf = exp, slab = restrict.1$restrictlabel,
	cex = 0.7, xlim = c(-18, 8), ylim = c(-5, 13), rows = 10, addfit = FALSE, digits = 7)

	text(-17.5, 13, "Study", pos = 4, offset = 0, cex = 0.7)
	text(6, 13, "IRR [95% CI]", cex = 0.7)
	abline(h = -2.8)
	text(-17.5, -4.5, "Intervention delivered in isolation *",
		pos = 4, offset = 0,  cex = 0.7)
	text(-17.5, -3.5, "Intervention delivered in isolation or in \ncombination with other invention(s)",
		pos = 4, offset = 0,  cex = 0.7)
	addpoly(x = -2.7117, ci.lb = -3.7547, ci.ub = -1.6687, atransf = exp,
		rows = -4.5, cex = 0.7)
	addpoly(x = -2.3884, ci.lb = -3.4516, ci.ub = -1.3253, atransf = exp,
		rows = -3.5, cex = 0.7)
dev.off()



## Encouraging helpseeking (with Stack) (figure 4a in Pirkis)
helpseek.1 <- subset(hotspots, StudyID %in% c(4, 6, 10, 11, 19, 20))
helpseek.2 <- subset(hotspots, StudyID %in% c(4, 6, 19))

mod.2a <- rma.glmm(x1i = e1, x2i = e0, t1i = t1, t2i = t0,
                  measure = "IRR", model = "CM.EL", data = helpseek.1)

mod.2b <- rma.glmm(x1i = e1, x2i = e0, t1i = t1, t2i = t0,
                  measure = "IRR", model = "CM.EL", data = helpseek.2)

mod.2a
predict(mod.2a, trans = exp)

mod.2b
predict(mod.2b, trans = exp)

postscript(file = "Figures/Helpseeking-Stack.eps", height = 5, width = 7)
	forest(mod.2a, atransf = exp, slab = helpseek.1$helpseekinglabel,
		cex = 0.7, xlim = c(-18, 8), ylim = c(-5, 7), rows = 4 , addfit = FALSE)
	abline(h = -2)
	text(-17.5, 5.5, "Study", pos = 4, offset = 0, cex = 0.7)
	text(6, 5.5, "IRR [95% CI]", cex = 0.7)
	text(-17.5, -4, "Intervention delivered in isolation *", pos = 4, offset = 0,  cex = 0.7)
	text(-17.5, -3, "Intervention delivered in isolation or in \ncombination with other invention(s)",
		pos = 4, offset = 0,  cex = 0.7)
	addpoly(x = -0.2355, ci.lb = -1.2923, ci.ub = 0.8213, atransf = exp, rows = -4, cex = 0.7)
	addpoly(x = -0.4445, ci.lb = -1.1175, ci.ub = 0.2285, atransf = exp, rows = -3, cex = 0.7)
dev.off()


## Encouraging helpseeking (without Stack) (figure 4b in Pirkis)
helpseek.3 <- subset(hotspots, StudyID %in% c(4, 6, 10, 11, 20))
helpseek.4 <- subset(hotspots, StudyID %in% c(4, 6))

mod.2c <- rma.glmm(x1i = e1, x2i = e0, t1i = t1, t2i = t0,
                  measure = "IRR", model = "CM.EL", data = helpseek.3)

mod.2d <- rma.glmm(x1i = e1, x2i = e0, t1i = t1, t2i = t0,
                  measure = "IRR", model = "CM.EL", data = helpseek.4)

mod.2c
predict(mod.2c, trans = exp)

mod.2d
predict(mod.2d, trans = exp)

postscript(file = "Figures/Helpseeking.eps", height = 5, width = 7)
	forest(mod.2c, atransf = exp, slab = helpseek.3$helpseekinglabel,
		cex = 0.7, xlim = c(-18, 8), ylim = c(-4, 7), rows = 4 , addfit = FALSE)
	abline(h = -1)
	text(-17.5, 5.5, "Study", pos = 4, offset = 0, cex = 0.7)
	text(6, 5.5, "IRR [95% CI]", cex = 0.7)
	text(-17.5, -3, "Intervention delivered in isolation *", pos = 4, offset = 0,  cex = 0.7)
	text(-17.5, -2, "Intervention delivered in isolation or in \ncombination with other invention(s)",
		pos = 4, offset = 0,  cex = 0.7)
	addpoly(x = -0.9504, ci.lb = -1.6745, ci.ub = -0.2263, atransf = exp, rows = -3, cex = 0.7)
	addpoly(x = -0.7141, ci.lb = -1.2470, ci.ub = -0.1813, atransf = exp, rows = -2, cex = 0.7)
dev.off()



## Increased likelihood of a third party intervening (figure 5 in Pirkis)
likelihood.1 <- subset(hotspots, StudyID %in% c(2, 10, 11, 20))

mod.3a <- rma.glmm(x1i = e1, x2i = e0, t1i = t1, t2i = t0,
                  measure = "IRR", model = "CM.EL", data = likelihood.1)

mod.3a
predict(mod.3a, trans = exp)


postscript(file = "Figures/Likelihood.eps", height = 5, width = 7)
	forest(mod.3a, atransf = exp, slab = likelihood.1$likelihoodlabel,
		cex = 0.7, xlim = c(-18, 8), ylim = c(-2, 7), rows = 4, addfit = FALSE)
	abline(h = 0)
	text(-17.5, 6, "Study", pos = 4, offset = 0, cex = 0.7)
	text(6, 6, "IRR [95% CI]", cex = 0.7)
	text(-17.5, -2, "Intervention delivered in isolation *", pos = 4, offset = 0,  cex = 0.7)
	text(-17.5, -1, "Intervention delivered in isolation or in \ncombination with other invention(s)",
		pos = 4, offset = 0,  cex = 0.7)
	addpoly(x = -0.6393, ci.lb = -1.1570, ci.ub = -0.1216, atransf = exp, rows = -1, cex = 0.7)
	text(4, -2, "N/A", pos = 4, offset = 0, cex = 0.7)
dev.off()














