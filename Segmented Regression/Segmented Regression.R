#### author: Claudia Zapata (1124185)

#install.packages("BBmisc")
#install.packages("segmented")

library(BBmisc)
library(segmented)
library(ggplot2)
suicides = read.csv("TimeSeriesData.csv")

### General Function
suicides_seg_reg = function(year, suicides, installation = NULL, breakpoints, study_name = NULL) {
  results = list()
  n.int = length(breakpoints) 
  
  #Fitting Model (Segmented)
  lm = lm(suicides ~ year)
  seg = segmented(lm, seg.Z = ~ year, psi = list(year = breakpoints))
  
  results$summary = summary(seg)
  results$slopes = slope(seg)
  
  # Plot Data Frame
  obs = length(year)
  X = data.frame(Year = year, Deaths = suicides, Suicides = rep("Recorded", obs))
  estimates = data.frame(Year = year, Deaths = fitted(seg), Suicides = rep("Estimated Trend", obs))
  data = X
  
  #Fitted Model (Recorded Trend)
  limits = c(min(year), breakpoints, max(year))
  ind.trends = list()
  
  for (b in seq(1, length(breakpoints)+1)) {
    if (b == 1) { 
      b.data = subset(X, Year >= limits[b])
    } else { 
      b.data = subset(X, Year > limits[b]) }
    
    b.data = subset(b.data, Year <= limits[b+1])
    b.lm = lm(Deaths ~ Year, data = b.data)
    
    if (b > 1) {
      int = predict(b.lm, newdata = data.frame("Year" = limits[b], "Suicides" = "Recorded Trend"))
      data = rbind(data, data.frame(Year = limits[b], Deaths = int, Suicides = "Recorded Trend"))
    }
    
    ind.trends[[b]] = b.lm
    b.data = cbind(b.data, Estimates = fitted(b.lm))
    trend = data.frame(Year = b.data$Year, Deaths = b.data$Estimates, Suicides = rep("Recorded Trend", dim(b.data)[1]))
    data = rbind(data, trend)
  }
  
  data = rbind(data, estimates)

  #Plot
  if (length(installation) > 0) {
    min = c(installation[1] - 0.25, seg$psi[,2] - 0.05)
    max = c(installation[2] + 0.25, seg$psi[,2] + 0.05)
  } else {
    min = c(seg$psi[,1] - 0.5, seg$psi[,2] - 0.05)
    max = c(seg$psi[,1] + 0.5, seg$psi[,2] + 0.05)
  }
  
  bp = data.frame(breakpoints = c(seg$psi[,1], seg$psi[,2]), 
                  Intervention = c(rep("Implementation", n.int), rep("Estimated", n.int)),
                  min = min, max = max)
  bp = cbind(bp, id = 1:dim(bp)[1])
  
  p = ggplot(data) +
    geom_line(aes(x = Year, y = Deaths, group = Suicides, colour = Suicides, linetype = Suicides), size = 1.2) + 
    ggtitle(study_name) +
    geom_rect(data = bp, inherit.aes = FALSE, 
              aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf, 
                  group = id, fill = Intervention), 
              alpha = 0.3) + 
    scale_fill_manual(values = c("orange", "yellow")) +
    scale_x_continuous(breaks = seq(floor(min(year)), ceiling(max(year)), 1))
  
  results$plot = p
  return(results)
}

# ## Jumping From Height 
# ### Physical Barrier
# #### Bridge
# OCarroll_Ellington = suicides[suicides$Study == "O’Carroll (Ellington Bridge)", -1]
# OE = suicides_seg_reg(OCarroll_Ellington$Year, OCarroll_Ellington$Suicides, breakpoints = c(1986), study_name = "O’Carroll (Ellington Bridge): Jumping from Bridge - Physical Barrier")
# 
# OCarroll_Taft = suicides[suicides$Study == "O’Carroll (Taft Bridge)", -1]
# OT = suicides_seg_reg(OCarroll_Taft$Year, OCarroll_Taft$Suicides, breakpoints = c(1984), study_name = "O’Carroll (Taft Bridge): Jumping from Bridge - Physical Barrier")
# 
# #### Cliff
# Lockley = suicides[suicides$Study == "Lockley", -1]
# L = suicides_seg_reg(Lockley$Year, Lockley$Suicides, breakpoints = c(2008), study_name = "Lockley: Jumping from Cliff - Physical Barrier")
# 
# #### Terrace
# Reisch = suicides[suicides$Study == "Reisch", -1]
# R = suicides_seg_reg(Reisch$Year, Reisch$Suicides, breakpoints = c(1998), study_name = "Reisch: Jumping from Terrace - Physical Barrier")

## Railways
### Physical Barrier 
Law_Yip = suicides[suicides$Study == "Law & Yip", -1]
# LY = suicides_seg_reg(Law_Yip$Year, Law_Yip$Suicides, breakpoints = c(2002), study_name = "Law & Yip: Railway - Physical Barrier")
LY50 = suicides_seg_reg(Law_Yip$Year, Law_Yip$AvgPer50, breakpoints = c(2002), study_name = "Law & Yip: Railway - Physical Barrier (Avg. per 50 Stations)")

Chung = suicides[suicides$Study == "Chung", -1]
# C = suicides_seg_reg(Chung$Year, Chung$Suicides, installation = c(2005, 2010), breakpoints = c(2005), study_name = "Chung: Railway - Physical Barrier")
C50 = suicides_seg_reg(Chung$Year, Chung$AvgPer50, installation = c(2005, 2010), breakpoints = c(2005), study_name = "Chung: Railway - Physical Barrier (Avg. per 50 Stations)")

Ueda = suicides[suicides$Study == "Ueda", -1]
# U = suicides_seg_reg(Ueda$Year, Ueda$Suicides, installation = c(2004, 2013), breakpoints = c(2007), study_name = "Ueda: Railway - Physical Barrier")
U50 = suicides_seg_reg(Ueda$Year, Ueda$AvgPer50, installation = c(2004, 2013), breakpoints = c(2007), study_name = "Ueda: Railway - Physical Barrier (Avg. per 50 Stations)")

## Blue Lights
Matsubayashi = suicides[suicides$Study == "Matsubayashi", -1]
# M = suicides_seg_reg(Matsubayashi$Year, Matsubayashi$Suicides, installation = c(2008, 2010), breakpoints = c(2008), study_name = "Matsubayashi: Railway - Blue Lights")
M50 = suicides_seg_reg(Matsubayashi$Year, Matsubayashi$AvgPer50, installation = c(2008, 2010), breakpoints = c(2008), study_name = "Matsubayashi & Ueda: Railway - Blue Lights (Avg. per 50 Stations)")

Ichikawa = suicides[suicides$Study == "Ichikawa", -1]
# I = suicides_seg_reg(Ichikawa$Year, Ichikawa$Suicides, installation = c(2008, 2011), breakpoints = c(2008), study_name = "Ichikawa: Railway - Blue Lights")
I50 = suicides_seg_reg(Ichikawa$Year, Ichikawa$AvgPer50, installation = c(2008, 2011), breakpoints = c(2008), study_name = "Ichikawa & Inada: Railway - Blue Lights (Avg. per 50 Stations)")

