#### INITIAL SETUP ####
# Packages and general setup: 
  library(lubridate)
  library(sf)
  library(dplyr)
  library(devtools)
  library(ggplot2)
  library(ggpubr)
  #library(batwintor)
  library(tidyr)
  library(anytime)
  library(raster)
  library(rgdal)
  library(sf)
  library(sp)
  library(maptools)
  library(rgeos)
  library(terra)
  library(readr)
  library(webshot2)
  library(tidyverse)
  library(gtsummary)
  library(gt)
  library(flextable)
  library(reshape)
  library(patchwork)
  library(MuMIn)
  library(lme4)
  library(nlme)
  library(PMCMRplus)
  library(DescTools)
  library(wesanderson) # finally some gorgeous color palettes to choose from :)
  library(rstatix)

# Reading in relevant dataframes: 
  allofit <- read.csv("C:/Users/lcrowley1/Dropbox/Haase Bat Lab/Leah's Folder/Dataframes/all_data.csv")
  tomatch <- read.csv("C:/Users/lcrowley1/Dropbox/Haase Bat Lab/Leah's Folder/Raw Microclimate Data/tomatch.csv")
    #allofit$IDN <- tomatch$IDN[match(interaction(allofit$Site.Type, allofit$State), interaction(tomatch$Site.Type, tomatch$State))]
    #write.csv(allofit, "C:/Users/lcrowley1/Dropbox/Haase Bat Lab/Leah's Folder/Dataframes//all_data.csv", row.names=FALSE)
  daily <- read.csv("C:/Users/lcrowley1/Dropbox/Haase Bat Lab/Leah's Folder/Dataframes/daily_data.csv")
  daily.summary <- read.csv("C:/Users/lcrowley1/Dropbox/Haase Bat Lab/Leah's Folder/Dataframes/daily.summary.csv")
  monthly <- read.csv("C:/Users/lcrowley1/Dropbox/Haase Bat Lab/Leah's Folder/Dataframes/monthly_data.csv")
  monthly.summary <- read.csv("C:/Users/lcrowley1/Dropbox/Haase Bat Lab/Leah's Folder/Dataframes/monthly.summary.csv")
    #daily$inrange <- allofit$wint.in.range[match(daily$Site, allofit$Site)]
    #daily$inoptrange <- allofit$wint.in.opt.range[match(daily$Site, allofit$Site)]
    #daily$inopthum <- allofit$wint.in.opt.hum[match(daily$Site, allofit$Site)]
    #daily.summary$IDN <- allofit$IDN[match(daily.summary$Site, allofit$Site)]
    #daily.summary$Assignment <- tomatch$assignment[match(daily.summary$IDN, tomatch$IDN)]
    #write.csv(daily.summary, "C:/Users/lcrowley1/Dropbox/Haase Bat Lab/Leah's Folder/Dataframes//daily.summary.csv", row.names=FALSE)
    #monthly.summary$IDN <- allofit$IDN[match(monthly.summary$Site, allofit$Site)]
    #monthly.summary$Assignment <- tomatch$assignment[match(monthly.summary$IDN, tomatch$IDN)]
    #write.csv(monthly.summary, "C:/Users/lcrowley1/Dropbox/Haase Bat Lab/Leah's Folder/Dataframes//monthly.summary.csv", row.names=FALSE)


#### MC ANOVAs ####
# Check for normality. If normal, use ANOVA. If non-normal, use Kruskal-Wallis. 
  ggdensity(monthly.summary$mean.temp.m, add = "mean", xlab = "temperature") #data do not look normal; let's run a test:
  shapiro.test(monthly.summary$mean.temp.m) #p-value is 0.007985; data are not normal
  
  ggdensity(monthly.summary$min.temp.m, xlab = "temperature") #data do not look normal; let's run a test:
  shapiro.test(monthly.summary$min.temp.m) #p-value is 5.713e-06; data are not normal
  
  ggdensity(monthly.summary$max.temp.m, xlab = "temperature") #data do not look normal; let's run a test:
  shapiro.test(monthly.summary$max.temp.m) #p-value is 0.006855; data are not normal 
  
  ggdensity(monthly.summary$mean.wvp.m, add = "mean", xlab = "wvp") 
  shapiro.test(monthly.summary$mean.wvp.m) #p-value is 0.1627; data are normal!
  
  ggdensity(monthly.summary$var.wvp.m, add = "mean", xlab = "wvp") #data do not look normal; let's run a test 
  shapiro.test(monthly.summary$var.wvp.m) #p-value is 2.169e-11; data are not normal 
  
  ggdensity(monthly.summary$mean.dwvp.m, add = "mean", xlab = "dwvp") #data do not look normal; let's run a test 
  shapiro.test(monthly.summary$mean.dwvp.m) #p-value is 5.082e-12; data are not normal
  
  ggdensity(monthly.summary$var.dwvp.m, add = "mean", xlab = "dwvp") #data do not look normal; let's run a test 
  shapiro.test(monthly.summary$var.dwvp.m) #p-value is 02.2e-16; data are not normal  


# One-way ANOVA (normal data) and Kruskal-Wallis tests (non-normal data), followed by Tukey's HSD (normal data) or Dunn's Test (non-normal data):
# Differences in mean daily mean temperature for every month by site:
  model.mean.temp <- monthly.summary %>% kruskal_test(mean.temp.m ~ Assignment)
  summary(model.mean.temp)
  DunnTest(model.mean.temp)

# Differences in mean daily minimum temperature for every month by site:   
  model.min.temp <- monthly.summary %>% kruskal.test(min.temp.m ~ Assignment)
  model.min.temp
  DunnTest(model.min.temp)

# Differences in mean daily maximum temperature for every month by site:   
  model.max.temp <- monthly.summary %>% kruskal.test(max.temp.m ~ Assignment)
  model.max.temp
  DunnTest(model.max.temp)

# Differences in mean daily mean wvp for every month by site:
  model.mean.wvp <- aov(mean.wvp.m ~ Assignment, data = monthly.summary)
  model.mean.wvp  
  TukeyHSD(model.mean.wvp)

# Differences in mean daily variance in wvp for every month by site: 
  model.var.wvp <- monthly.summary %>% kruskal_test(var.wvp.m ~ Assignment)
  summary(model.var.wvp)
  DunnTest(model.var.wvp)

# Differences in mean daily mean dwvp for every month by site:
  model.mean.dwvp <- monthly.summary %>% kruskal_test(mean.dwvp.m ~ Assignment)
  summary(model.mean.dwvp)
  DunnTest(model.mean.dwvp)

# Differences in mean daily variance in dwvp for every month by site: 
  model.var.dwvp <- monthly.summary %>% kruskal_test(var.dwvp.m ~ Assignment)
  summary(model.var.dwvp)
  DunnTest(model.var.dwvp)

#### MC GLMMs ####
# Check normality: 
  shapiro.test(monthly.summary$mean.temp.m) #non-normal - use glmer
  shapiro.test(monthly.summary$min.temp.m)  #non-normal - use glmer
  shapiro.test(monthly.summary$max.temp.m)  #non-normal - use glmer
  shapiro.test(monthly.summary$mean.wvp.m)  #normal - use lme
  shapiro.test(monthly.summary$var.wvp.m)   #non-normal - use glmer
  shapiro.test(monthly.summary$mean.dwvp.m) #non-normal - use glmer
  shapiro.test(monthly.summary$var.dwvp.m)  #non-normal - use glmer 

# Fitting generalized linear mixed effects models with site as random effects:
  glmm.mean.temp <- glmer(mean.temp.m ~ scale(Latitude.m) + Assignment + scale(Elevation.m) + Month + (1 | Site), data = monthly.summary, family = gaussian(link = "log"))
  summary(glmm.mean.temp)  
  glmm.min.temp  <- glmer(min.temp.m ~ scale(Latitude.m) + Assignment + scale(Elevation.m) + Month + (1 | Site), data = monthly.summary, family = gaussian(link = "log"))
  summary(glmm.min.temp)
  glmm.max.temp  <- glmer(max.temp.m ~ scale(Latitude.m) + Assignment + scale(Elevation.m) + Month + (1 | Site), data = monthly.summary, family = gaussian(link = "log"))
  summary(glmm.max.temp)
  glmm.mean.wvp  <- lme(mean.wvp.m ~ scale(Latitude.m) + Assignment + scale(Elevation.m) + Month, random = ~1|Site, data = monthly.summary)
  summary(glmm.mean.wvp)
  glmm.var.wvp   <- glmer(var.wvp.m ~ scale(Latitude.m) + Assignment + scale(Elevation.m) + Month + (1 | Site), data = monthly.summary, family = gaussian(link = "log"))
  summary(glmm.var.wvp)
  glmm.mean.dwvp <- glm(mean.wvp.m ~ Site, data = monthly.summary[monthly.summary$mean.wvp.m < 2,], family = gaussian(link = "inverse"))
  summary(glmm.mean.dwvp)
  glmm.var.dwvp  <- glmer(var.dwvp.m  ~ scale(Latitude.m) + Assignment + scale(Elevation.m) + Month + (1 | Site), data = monthly.summary, family = gaussian(link = "log"))
  summary(glmm.var.dwvp)

#### % WINTER ANOVAs ####
# Check normality for each of the three variables: 
  ggdensity(month.percentages$fancy.range, xlab = "winter in range: SITES")
  ggqqplot(month.percentages$fancy.range)
  shapiro.test(month.percentages$fancy.range) #p-value is 2.2e-16; data are not normal
  ggdensity(month.percentages$fancy.opt.range, xlab = "winter in opt range: SITES")
  ggqqplot(month.percentages$fancy.opt.range)
  shapiro.test(month.percentages$fancy.opt.range) #p-value is 2.2e-16; data are not normal
  ggdensity(month.percentages$fancy.hum, xlab = "winter in humidity: SITES")
  ggqqplot(month.percentages$fancy.hum)
  shapiro.test(month.percentages$fancy.hum) #p-value is 2.2e-16; data are not normal

# None are normal; will have to use Kruskal-Wallis tests for all variables.
# Differences in percent of month spent in Pd range by assignment:
  model.fancy.range <- kruskal.test(fancy.range ~ Assignment, data = month.percentages)
  model.fancy.range
  DunnTest(model.fancy.range.a1)
# Differences in percent of month spent in optimal Pd range by assignment:
  model.fancy.opt.range <- kruskal.test(fancy.range ~ Assignment, data = month.percentages)
  model.fancy.opt.range
  DunnTest(model.fancy.opt.range.a1)
# Differences in percent of month spent in high humidity by assignment:
  model.fancy.hum <- kruskal.test(fancy.hum ~ Assignment, data = month.percentages)
  model.fancy.hum
  DunnTest(model.fancy.hum)

#### % WINTER GLMMs ####
# Check normality (again... it's the same as above): 
  shapiro.test(month.percentages$fancy.range.a1) #non-normal - use glmer
  shapiro.test(month.percentages$fancy.opt.range.a1)  #non-normal - use glmer
  shapiro.test(month.percentages$fancy.hum.a1)  #non-normal - use glmer

# Use Gamma distribution with identity function:
  inrange.model <- glmer(fancy.range.a1 ~ scale(Latitude) + Assignment + scale(Elevation) + Month + (1|Site), data = month.percentages, family=Gamma(link = "identity")) 
  inoptrange.model <- glmer(fancy.opt.range.a1 ~ scale(Latitude) + Assignment + scale(Elevation) + Month + (1 | Site), data = month.percentages, family = Gamma(link = "identity"))
  inopthum.model <- glmer(fancy.hum.a1 ~ scale(Latitude) + Assignment + scale(Elevation) + Month + (1 | Site), data = month.percentages, family = Gamma(link = "identity"))
