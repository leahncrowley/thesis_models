# Leah N. Crowley
# Models!

#### Set-up ####

# Read in relevant packages:  
  library(lubridate)
  library(dplyr)
  library(devtools)
  library(ggplot2)
  library(ggpubr)
  library(tidyr)
  library(patchwork)
  library(MuMIn)
  library(lme4)

# Read in relevant data: 
  daily   <- read.csv("C:/Users/lcrowley1/Dropbox/Haase Bat Lab/Leah's Folder/Dataframes/daily_data.csv")
  monthly <- read.csv("C:/Users/lcrowley1/Dropbox/Haase Bat Lab/Leah's Folder/Dataframes/monthly_data.csv")  
  sites   <- read.csv("C:/Users/lcrowley1/Dropbox/Haase Bat Lab/Leah's Folder/Dataframes/SITE_DATA.csv")
  
#### 1. Compare MCs between sites ####

# DAILY DATA:
  summary(lme(temp.mean.d  ~ scale(Latitude) + scale(Elevation) + Site.Type, random = ~ 1|Month/Site, data = daily))
  summary(lme(temp.min.d   ~ scale(Latitude) + scale(Elevation) + Site.Type, random = ~ 1|Month/Site, data = daily))
  summary(lme(temp.max.d   ~ scale(Latitude) + scale(Elevation) + Site.Type, random = ~ 1|Month/Site, data = daily))
  summary(lme(temp.range.d ~ scale(Latitude) + scale(Elevation) + Site.Type, random = ~ 1|Month/Site, data = daily))
  summary(lme(temp.var.d   ~ scale(Latitude) + scale(Elevation) + Site.Type, random = ~ 1|Month/Site, data = daily))
    
  summary(lme(dwvp.mean.d  ~ scale(Latitude) + scale(Elevation) + Site.Type, random = ~ 1|Month/Site, data = daily))
  summary(lme(dwvp.min.d   ~ scale(Latitude) + scale(Elevation) + Site.Type, random = ~ 1|Month/Site, data = daily))
  summary(lme(dwvp.max.d   ~ scale(Latitude) + scale(Elevation) + Site.Type, random = ~ 1|Month/Site, data = daily))
  summary(lme(dwvp.range.d ~ scale(Latitude) + scale(Elevation) + Site.Type, random = ~ 1|Month/Site, data = daily))
  summary(lme(dwvp.var.d   ~ scale(Latitude) + scale(Elevation) + Site.Type, random = ~ 1|Month/Site, data = daily))

  summary(lme(wint.in.range ~ scale(Latitude) + scale(Elevation) + Site.Type, random = ~ 1|Month/Site, data = daily))
  summary(lme(wint.in.opt.range ~ scale(Latitude) + scale(Elevation) + Site.Type, random = ~ 1|Month/Site, data = daily))
    
# MONTHLY DATA:
  summary(lme(temp.mean.m  ~ Site.Type + scale(Latitude) + scale(Elevation), random = ~1 | Site, data = monthly))
  summary(lme(temp.min.m   ~ Site.Type + scale(Latitude) + scale(Elevation), random = ~1 | Site, data = monthly))
  summary(lme(temp.max.m   ~ Site.Type + scale(Latitude) + scale(Elevation), random = ~1 | Site, data = monthly))
  summary(lme(temp.range.m ~ Site.Type + scale(Latitude) + scale(Elevation), random = ~1 | Site, data = monthly))
  summary(lme(temp.var.m   ~ Site.Type + scale(Latitude) + scale(Elevation), random = ~1 | Site, data = monthly))
  
  summary(lme(dwvp.mean.m  ~ Site.Type + scale(Latitude) + scale(Elevation), random = ~1 | Site, data = monthly))
  summary(lme(dwvp.min.m   ~ Site.Type + scale(Latitude) + scale(Elevation), random = ~1 | Site, data = monthly))
  summary(lme(dwvp.max.m   ~ Site.Type + scale(Latitude) + scale(Elevation), random = ~1 | Site, data = monthly))
  summary(lme(dwvp.range.m ~ Site.Type + scale(Latitude) + scale(Elevation), random = ~1 | Site, data = monthly))
  summary(lme(dwvp.var.m   ~ Site.Type + scale(Latitude) + scale(Elevation), random = ~1 | Site, data = monthly))
  
  summary(lme(wint.in.range ~ Site.Type + scale(Latitude) + scale(Elevation), random = ~1 | Site, data = monthly))
  summary(lme(wint.in.opt.range ~ Site.Type + scale(Latitude) + scale(Elevation), random = ~1 | Site, data = monthly))
  
#### 2. Hypothesis ####
  
  # Build models:
    tempmod.1 <- glm(WNS.status ~ mean.daily.temp.mean + Latitude + Elevation, family =  binomial(link = "logit"), data = sites)
      summary(tempmod.1)
    tempmod.2 <- glm(WNS.status ~ mean.daily.temp.var + Latitude + Elevation, family =  binomial(link = "logit"), data = sites)
      summary(tempmod.2)
    tempmod.3 <- glm(WNS.status ~ mean.wint.temp + Latitude + Elevation, family =  binomial(link = "logit"), data = sites)
      summary(tempmod.3)
    tempmod.4 <- glm(WNS.status ~ var.wint.temp + Latitude + Elevation, family =  binomial(link = "logit"), data = sites)
      summary(tempmod.4)
  
    dwvpmod.1 <- glm(WNS.status ~ mean.daily.dwvp.mean + Latitude + Elevation, family =  binomial(link = "logit"), data = sites)
      summary(dwvpmod.1)
    dwvpmod.2 <- glm(WNS.status ~ mean.daily.dwvp.var + Latitude + Elevation, family =  binomial(link = "logit"), data = sites)
      summary(dwvpmod.2)
    dwvpmod.3 <- glm(WNS.status ~ mean.wint.dwvp + Latitude + Elevation, family =  binomial(link = "logit"), data = sites)
      summary(dwvpmod.3)
    dwvpmod.4 <- glm(WNS.status ~ var.wint.dwvp + Latitude + Elevation, family =  binomial(link = "logit"), data = sites)
      summary(dwvpmod.4)
  
  # Model selection:
    temp.sel <- model.sel(tempmod.1, tempmod.2, tempmod.3, tempmod.4)
    dwvp.sel <- model.sel(dwvpmod.1, dwvpmod.2, dwvpmod.3, dwvpmod.4)
    
    # They are... the same? Let's try a global model...
    # Global model: 
      global.temp <- glm(WNS.status ~ mean.daily.temp.mean + mean.daily.temp.var + mean.wint.temp + var.wint.temp + Latitude + Elevation + Winter.days + wint.in.range + wint.in.opt.range, family = binomial, data = sites, na.action = na.roughfix)
      global.temp.dredge <- dredge(global.temp) 
      results.h
      
      all.params.t <- glm(WNS.status ~ Temperature + Latitude + Elevation + Winter.days + wint.in.range + wint.in.opt.range, family = binomial, data = monthly, na.action = na.exclude)
      results.t <- dredge(all.params.t)
      results.t


