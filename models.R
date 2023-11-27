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
  
# For help, run:

# MONTHLY DATA:   
  # Build models:
    mod1 <- glm(WNS.status ~ Temperature + Latitude + Elevation, family =  binomial(link = "logit"), data = monthly)
    mod2 <- glm(WNS.status ~ dwvp + Latitude + Elevation, family =  binomial, data = monthly) 
  
  # Model selection:
    out.put <- model.sel(mod1, mod2)
    out.put
    # They are... the same? Let's try a global model...
    
  # Global model: 
    all.params.h <- glm(WNS.status ~ Relative.Humidity + Latitude + Elevation + Winter.days + wint.in.range + wint.in.opt.range, family = binomial, data = monthly, na.action = na.roughfix)
    results.h <- dredge(all.params.h)
    results.h
    
    all.params.t <- glm(WNS.status ~ Temperature + Latitude + Elevation + Winter.days + wint.in.range + wint.in.opt.range, family = binomial, data = monthly, na.action = na.exclude)
    results.t <- dredge(all.params.t)
    results.t


