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
  
# For help, run:
# ?lmer

# DAILY DATA:
  
  # Fixed effects:  WNS, Latitude, Elevation
  # Random effects: Month, Year
  # Response:       Temperature variables OR  dwvp variables OR %winter variables 
  # Terms:          WNS, Latitude, Elevation, Month, Year
  # Data:           daily 
  
    lmer(temp.mean.d  ~ WNS.status + Latitude + Elevation + (Month | Year), data = daily) 
    lmer(temp.min.d   ~ WNS.status + Latitude + Elevation + (Month | Year), data = daily) 
    lmer(temp.max.d   ~ WNS.status + Latitude + Elevation + (Month | Year), data = daily)
    lmer(temp.range.d ~ WNS.status + Latitude + Elevation + (Month | Year), data = daily)
    lmer(temp.var.d   ~ WNS.status + Latitude + Elevation + (Month | Year), data = daily)
    
    lmer(dwvp.mean.d  ~ WNS.status + Latitude + Elevation + (Month | Year), data = daily) 
    lmer(dwvp.min.d   ~ WNS.status + Latitude + Elevation + (Month | Year), data = daily)
    lmer(dwvp.max.d   ~ WNS.status + Latitude + Elevation + (Month | Year), data = daily) 
    lmer(dwvp.range.d ~ WNS.status + Latitude + Elevation + (Month | Year), data = daily)
    lmer(dwvp.var.d   ~ WNS.status + Latitude + Elevation + (Month | Year), data = daily)
    
    lmer(wint.in.range ~ WNS.status + Latitude + Elevation + (Month | Year), data = daily)
    lmer(wint.in.opt.range ~ WNS.status + Latitude + Elevation + (Month | Year), data = daily)
    
# MONTHLY DATA:
    
    # Fixed effects:  WNS, Latitude, Elevation
    # Random effects: Year
    # Response:       Temperature variables OR  dwvp variables OR %winter variables 
    # Terms:          WNS, Latitude, Elevation, Month, Year
    # Data:           monthly 
    
    lmer(temp.mean.m  ~ WNS.status + Latitude + Elevation + (1 | Year), data = monthly) 
    lmer(temp.min.m   ~ WNS.status + Latitude + Elevation + (1 | Year), data = monthly) 
    lmer(temp.max.m   ~ WNS.status + Latitude + Elevation + (1 | Year), data = monthly)
    lmer(temp.range.m ~ WNS.status + Latitude + Elevation + (1 | Year), data = monthly)
    lmer(temp.var.m   ~ WNS.status + Latitude + Elevation + (1 | Year), data = monthly)
    
    lmer(dwvp.mean.m  ~ WNS.status + Latitude + Elevation + (1 | Year), data = monthly) 
    lmer(dwvp.min.m   ~ WNS.status + Latitude + Elevation + (1 | Year), data = monthly)
    lmer(dwvp.max.m   ~ WNS.status + Latitude + Elevation + (1 | Year), data = monthly) 
    lmer(dwvp.range.m ~ WNS.status + Latitude + Elevation + (1 | Year), data = monthly)
    lmer(dwvp.var.m   ~ WNS.status + Latitude + Elevation + (1 | Year), data = monthly)
    
    lmer(wint.in.range ~ WNS.status + Latitude + Elevation + (1 | Year), data = monthly)
    lmer(wint.in.opt.range ~ WNS.status + Latitude + Elevation + (1 | Year), data = monthly)
    
    
#### 2. Hypothesis ####
  
# For help, run:
# ?glm   

# MONTHLY DATA:   
  # Build models:
    mod1 <- glm(WNS.status ~ Temperature + Latitude + Elevation, family =  binomial, data = monthly)
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


