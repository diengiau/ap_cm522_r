#PROBLEM 1: Run regression for 1 firm
  #Import dataset
library(tidyverse)
library(plyr)
library(dplyr)
library(haven)
library(data.table)
library(lubridate)
rm()

  #CRSP
CRSP = readRDS("C:/LINH/PROJECT RICHARD/TA/TA/sfz_mth.rds")
  #CRSP (10 years monthly data)
class(CRSP)
CRSP = CRSP %>%
  filter(MCALDT >= as.Date("2010-01-01") & MCALDT <= as.Date("2020-12-31"))
CRSP$YEAR  = year(CRSP$MCALDT)
CRSP$MONTH = month(CRSP$MCALDT)
  #Fama French database
    #Download and import FF 3 factors/ 5 factors model
library(readxl)
F_F = read_excel("C:/LINH/THESIS/F-F_Research_Data_Factors_CSV/F-F_Research_Data_Factors.xlsx")
    #Merge CRSP with F_F. Change type of month and year
F_F$Year  = as.numeric(F_F$Year)
F_F$Month = as.numeric(F_F$Month)
File_merged = merge(CRSP, F_F, 
   by.x = c('YEAR', 'MONTH'),
   by.y = c('Year', 'Month'))  
    #Run regression for 1 firm
File_merged$Market_RET = File_merged$`Mkt-RF`- File_merged$RF
test = File_merged[which(File_merged$KYPERMNO=="10001"), ]
reg = lm (MRET~ Market_RET, data = test)
summary(reg)

#Extension:  Run regression for all firms

  #Remove missing values
File_merged = File_merged %>% 
  filter(!is.na(MRET), !is.infinite(Market_RET))

  #Run regression for different firm
models = dlply(File_merged, "KYPERMNO", function(df) 
  lm(MRET~ Market_RET, data = df))
  #Apply coef to each model and return a data frame
coef_reg = ldply(models, coef)
  #Rename coef into alpha, beta
coef_reg = coef_reg %>% 
  mutate(
    Intercept = coef_reg$`(Intercept)`,
    beta      = coef_reg$vwretd
  )
coef_reg = subset(coef_reg, select = -c(2,3))

#PROBLEM 2: SHARPE RATIO
  #Calculate mean(RET), sd(RET)
library(data.table) 
Data = setDT(File_merged)[, list(mean_ret = mean(MRET,na.rm=TRUE), 
                                  sd_ret = sd(MRET,na.rm=TRUE)), 
                     by = c("KYPERMNO")]
File_merged = merge(File_merged, Data, 
                    by.x = c('KYPERMNO'),
                    by.y = c('KYPERMNO'))
  #Anuualized mean and standard deviation of RET
File_merged = File_merged %>% 
  mutate(
    ann_mean_ret = mean_ret*12,
    ann_sd_ret   = sd_ret*sqrt(12)
  )
File_merged = File_merged %>% 
  mutate(
    Year  = year(MCALDT),
    Month = month(MCALDT)
  )
  #Sharpe ratio
File_merged = File_merged %>% 
  mutate(SR = (ann_mean_ret - RF)/ann_sd_ret)

#PROBLEM 3: BETA
  #Using file "test" from PROBLEM 2
  #Command
date_rolling   = character()
beta_rolling   = numeric()

for (i in 1:nrow(test)) {
  date_i = test$MCALDT[i]
  date_rolling = append(date_rolling, date_i)
  train = subset(test, date_i > MCALDT & date_i < MCALDT + 60*30) 
  if (nrow(train)>=1)
  {beta_rolling = append(beta_rolling,lm (MRET~Market_RET, data = train)$coef['Market_RET'])}
  else {beta_rolling = append(beta_rolling,0)}
}

rolling_data = data.frame(date_rolling, beta_rolling)

#EXPANSION (data needs at least 36 months)

date_rolling   = character()
beta_rolling   = numeric()

for (i in 1:nrow(test)) {
  date_i = test$MCALDT[i]
  date_rolling = append(date_rolling, date_i)
  train = subset(test, date_i > MCALDT & date_i < MCALDT + 60*30) 
  if (nrow(train)>=36)
  {beta_rolling = append(beta_rolling,lm (MRET~Market_RET, data = train)$coef['Market_RET'])}
  else {beta_rolling = append(beta_rolling,0)}
  
}

rolling_data = data.frame(date_rolling, beta_rolling)
