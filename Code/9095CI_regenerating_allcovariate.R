rm(list = setdiff(ls(),lsf.str()))
library(data.table)
library(dplyr)
library(stringr)
library(car)
#library(lubridate)

data <- fread("../Data/returns_vix_usd_SPY.csv")
data <- data.frame(data)

head(data)
dim(data)

# FOMC date
if(1 == 1)
{
  FOMC <- c("2007-09-18", "2007-10-31","2007-12-11","2008-01-22",
            "2008-01-30", "2008-03-18","2008-04-30","2008-10-08",
            "2008-10-29","2008-12-16","2015-12-17","2016-12-15",
            "2017-03-16","2017-06-15","2017-12-14","2018-03-22",
            "2018-06-14"
  )
  
  # one day after FOMC
  FOMClag1 <- c("2007-09-19", "2007-11-01","2007-12-12","2008-01-23",
                "2008-01-31", "2008-03-19","2008-05-01","2008-10-09",
                "2008-10-30","2008-12-17","2015-12-18","2016-12-16",
                "2017-03-17","2017-06-16","2017-12-15","2018-03-23",
                "2018-06-15")
  
  #two day after FOMC
  FOMClag2 <- c("2007-09-20", "2007-11-02","2007-12-13","2008-01-24",
                "2008-02-01", "2008-03-20","2008-05-02","2008-10-10",
                "2008-10-31","2008-12-18","2015-12-21","2016-12-19",
                "2017-03-20","2017-06-19","2017-12-18","2018-03-26",
                "2018-06-18")
  
  #three day after FOMC
  FOMClag3 <- c("2007-09-21", "2007-11-05","2007-12-14","2008-01-25",
                "2008-02-04", "2008-03-24","2008-05-05","2008-10-13",
                "2008-11-03","2008-12-19","2015-12-22","2016-12-20",
                "2017-03-21","2017-06-20","2017-12-19","2018-03-27",
                "2018-06-19")
  
  data$I_fomc <- ifelse(data$date%in%FOMC, 1, 0)
  data$I_fomc1lag <- ifelse(data$date%in%FOMClag1, 1, 0)
  data$I_fomc2lag <- ifelse(data$date%in%FOMClag2, 1, 0)
  data$I_fomc3lag <- ifelse(data$date%in%FOMClag3, 1, 0)
  
  
  data$I_fomclags <- data$I_fomc+data$I_fomc1lag+data$I_fomc2lag+data$I_fomc3lag
  
  data$I_fomc <- as.factor(data$I_fomc)
  data$I_fomc1lag <- as.factor(data$I_fomc1lag)
  data$I_fomc2lag <- as.factor(data$I_fomc2lag)
  data$I_fomc3lag <- as.factor(data$I_fomc3lag)
  data$I_fomclags <- as.factor(data$I_fomclags)  
}

## weekday and months
data$weekday <- wday(data$date) - 1
data$weekday <- as.factor(data$weekday)
data$mon <- month(data$date)
data$mon <- as.factor(data$mon)

data$date <- NULL
head(data)
#dummy_vars <- model.matrix(~mon-1,data)
# write.csv(data, "./Data/all_data.csv", row.names = F)

#data$mon <- NULL

#data <- cbind(data, dummy_vars)
#head(data)
#write.csv(data, "../Data/all_data_SPY.csv", row.names = F)

if(file.exists("../Data/fit_b_scaled_SPY.rds"))
{
  fit_b_scaled <- readRDS("../Data/fit_b_scaled_SPY.rds")
}else
{
  data_scaled <- cbind(scale(data[,1:(ncol(data)-7)]), data[, (ncol(data)-6):ncol(data)])
  fit_b_scaled <- brm(data = data_scaled, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags,
                      cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                      backend = "cmdstanr",
                      prior = c(prior(normal(0, 10), class = Intercept),
                                prior(normal(0, 10), class = b),
                                prior(gamma(4, 1), class = nu),
                                prior(cauchy(0, 1),  class = sigma)), seed = 1111)
  saveRDS(fit_b_scaled, file = "../Data/fit_b_scaled_SPY.rds")
}


