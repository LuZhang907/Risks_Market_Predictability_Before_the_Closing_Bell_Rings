rm(list = setdiff(ls(),lsf.str()))
library(data.table)
library(dplyr)
library(stringr)
library(car)
#library(lubridate)

data <- fread("/Users/luzhang/Desktop/Risks/data/returns_vix_usd_SPY.csv")
data <- data.frame(data)

head(data)
dim(data)

data$date <- as.Date(as.character(data$date), format = "%Y%m%d")

# FOMC date
if(1 == 1)
{
  FOMC <- c("2024-12-17","2016-12-15",
            "2017-03-16","2017-06-15","2017-12-14","2018-03-22",
            "2018-06-14","2018-09-27",	"2018-12-20",	"2019-08-01",	"2019-09-19",	
            "2019-10-31",	"2020-03-03",	"2020-03-16",	"2022-03-17",	"2022-05-05",
            "2022-06-16",	"2022-07-27",	"2022-09-21",	"2022-11-02",	"2022-12-14",	
            "2023-02-01",	"2023-03-22",	"2023-05-03",	"2023-07-26",	"2024-09-18")
  
  # one day after FOMC
  FOMClag1 <- c("2024-12-18","2016-12-16",
                "2017-03-17","2017-06-16","2017-12-15","2018-03-23",
                "2018-06-15","2018-09-28",	"2018-12-21",	"2019-08-02",	"2019-09-20",	
                "2019-11-01",	"2020-03-04",	"2020-03-17",	"2022-03-18",	"2022-05-06",
                "2022-06-17",	"2022-07-28",	"2022-09-22",	"2022-11-03",	"2022-12-15",
                "2023-02-02",	"2023-03-23",	"2023-05-04",	"2023-07-27",	"2024-09-19")
  
  #two day after FOMC
  FOMClag2 <- c("2024-12-21","2016-12-19",
                "2017-03-20","2017-06-19","2017-12-18","2018-03-26",
                "2018-06-18","2018-10-01",	"2018-12-24",	"2019-08-05",	"2019-09-23",	
                "2019-11-04", "2020-03-05",	"2020-03-18",	"2022-03-21",	"2022-05-09",	
                "2022-06-21",	"2022-07-29", "2022-09-23",	"2022-11-04",	"2022-12-16",	
                "2023-02-03",	"2023-03-24",	"2023-05-05",
                "2023-07-28",	"2024-09-20"
                )
  
  #three day after FOMC
  FOMClag3 <- c("2024-12-22","2016-12-20",
                "2017-03-21","2017-06-20","2017-12-19","2018-03-27",
                "2018-06-19","2018-10-02",	"2018-12-26",	"2019-08-06",	"2019-09-24",
                "2019-11-05",	"2020-03-06",	"2020-03-19",	"2022-03-22",	"2022-05-10",	
                "2022-06-22",	"2022-08-01",	"2022-09-26",	"2022-11-07",	"2022-12-19",	
                "2023-02-06",	"2023-03-27",	"2023-05-08",	"2023-07-31",	"2024-09-23"
                )
  
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

# write.csv(data, "/Users/luzhang/Desktop/Risks/data/all_data.csv", row.names = F)
head(data)

# data overnight large/small
data_onsmall <- data[data$r_on <= median(data$r_on),]
data_onsmall$date <- NULL
data_onlarge <- data[data$r_on > median(data$r_on),]
data_onlarge$date <- NULL
dim(data_onlarge)
dim(data_onsmall)


# data three years rolling
data_roll3_2018 <- data[(year(data$date)<=2018) & (year(data$date)>=2016),]
data_roll3_2018$date <- NULL
data_roll3_2019 <- data[(year(data$date)<=2019) & (year(data$date)>=2017),]
data_roll3_2019$date <- NULL
data_roll3_2020 <- data[(year(data$date)<=2020) & (year(data$date)>=2018),]
data_roll3_2020$date <- NULL
data_roll3_2021 <- data[(year(data$date)<=2021) & (year(data$date)>=2019),]
data_roll3_2021$date <- NULL
data_roll3_2022 <- data[(year(data$date)<=2022) & (year(data$date)>=2020),]
data_roll3_2022$date <- NULL
data_roll3_2023 <- data[(year(data$date)<=2023) & (year(data$date)>=2021),]
data_roll3_2023$date <- NULL
data_roll3_2024 <- data[(year(data$date)<=2024) & (year(data$date)>=2022),]
data_roll3_2024$date <- NULL

data$date <- NULL
head(data)

############
# Analysis #
############

######## Linear model #######
if(1==1)
{
  names(data)
  # plot(data$r13)
  #fit_lm <- lm(r13~.-vixpct-usdpct-I_fomclags-mon-vixpctlag-usdlagclose, data = data)
  fit_lm <- lm(r13~.-vixpct-usdpct-I_fomclags, data = data)
  #plot(fit_lm)
  (rst_lm <- summary(fit_lm))
  rst_lm <- as.data.frame(rst_lm$coefficients)
  rst_lm$sig <- "No"
  rst_lm$sig[rst_lm$`Pr(>|t|)` < 0.05] <- "Yes"
  
  xtable::xtable(rst_lm, digits = -2, caption = "Estimates of the multiple linear regression model.")
  #step(fit_lm)  
}


######## Bayesian methods #########
library(brms)
library(ggplot2)
library(tidyverse)
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# cmdstanr::check_cmdstan_toolchain(fix = TRUE)
# cmdstanr::install_cmdstan()

###########
# fitting #
###########

# fit_tmp <- brm(data = data, family = student, 
#              r13~.-vixpct-usdpct-I_fomclags,
#              #r13 ~ r6,
#              cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
#              backend = "cmdstanr",
#              prior = c(prior(normal(0, 10), class = Intercept),
#                        prior(normal(0, 10), class = b),
#                        prior(gamma(4, 1), class = nu),
#                        prior(cauchy(0, 1),  class = sigma)), seed = 1111)

if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_full_SPY.rds"))
{
  fit_b <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_full_SPY.rds") 
}else
{
  fit_b <- brm(data = data, family = student, 
               #r13~.-vixpct-usdpct-I_fomclags,
               r13~.-vixpct-usdpct-I_fomclags,
               cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
               backend = "cmdstanr",
               prior = c(prior(normal(0, 10), class = Intercept),
                         prior(normal(0, 10), class = b),
                         prior(gamma(4, 1), class = nu),
                         prior(cauchy(0, 1),  class = sigma)), seed = 1111)
  saveRDS(fit_b, file = "/Users/luzhang/Desktop/Risks/data/fit_full_SPY.rds")
}


if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_scaled_SPY.rds"))
{
 fit_b_scaled <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_scaled_SPY.rds")
}else
{
 data_scaled <- cbind(scale(data[,1:(ncol(data)-7)]), data[, (ncol(data)-6):ncol(data)])
 fit_b_scaled <- brm(data = data_scaled, family = student, r13~.-vixpct-usdpct-I_fomclags,
              cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
              backend = "cmdstanr",
              prior = c(prior(normal(0, 10), class = Intercept),
                        prior(normal(0, 10), class = b),
                        prior(gamma(4, 1), class = nu),
                        prior(cauchy(0, 1),  class = sigma)), seed = 1111)
 saveRDS(fit_b_scaled, file = "/Users/luzhang/Desktop/Risks/data/fit_b_scaled_SPY.rds")
}


# data overnight split
if(1==1)   
{
  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_onlarge_SPY.rds"))
  {
    fit_b_onlarge <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_onlarge_SPY.rds")
  }else
  {
    fit_b_onlarge <- brm(data = data_onlarge, family = student, r13~.-vixpct-usdpct-I_fomclags,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_onlarge, file = "/Users/luzhang/Desktop/Risks/data/fit_b_onlarge_SPY.rds")
  }
  
  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_onsmall_SPY.rds"))
  {
    fit_b_onsmall <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_onsmall_SPY.rds")
  }else
  {
    fit_b_onsmall <- brm(data = data_onsmall, family = student, r13~.-vixpct-usdpct-I_fomclags,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_onsmall, file = "/Users/luzhang/Desktop/Risks/data/fit_b_onsmall_SPY.rds")
  }
  
}

if(1==2) # seed2 (different seeds checked, seems no difference)
{
  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_onlarge_seed2_SPY.rds"))
  {
    fit_b_onlarge_seed2 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_onlarge_seed2_SPY.rds")
  }else
  {
    fit_b_onlarge_seed2 <- brm(data = data_onlarge, family = student, r13~.-vixpct-usdpct-I_fomclags,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 3333)
    saveRDS(fit_b_onlarge_seed2, file = "/Users/luzhang/Desktop/Risks/data/fit_b_onlarge_seed2_SPY.rds")
  }

  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_onsmall_seed2_SPY.rds"))
  {
    fit_b_onsmall_seed2 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_onsmall_seed2_SPY.rds")
  }else
  {
    fit_b_onsmall_seed2 <- brm(data = data_onsmall, family = student, r13~.-vixpct-usdpct-I_fomclags,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 3333)
    saveRDS(fit_b_onsmall_seed2, file = "/Users/luzhang/Desktop/Risks/data/fit_b_onsmall_seed2_SPY.rds")
  }

}

# data three years rolling
if(1==2)
{
  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2018_SPY.rds"))
  {
    fit_b_roll3_2018 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2018_SPY.rds")
  }else
  {
    fit_b_roll3_2018 <- brm(data = data_roll3_2018, family = student,
                            r13~.-vixpct-usdpct-I_fomclags,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2018, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2018_SPY.rds")
  }

  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2019_SPY.rds"))
  {
    fit_b_roll3_2019 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2019_SPY.rds")
  }else
  {
    fit_b_roll3_2019 <- brm(data = data_roll3_2019, family = student,
                            r13~.-vixpct-usdpct-I_fomclags,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2019, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2019_SPY.rds")
  }

  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2020_SPY.rds"))
  {
    fit_b_roll3_2020 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2020_SPY.rds")
  }else
  {
    fit_b_roll3_2020 <- brm(data = data_roll3_2020, family = student,
                            r13~.-vixpct-usdpct-I_fomclags,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2020, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2020_SPY.rds")
  }


  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2021_SPY.rds"))
  {
    fit_b_roll3_2021 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2021_SPY.rds")
  }else
  {
    fit_b_roll3_2021 <- brm(data = data_roll3_2021, family = student,
                            r13~.-vixpct-usdpct-I_fomclags,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2021, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2021_SPY.rds")
  }

  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2022_SPY.rds"))
  {
    fit_b_roll3_2022 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2022_SPY.rds")
  }else
  {
    fit_b_roll3_2022 <- brm(data = data_roll3_2022, family = student,
                            r13~.-vixpct-usdpct-I_fomclags,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2022, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2022_SPY.rds")
  }

  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2023_SPY.rds"))
  {
    fit_b_roll3_2023 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2023_SPY.rds")
  }else
  {
    fit_b_roll3_2023 <- brm(data = data_roll3_2023, family = student,
                            r13~.-vixpct-usdpct-I_fomclags,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2023, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2023_SPY.rds")
  }

  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2024_SPY.rds"))
  {
    fit_b_roll3_2024 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2024_SPY.rds")
  }else
  {
    fit_b_roll3_2024 <- brm(data = data_roll3_2024, family = student,
                            r13~.-vixpct-usdpct-I_fomclags,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2024, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2024_SPY.rds")
  }


  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2016_SPY.rds"))
  {
    fit_b_roll3_2016 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2016_SPY.rds")
  }else
  {
    fit_b_roll3_2016 <- brm(data = data_roll3_2016, family = student,
                            r13~.-vixpct-usdpct-I_fomclags,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2016, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2016_SPY.rds")
  }


  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2017_SPY.rds"))
  {
    fit_b_roll3_2017 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2017_SPY.rds")
  }else
  {
    fit_b_roll3_2017 <- brm(data = data_roll3_2017, family = student,
                            r13~.-vixpct-usdpct-I_fomclags,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2017, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2017_SPY.rds")
  }

  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2018_SPY.rds"))
  {
    fit_b_roll3_2018 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2018_SPY.rds")
  }else
  {
    fit_b_roll3_2018 <- brm(data = data_roll3_2018, family = student,
                            r13~.-vixpct-usdpct-I_fomclags,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2018, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_2018_SPY.rds")
  }

}

# data three years rolling noFOMC
if(1==1)
{
  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2018_SPY.rds"))
  {
    fit_b_roll3_noFOMC2018 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2018_SPY.rds")
  }else
  {
    fit_b_roll3_noFOMC2018 <- brm(data = data_roll3_2018, family = student, 
                                  r13~.-vixpct-usdpct-I_fomclags-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2018, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2018_SPY.rds")
  }
  
  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2019_SPY.rds"))
  {
    fit_b_roll3_noFOMC2019 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2019_SPY.rds")
  }else
  {
    fit_b_roll3_noFOMC2019 <- brm(data = data_roll3_2019, family = student, 
                                  r13~.-vixpct-usdpct-I_fomclags-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2019, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2019_SPY.rds")
  }
  
  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2020_SPY.rds"))
  {
    fit_b_roll3_noFOMC2020 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2020_SPY.rds")
  }else
  {
    fit_b_roll3_noFOMC2020 <- brm(data = data_roll3_2020, family = student, 
                                  r13~.-vixpct-usdpct-I_fomclags-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2020, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2020_SPY.rds")
  }
  
  
  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2021_SPY.rds"))
  {
    fit_b_roll3_noFOMC2021 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2021_SPY.rds")
  }else
  {
    fit_b_roll3_noFOMC2021 <- brm(data = data_roll3_2021, family = student, 
                                  r13~.-vixpct-usdpct-I_fomclags-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2021, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2021_SPY.rds")
  }
  
  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2022_SPY.rds"))
  {
    fit_b_roll3_noFOMC2022 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2022_SPY.rds")
  }else
  {
    fit_b_roll3_noFOMC2022 <- brm(data = data_roll3_2022, family = student, 
                                  r13~.-vixpct-usdpct-I_fomclags-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2022, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2022_SPY.rds")
  }
  
  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2023_SPY.rds"))
  {
    fit_b_roll3_noFOMC2023 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2023_SPY.rds")
  }else
  {
    fit_b_roll3_noFOMC2023 <- brm(data = data_roll3_2023, family = student, 
                                  r13~.-vixpct-usdpct-I_fomclags-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2023, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2023_SPY.rds")
  }
  
  if(file.exists("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2024_SPY.rds"))
  {
    fit_b_roll3_noFOMC2024 <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2024_SPY.rds")
  }else
  {
    fit_b_roll3_noFOMC2024 <- brm(data = data_roll3_2024, family = student, 
                                  r13~.-vixpct-usdpct-I_fomclags-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2024, file = "/Users/luzhang/Desktop/Risks/data/fit_b_roll3_noFOMC2024_SPY.rds")
  }
} 
  
  


################
# post fitting #
################

fit_tmp <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_full_SPY.rds")
# for one specific model
md_name <- "fit_tmp"   # <------- change model name each time
md <- get(md_name)
md_file <-  paste0("/Users/luzhang/Desktop/Risks/data/", md_name, ".rds")
if(!file.exists(md_file)) saveRDS(md, file = md_file)
print(md$fit, digits=4)

#plot(md)

post_data <- as.data.frame(md$fit)
col_post <- names(post_data)
rst <- cbind(apply(post_data, 2, quantile, 0.025), apply(post_data, 2, quantile, 0.5), apply(post_data, 2, quantile, 0.975))
rst <- data.frame(rst)
names(rst) <- c("2.5%", "50%", "97.5%")
rst$sig <- "No"
rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
rownames(rst) <- stringr::str_replace(rownames(rst), "b_", "")
rst <- rst[!(rownames(rst) %in% c("lprior", "lp__")),]
xtable::xtable(rst, digits = -2, caption = "Bayesian linear regression model with Student-t error terms.")

var_vec <-row.names(rst)

# after data summary plots
if(1==1)
{
  md_vec <- c("fit_b", "fit_b_after2008", "fit_b_after2018", "fit_b_after2019",
              "fit_b_after2020", "fit_b_after2021", "fit_b_after2022", "fit_b_after2023",
              "fit_b_after2024", "fit_b_after2016", "fit_b_after2017")
  yr_vec <- c("All", "> 2008", "> 2018", "> 2019", "> 2020", "> 2021", "> 2022", "> 2023", "> 2024", "> 2016", "> 2017")
  
  post_data <- as.data.frame(fit_b_after2008$fit)
  col_post <- names(post_data)
  rst <- cbind(apply(post_data, 2, quantile, 0.025),
               apply(post_data, 2, quantile, 0.5),
               apply(post_data, 2, quantile, 0.975))
  rst <- data.frame(rst)
  names(rst) <- c("2.5%", "50%", "97.5%")
  rst$sig <- "No"
  rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
  rownames(rst) <- stringr::str_replace(rownames(rst), "b_", "")
  rst <- rst[!(rownames(rst) %in% c("lprior", "lp__")),]
  var_vec <-row.names(rst)
  

  pdf("./images/after_yr_SPY.pdf", 12, 12)
  par(mfrow = c(5,5), mar = c(5,3,4,1.2))
  for(var_now in var_vec[-1])
  {
    rst_all <- NULL
    for(ii in 1:length(md_vec))
    {
      # ii <- 1
      md_name <- md_vec[ii]
      md <- get(md_name)
      post_data <- as.data.frame(md$fit)
      col_post <- names(post_data)
      rst <- cbind(apply(post_data, 2, quantile, 0.025),
                   apply(post_data, 2, quantile, 0.5),
                   apply(post_data, 2, quantile, 0.975))
      rst <- data.frame(rst)
      names(rst) <- c("2.5%", "50%", "97.5%")
      rst$sig <- "No"
      rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
      rownames(rst) <- stringr::str_replace(rownames(rst), "b_", "")
      rst <- rst[!(rownames(rst) %in% c("lprior", "lp__")),]
      rst_all <- rbind(rst_all, c(rst[var_now,], yr_vec[ii]))
    }
    rst_all <- data.frame(rst_all)
    names(rst_all) <- c("2.5%", "50%", "97.5%", "sig", "Period")
    me <- unlist(rst_all$`50%`)
    lo <- unlist(rst_all$`2.5%`)
    up <- unlist(rst_all$`97.5%`)
    plot(NULL, xlim=c(1, length(me)), ylim=c(min(lo, -0.0000001), max(up)), xaxt='n', ylab='', xlab='', main=var_now)
    axis(1, at = 1:length(me), labels = yr_vec, las=3)
    arrows(x0=1:length(me), x1=1:length(me), y0 = lo, y1 = up, angle=90, code=3, len=0.04)
    points(1:length(me), me, pch=4, col=1)
    abline(h=0, lty=4, col="red")
  }
  dev.off()
}

# after_noFOMC data summary plots
if(1==2)
{
  md_vec <- c("fit_b", "fit_b_after_noFOMC2008", "fit_b_after_noFOMC2018", "fit_b_after_noFOMC2019",
              "fit_b_after_noFOMC2020", "fit_b_after_noFOMC2021", "fit_b_after_noFOMC2022", "fit_b_after_noFOMC2023",
              "fit_b_after_noFOMC2024", "fit_b_after_noFOMC2016", "fit_b_after_noFOMC2017")
  yr_vec <- c("All", "> 2008", "> 2018", "> 2019", "> 2020", "> 2021", "> 2022", "> 2023", "> 2024", "> 2016", "> 2017")
  
  post_data <- as.data.frame(fit_b_after_noFOMC2008$fit)
  col_post <- names(post_data)
  rst <- cbind(apply(post_data, 2, quantile, 0.025),
               apply(post_data, 2, quantile, 0.5),
               apply(post_data, 2, quantile, 0.975))
  rst <- data.frame(rst)
  names(rst) <- c("2.5%", "50%", "97.5%")
  rst$sig <- "No"
  rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
  rownames(rst) <- stringr::str_replace(rownames(rst), "b_", "")
  rst <- rst[!(rownames(rst) %in% c("lprior", "lp__")),]
  var_vec <-row.names(rst)
  
  
  pdf("./images/after_noFOMC_yr_SPY.pdf", 12, 12)
  par(mfrow = c(5,5), mar = c(5,3,4,1.2))
  for(var_now in var_vec[-1])
  {
    rst_all <- NULL
    for(ii in 1:length(md_vec))
    {
      # ii <- 1
      md_name <- md_vec[ii]
      md <- get(md_name)
      post_data <- as.data.frame(md$fit)
      col_post <- names(post_data)
      rst <- cbind(apply(post_data, 2, quantile, 0.025),
                   apply(post_data, 2, quantile, 0.5),
                   apply(post_data, 2, quantile, 0.975))
      rst <- data.frame(rst)
      names(rst) <- c("2.5%", "50%", "97.5%")
      rst$sig <- "No"
      rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
      rownames(rst) <- stringr::str_replace(rownames(rst), "b_", "")
      rst <- rst[!(rownames(rst) %in% c("lprior", "lp__")),]
      rst_all <- rbind(rst_all, c(rst[var_now,], yr_vec[ii]))
    }
    rst_all <- data.frame(rst_all)
    names(rst_all) <- c("2.5%", "50%", "97.5%", "sig", "Period")
    me <- unlist(rst_all$`50%`)
    lo <- unlist(rst_all$`2.5%`)
    up <- unlist(rst_all$`97.5%`)
    plot(NULL, xlim=c(1, length(me)), ylim=c(min(lo, -0.0000001), max(up)), xaxt='n', ylab='', xlab='', main=var_now)
    axis(1, at = 1:length(me), labels = yr_vec, las=3)
    arrows(x0=1:length(me), x1=1:length(me), y0 = lo, y1 = up, angle=90, code=3, len=0.04)
    points(1:length(me), me, pch=4, col=1)
    abline(h=0, lty=4, col="red")
  }
  dev.off()
}

# before data summary plots
if(1==1)
{
  md_vec <- c("fit_b_before2008", "fit_b_before2018", "fit_b_before2019",
              "fit_b_before2020", "fit_b_before2021", "fit_b_before2022", "fit_b_before2023",
              "fit_b_before2024", "fit_b_before2016", "fit_b_before2017", "fit_b")
  yr_vec <- c("<= 2008", "<= 2018", "<= 2019", "<= 2020", "<= 2021", "<= 2022", "<= 2023", "<= 2024", "<= 2016", "<= 2017", "All")
  pdf("./images/before_yr_SPY.pdf", 12, 12)
  par(mfrow = c(5,5), mar = c(5,3,4,1.2))
  for(var_now in var_vec[-1])
  {
    rst_all <- NULL
    for(ii in 1:length(md_vec))
    {
      # ii <- 1
      md_name <- md_vec[ii]
      md <- get(md_name)
      post_data <- as.data.frame(md$fit)
      col_post <- names(post_data)
      rst <- cbind(apply(post_data, 2, quantile, 0.025),
                   apply(post_data, 2, quantile, 0.5),
                   apply(post_data, 2, quantile, 0.975))
      rst <- data.frame(rst)
      names(rst) <- c("2.5%", "50%", "97.5%")
      rst$sig <- "No"
      rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
      rownames(rst) <- stringr::str_replace(rownames(rst), "b_", "")
      rst <- rst[!(rownames(rst) %in% c("lprior", "lp__")),]
      rst_all <- rbind(rst_all, c(rst[var_now,], yr_vec[ii]))
    }
    rst_all <- data.frame(rst_all)
    names(rst_all) <- c("2.5%", "50%", "97.5%", "sig", "Period")
    me <- unlist(rst_all$`50%`)
    lo <- unlist(rst_all$`2.5%`)
    up <- unlist(rst_all$`97.5%`)
    plot(NULL, xlim=c(1, length(me)), ylim=c(min(lo, -0.0000001), max(up)), xaxt='n', ylab='', xlab='', main=var_now)
    axis(1, at = 1:length(me), labels = yr_vec, las=3)
    arrows(x0=1:length(me), x1=1:length(me), y0 = lo, y1 = up, angle=90, code=3, len=0.04)
    points(1:length(me), me, pch=4, col=1)
    abline(h=0, lty=4, col="red")
  }
  dev.off()
}

# before_noFOMC data summary plots
if(1==2)
{
  md_vec <- c("fit_b", "fit_b_before_noFOMC2008", "fit_b_before_noFOMC2018", "fit_b_before_noFOMC2019",
              "fit_b_before_noFOMC2020", "fit_b_before_noFOMC2021", "fit_b_before_noFOMC2022", "fit_b_before_noFOMC2023",
              "fit_b_before_noFOMC2024", "fit_b_before_noFOMC2016", "fit_b_before_noFOMC2017")
  yr_vec <- c("All", "> 2008", "> 2018", "> 2019", "> 2020", "> 2021", "> 2022", "> 2023", "> 2024", "> 2016", "> 2017")
  
  post_data <- as.data.frame(fit_b_before_noFOMC2008$fit)
  col_post <- names(post_data)
  rst <- cbind(apply(post_data, 2, quantile, 0.025),
               apply(post_data, 2, quantile, 0.5),
               apply(post_data, 2, quantile, 0.975))
  rst <- data.frame(rst)
  names(rst) <- c("2.5%", "50%", "97.5%")
  rst$sig <- "No"
  rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
  rownames(rst) <- stringr::str_replace(rownames(rst), "b_", "")
  rst <- rst[!(rownames(rst) %in% c("lprior", "lp__")),]
  var_vec <-row.names(rst)
  
  
  pdf("./images/before_noFOMC_yr_SPY.pdf", 12, 12)
  par(mfrow = c(5,5), mar = c(5,3,4,1.2))
  for(var_now in var_vec[-1])
  {
    rst_all <- NULL
    for(ii in 1:length(md_vec))
    {
      # ii <- 1
      md_name <- md_vec[ii]
      md <- get(md_name)
      post_data <- as.data.frame(md$fit)
      col_post <- names(post_data)
      rst <- cbind(apply(post_data, 2, quantile, 0.025),
                   apply(post_data, 2, quantile, 0.5),
                   apply(post_data, 2, quantile, 0.975))
      rst <- data.frame(rst)
      names(rst) <- c("2.5%", "50%", "97.5%")
      rst$sig <- "No"
      rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
      rownames(rst) <- stringr::str_replace(rownames(rst), "b_", "")
      rst <- rst[!(rownames(rst) %in% c("lprior", "lp__")),]
      rst_all <- rbind(rst_all, c(rst[var_now,], yr_vec[ii]))
    }
    rst_all <- data.frame(rst_all)
    names(rst_all) <- c("2.5%", "50%", "97.5%", "sig", "Period")
    me <- unlist(rst_all$`50%`)
    lo <- unlist(rst_all$`2.5%`)
    up <- unlist(rst_all$`97.5%`)
    plot(NULL, xlim=c(1, length(me)), ylim=c(min(lo, -0.0000001), max(up)), xaxt='n', ylab='', xlab='', main=var_now)
    axis(1, at = 1:length(me), labels = yr_vec, las=3)
    arrows(x0=1:length(me), x1=1:length(me), y0 = lo, y1 = up, angle=90, code=3, len=0.04)
    points(1:length(me), me, pch=4, col=1)
    abline(h=0, lty=4, col="red")
  }
  dev.off()
}

# overnight data summary plots
if(1==1)
{
  md_vec <- c("fit_b_onsmall", "fit_b_onlarge", "fit_b")
  yr_vec <- c("Small", "Large", "All")
  pdf("./images/on_plots_SPY.pdf", 12, 12)
  par(mfrow = c(5,5), mar = c(5,3,4,1.2))
  for(var_now in var_vec[-1])
  {
    rst_all <- NULL
    for(ii in 1:length(md_vec))
    {
      # ii <- 1
      md_name <- md_vec[ii]
      md <- get(md_name)
      post_data <- as.data.frame(md$fit)
      col_post <- names(post_data)
      rst <- cbind(apply(post_data, 2, quantile, 0.025), apply(post_data, 2, quantile, 0.5), apply(post_data, 2, quantile, 0.975))
      rst <- data.frame(rst)
      names(rst) <- c("2.5%", "50%", "97.5%")
      rst$sig <- "No"
      rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
      rownames(rst) <- stringr::str_replace(rownames(rst), "b_", "")
      rst <- rst[!(rownames(rst) %in% c("lprior", "lp__")),]
      rst_all <- rbind(rst_all, c(rst[var_now,], yr_vec[ii]))
    }
    rst_all <- data.frame(rst_all)
    names(rst_all) <- c("2.5%", "50%", "97.5%", "sig", "Period")
    me <- unlist(rst_all$`50%`)
    lo <- unlist(rst_all$`2.5%`)
    up <- unlist(rst_all$`97.5%`)
    plot(NULL, xlim=c(1, length(me)), ylim=c(min(lo, -0.0000001), max(up)), xaxt='n', ylab='', xlab='', main=var_now)
    axis(1, at = 1:length(me), labels = yr_vec, las=3)
    arrows(x0=1:length(me), x1=1:length(me), y0 = lo, y1 = up, angle=90, code=3, len=0.04)
    points(1:length(me), me, pch=4, col=1)
    abline(h=0, lty=4, col="red")
  }
  dev.off()
}
if(1==2)  # seed 2
{
  md_vec <- c("fit_b_onsmall_seed2", "fit_b_onlarge_seed2", "fit_b")
  yr_vec <- c("Small", "Large", "All")
  pdf("./images/on_plots_seed2.pdf", 12, 12)
  par(mfrow = c(5,5), mar = c(5,3,4,1.2))
  for(var_now in var_vec[-1])
  {
    rst_all <- NULL
    for(ii in 1:length(md_vec))
    {
      # ii <- 1
      md_name <- md_vec[ii]
      md <- get(md_name)
      post_data <- as.data.frame(md$fit)
      col_post <- names(post_data)
      rst <- cbind(apply(post_data, 2, quantile, 0.025),
                   apply(post_data, 2, quantile, 0.5),
                   apply(post_data, 2, quantile, 0.975))
      rst <- data.frame(rst)
      names(rst) <- c("2.5%", "50%", "97.5%")
      rst$sig <- "No"
      rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
      rownames(rst) <- stringr::str_replace(rownames(rst), "b_", "")
      rst <- rst[!(rownames(rst) %in% c("lprior", "lp__")),]
      rst_all <- rbind(rst_all, c(rst[var_now,], yr_vec[ii]))
    }
    rst_all <- data.frame(rst_all)
    names(rst_all) <- c("2.5%", "50%", "97.5%", "sig", "Period")
    me <- unlist(rst_all$`50%`)
    lo <- unlist(rst_all$`2.5%`)
    up <- unlist(rst_all$`97.5%`)
    plot(NULL, xlim=c(1, length(me)), ylim=c(min(lo, -0.0000001), max(up)), xaxt='n', ylab='', xlab='', main=var_now)
    axis(1, at = 1:length(me), labels = yr_vec, las=3)
    arrows(x0=1:length(me), x1=1:length(me), y0 = lo, y1 = up, angle=90, code=3, len=0.04)
    points(1:length(me), me, pch=4, col=1)
    abline(h=0, lty=4, col="red")
  }
  dev.off()
}


# data three years rolling noFOMC
if(1==1)
{
  md_vec <- c("fit_b_roll3_noFOMC2018", "fit_b_roll3_noFOMC2019",
              "fit_b_roll3_noFOMC2020", "fit_b_roll3_noFOMC2021", "fit_b_roll3_noFOMC2022", "fit_b_roll3_noFOMC2023",
              "fit_b_roll3_noFOMC2024", "fit_b_roll3_noFOMC2016", "fit_b_roll3_noFOMC2017", "fit_b_roll3_noFOMC2018", "fit_b")
  yr_vec <- c("2018(3)", "2019(3)", "2020(3)", "2021(3)", "2022(3)", "2023(3)", "2024(3)", "2016(3)", "2017(3)", "2018(3)", "All")
  
  post_data <- as.data.frame(fit_b_roll3_noFOMC2018$fit)
  col_post <- names(post_data)
  rst <- cbind(apply(post_data, 2, quantile, 0.025),
               apply(post_data, 2, quantile, 0.5),
               apply(post_data, 2, quantile, 0.975))
  rst <- data.frame(rst)
  names(rst) <- c("2.5%", "50%", "97.5%")
  rst$sig <- "No"
  rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
  rownames(rst) <- stringr::str_replace(rownames(rst), "b_", "")
  rst <- rst[!(rownames(rst) %in% c("lprior", "lp__")),]
  var_vec <-row.names(rst)
  
  
  pdf("./images/roll3_noFOMC_SPY.pdf", 12, 12)
  par(mfrow = c(5, 5), mar = c(5,3,4,1.2))
  var_all <- var_vec[-1]
  for(kk in 1:length(var_all))
  {
    var_now <- var_all[kk]
    rst_all <- NULL
    for(ii in 1:length(md_vec))
    {
      # ii <- 1
      md_name <- md_vec[ii]
      md <- get(md_name)
      post_data <- as.data.frame(md$fit)
      col_post <- names(post_data)
      rst <- cbind(apply(post_data, 2, quantile, 0.025),
                   apply(post_data, 2, quantile, 0.5),
                   apply(post_data, 2, quantile, 0.975))
      rst <- data.frame(rst)
      names(rst) <- c("2.5%", "50%", "97.5%")
      rst$sig <- "No"
      rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
      rownames(rst) <- stringr::str_replace(rownames(rst), "b_", "")
      rst <- rst[!(rownames(rst) %in% c("lprior", "lp__")),]
      rst_all <- rbind(rst_all, c(rst[var_now,], yr_vec[ii]))
    }
    rst_all <- data.frame(rst_all)
    names(rst_all) <- c("2.5%", "50%", "97.5%", "sig", "Period")
    me <- unlist(rst_all$`50%`)
    lo <- unlist(rst_all$`2.5%`)
    up <- unlist(rst_all$`97.5%`)
    plot(NULL, xlim=c(1, length(me)), ylim=c(min(lo, -0.0000001), max(up)), xaxt='n', ylab='', xlab='', main=var_now)
    axis(1, at = 1:length(me), labels = yr_vec, las=3)
    arrows(x0=1:length(me), x1=1:length(me), y0 = lo, y1 = up, angle=90, code=3, len=0.04)
    points(1:length(me), me, pch=4, col=1)
    abline(h=0, lty=4, col="red")
  }
  dev.off()
}

if(1==2)
{
  pred <- predict(md)
  md$data$r13 - pred[,1]
  r_sq <- summary(lm(md$data$r13 ~ pred[, 1]))$r.squared
  cat("R squre:", r_sq, "\n")
  col_vec <- rep("yellow", nrow(md$data))
  col_vec[sign(md$data$r13) == sign(pred[,1])] <- "red"
  plot(md$data$r13, pch=16)
  points(pred[,1], col=col_vec, pch="+")
  plot(md$data$r13[250:500], pch=16)
  points(pred[250:500,1], col=col_vec, pch="+")
  acc <- sum(sign(md$data$r13) == sign(pred[,1])) / nrow(md$data)
  cat("Acc: ", acc, "\n")
  
}

# plots for papers (base model using all data)
if(1==1)
{
  md_name <- "fit_b_scaled"
  md <- get(md_name)
  
  # conditional effects
  pdf(paste0("./images/", md_name, "_plot_scaled_SPY.pdf"), 3, 3)
  var_fitted <- colnames(md$data)
  for(var_now in var_fitted[-1])
  {
    # var_now <- var_fitted[17]
    p <- conditional_effects(md, var_now)
    pp <- plot(p,plot=FALSE,cat_args=list(pch=4, size=1))[[1]] + ylim(-1, 1) + ggthemes::theme_clean()
    show(pp)
  }
  dev.off()
  
  # posterior distribution
  pdf(paste0("./images/", md_name, "_post_scaled_SPY.pdf"), 7, 1.5)
  plot(md, ask=F, N=1)
  dev.off()
  
  # leave-one-out C.V.
  set.seed(12)
  (loo_rst <- brms::loo(md))
  pdf(paste0("./images/", md_name, "_conv_SPY.pdf"), 4, 3)
  plot(loo_rst)
  dev.off()

  set.seed(21)
  pdf(paste0("./images/", md_name, "_ppcheck_SPY.pdf"), 4, 3)
  brms::pp_check(md, ndraws=100) + ggplot2::xlim(-10, 10)
  dev.off()

  set.seed(1122)
  brms::pp_check(md, ndraws = 100, type = "scatter_avg_grouped", group="weekday") +
    ggplot2::geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)
  set.seed(4553)
  brms::pp_check(md, ndraws = 100, type = "scatter_avg_grouped", group="I_fomc2lag") +
    ggplot2::geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)  
}
