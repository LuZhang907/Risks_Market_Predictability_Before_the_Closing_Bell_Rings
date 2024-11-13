rm(list = setdiff(ls(),lsf.str()))
library(data.table)
library(dplyr)
library(stringr)
library(car)
#library(lubridate)

data <- fread("./Data/returns_vix_usd_SPY.csv")
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

# write.csv(data, "./Data/all_data.csv", row.names = F)
head(data)

# data after
data_after2008 <- data[year(data$date) > 2008,]
data_after2008$date <- NULL
data_after2009 <- data[year(data$date) > 2009,]
data_after2009$date <- NULL
data_after2010 <- data[year(data$date) > 2010,]
data_after2010$date <- NULL
data_after2011 <- data[year(data$date) > 2011,]
data_after2011$date <- NULL
data_after2012 <- data[year(data$date) > 2012,]
data_after2012$date <- NULL
data_after2013 <- data[year(data$date) > 2013,]
data_after2013$date <- NULL
data_after2014 <- data[year(data$date) > 2014,]
data_after2014$date <- NULL
data_after2015 <- data[year(data$date) > 2015,]
data_after2015$date <- NULL
data_after2016 <- data[year(data$date) > 2016,]
data_after2016$date <- NULL
data_after2017 <- data[year(data$date) > 2017,]
data_after2017$date <- NULL

# data before
data_before2008 <- data[year(data$date)<=2008,]
data_before2008$date <- NULL
data_before2009 <- data[year(data$date)<=2009,]
data_before2009$date <- NULL
data_before2010 <- data[year(data$date)<=2010,]
data_before2010$date <- NULL
data_before2011 <- data[year(data$date)<=2011,]
data_before2011$date <- NULL
data_before2012 <- data[year(data$date)<=2012,]
data_before2012$date <- NULL
data_before2013 <- data[year(data$date)<=2013,]
data_before2013$date <- NULL
data_before2014 <- data[year(data$date)<=2014,]
data_before2014$date <- NULL
data_before2015 <- data[year(data$date)<=2015,]
data_before2015$date <- NULL
data_before2016 <- data[year(data$date)<=2016,]
data_before2016$date <- NULL
data_before2017 <- data[year(data$date)<=2017,]
data_before2017$date <- NULL


# data now
data_now2008 <- data[year(data$date)==2008,]
data_now2008$date <- NULL
data_now2009 <- data[year(data$date)==2009,]
data_now2009$date <- NULL
data_now2010 <- data[year(data$date)==2010,]
data_now2010$date <- NULL
data_now2011 <- data[year(data$date)==2011,]
data_now2011$date <- NULL
data_now2012 <- data[year(data$date)==2012,]
data_now2012$date <- NULL
data_now2013 <- data[year(data$date)==2013,]
data_now2013$date <- NULL
data_now2014 <- data[year(data$date)==2014,]
data_now2014$date <- NULL
data_now2015 <- data[year(data$date)==2015,]
data_now2015$date <- NULL
data_now2016 <- data[year(data$date)==2016,]
data_now2016$date <- NULL
data_now2017 <- data[year(data$date)==2017,]
data_now2017$date <- NULL

# data overnight large/small
data_onsmall <- data[data$r_on <= median(data$r_on),]
data_onsmall$date <- NULL
data_onlarge <- data[data$r_on > median(data$r_on),]
data_onlarge$date <- NULL
dim(data_onlarge)
dim(data_onsmall)


# data three years rolling
data_roll3_2009 <- data[(year(data$date)<=2009) & (year(data$date)>=2007),]
data_roll3_2009$date <- NULL
data_roll3_2010 <- data[(year(data$date)<=2010) & (year(data$date)>=2008),]
data_roll3_2010$date <- NULL
data_roll3_2011 <- data[(year(data$date)<=2011) & (year(data$date)>=2009),]
data_roll3_2011$date <- NULL
data_roll3_2012 <- data[(year(data$date)<=2012) & (year(data$date)>=2010),]
data_roll3_2012$date <- NULL
data_roll3_2013 <- data[(year(data$date)<=2013) & (year(data$date)>=2011),]
data_roll3_2013$date <- NULL
data_roll3_2014 <- data[(year(data$date)<=2014) & (year(data$date)>=2012),]
data_roll3_2014$date <- NULL
data_roll3_2015 <- data[(year(data$date)<=2015) & (year(data$date)>=2013),]
data_roll3_2015$date <- NULL
data_roll3_2016 <- data[(year(data$date)<=2016) & (year(data$date)>=2014),]
data_roll3_2016$date <- NULL
data_roll3_2017 <- data[(year(data$date)<=2017) & (year(data$date)>=2015),]
data_roll3_2017$date <- NULL
data_roll3_2018 <- data[(year(data$date)<=2018) & (year(data$date)>=2016),]
data_roll3_2018$date <- NULL

data$date <- NULL
head(data)

############
# Analysis #
############

######## Linear model #######
if(1==2)
{
  names(data)
  # plot(data$r13)
  fit_lm <- lm(r13~.-r_onfh-vixpct-usdpct-I_fomclags-mon-vixpctlag-usdlagclose-usdpctlag, data = data)
  fit_lm <- lm(r13~.-r_onfh-vixpct-usdpct-I_fomclags, data = data)
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

fit_tmp <- brm(data = data, family = student, 
             r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdlagclose-mon,
             #r13 ~ r6,
             cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
             backend = "cmdstanr",
             prior = c(prior(normal(0, 10), class = Intercept),
                       prior(normal(0, 10), class = b),
                       prior(gamma(4, 1), class = nu),
                       prior(cauchy(0, 1),  class = sigma)), seed = 1111)

fwrite("./Data/fit_b.rds")

if(file.exists("./Data/fit_b.rds"))
{
  fit_b <- readRDS("./Data/fit_b.rds")
}else
{
  fit_b <- brm(data = data, family = student, 
                r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
               #r13~.-r_onfh-vixpct-usdpct-I_fomclags,
               cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
               backend = "cmdstanr",
               prior = c(prior(normal(0, 10), class = Intercept),
                         prior(normal(0, 10), class = b),
                         prior(gamma(4, 1), class = nu),
                         prior(cauchy(0, 1),  class = sigma)), seed = 1111)
}

if(file.exists("./Data/fit_b_scaled.rds"))
{
  fit_b <- readRDS("./Data/fit_b_scaled.rds")
}else
{
  data_scaled <- cbind(scale(data[,1:(ncol(data)-7)]), data[, (ncol(data)-6):ncol(data)])
  fit_b_scaled <- brm(data = data_scaled, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
               cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
               backend = "cmdstanr",
               prior = c(prior(normal(0, 10), class = Intercept),
                         prior(normal(0, 10), class = b),
                         prior(gamma(4, 1), class = nu),
                         prior(cauchy(0, 1),  class = sigma)), seed = 1111)
}



# data after
if(1==2)
{
  if(file.exists("./Data/fit_b_after2008.rds"))
  {
    fit_b_after2008 <- readRDS("./Data/fit_b_after2008.rds")
  }else
  {
    fit_b_after2008 <- brm(data = data_after2008, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                           cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                           backend = "cmdstanr",
                           prior = c(prior(normal(0, 10), class = Intercept),
                                     prior(normal(0, 10), class = b),
                                     prior(gamma(4, 1), class = nu),
                                     prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after2008, file = "./Data/fit_b_after2008.rds")
  }
  
  if(file.exists("./Data/fit_b_after2009.rds"))
  {
    fit_b_after2009 <- readRDS("./Data/fit_b_after2009.rds")
  }else
  {
    fit_b_after2009 <- brm(data = data_after2009, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                           cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                           backend = "cmdstanr",
                           prior = c(prior(normal(0, 10), class = Intercept),
                                     prior(normal(0, 10), class = b),
                                     prior(gamma(4, 1), class = nu),
                                     prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after2009, file = "./Data/fit_b_after2009.rds")
  }
  
  if(file.exists("./Data/fit_b_after2010.rds"))
  {
    fit_b_after2010 <- readRDS("./Data/fit_b_after2010.rds")
  }else
  {
    fit_b_after2010 <- brm(data = data_after2010, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                           cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                           backend = "cmdstanr",
                           prior = c(prior(normal(0, 10), class = Intercept),
                                     prior(normal(0, 10), class = b),
                                     prior(gamma(4, 1), class = nu),
                                     prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after2010, file = "./Data/fit_b_after2010.rds")
  }
  
  if(file.exists("./Data/fit_b_after2011.rds"))
  {
    fit_b_after2011 <- readRDS("./Data/fit_b_after2011.rds")
  }else
  {
    fit_b_after2011 <- brm(data = data_after2011, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                           cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                           backend = "cmdstanr",
                           prior = c(prior(normal(0, 10), class = Intercept),
                                     prior(normal(0, 10), class = b),
                                     prior(gamma(4, 1), class = nu),
                                     prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after2011, file = "./Data/fit_b_after2011.rds")
  }
  
  
  if(file.exists("./Data/fit_b_after2012.rds"))
  {
    fit_b_after2012 <- readRDS("./Data/fit_b_after2012.rds")
  }else
  {
    fit_b_after2012 <- brm(data = data_after2012, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                           cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                           backend = "cmdstanr",
                           prior = c(prior(normal(0, 10), class = Intercept),
                                     prior(normal(0, 10), class = b),
                                     prior(gamma(4, 1), class = nu),
                                     prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after2012, file = "./Data/fit_b_after2012.rds")
  }
  
  if(file.exists("./Data/fit_b_after2013.rds"))
  {
    fit_b_after2013 <- readRDS("./Data/fit_b_after2013.rds")
  }else
  {
    fit_b_after2013 <- brm(data = data_after2013, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                           cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                           backend = "cmdstanr",
                           prior = c(prior(normal(0, 10), class = Intercept),
                                     prior(normal(0, 10), class = b),
                                     prior(gamma(4, 1), class = nu),
                                     prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after2013, file = "./Data/fit_b_after2013.rds")
  }
  
  if(file.exists("./Data/fit_b_after2014.rds"))
  {
    fit_b_after2014 <- readRDS("./Data/fit_b_after2014.rds")
  }else
  {
    fit_b_after2014 <- brm(data = data_after2014, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                           cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                           backend = "cmdstanr",
                           prior = c(prior(normal(0, 10), class = Intercept),
                                     prior(normal(0, 10), class = b),
                                     prior(gamma(4, 1), class = nu),
                                     prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after2014, file = "./Data/fit_b_after2014.rds")
  }
  
  if(file.exists("./Data/fit_b_after2015.rds"))
  {
    fit_b_after2015 <- readRDS("./Data/fit_b_after2015.rds")
  }else
  {
    fit_b_after2015 <- brm(data = data_after2015, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                           cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                           backend = "cmdstanr",
                           prior = c(prior(normal(0, 10), class = Intercept),
                                     prior(normal(0, 10), class = b),
                                     prior(gamma(4, 1), class = nu),
                                     prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after2015, file = "./Data/fit_b_after2015.rds")
  }
  
  
  if(file.exists("./Data/fit_b_after2016.rds"))
  {
    fit_b_after2016 <- readRDS("./Data/fit_b_after2016.rds")
  }else
  {
    fit_b_after2016 <- brm(data = data_after2016, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                           cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                           backend = "cmdstanr",
                           prior = c(prior(normal(0, 10), class = Intercept),
                                     prior(normal(0, 10), class = b),
                                     prior(gamma(4, 1), class = nu),
                                     prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after2016, file = "./Data/fit_b_after2016.rds")
  }
  
  
  if(file.exists("./Data/fit_b_after2017.rds"))
  {
    fit_b_after2017 <- readRDS("./Data/fit_b_after2017.rds")
  }else
  {
    fit_b_after2017 <- brm(data = data_after2017, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                           cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                           backend = "cmdstanr",
                           prior = c(prior(normal(0, 10), class = Intercept),
                                     prior(normal(0, 10), class = b),
                                     prior(gamma(4, 1), class = nu),
                                     prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after2017, file = "./Data/fit_b_after2017.rds")
  }
}

# data after_noFOMC
if(1==2)
{
  if(file.exists("./Data/fit_b_after_noFOMC2008.rds"))
  {
    fit_b_after_noFOMC2008 <- readRDS("./Data/fit_b_after_noFOMC2008.rds")
  }else
  {
    fit_b_after_noFOMC2008 <- brm(data = data_after2008, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after_noFOMC2008, file = "./Data/fit_b_after_noFOMC2008.rds")
  }
  
  if(file.exists("./Data/fit_b_after_noFOMC2009.rds"))
  {
    fit_b_after_noFOMC2009 <- readRDS("./Data/fit_b_after_noFOMC2009.rds")
  }else
  {
    fit_b_after_noFOMC2009 <- brm(data = data_after2009, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after_noFOMC2009, file = "./Data/fit_b_after_noFOMC2009.rds")
  }
  
  if(file.exists("./Data/fit_b_after_noFOMC2010.rds"))
  {
    fit_b_after_noFOMC2010 <- readRDS("./Data/fit_b_after_noFOMC2010.rds")
  }else
  {
    fit_b_after_noFOMC2010 <- brm(data = data_after2010, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after_noFOMC2010, file = "./Data/fit_b_after_noFOMC2010.rds")
  }
  
  if(file.exists("./Data/fit_b_after_noFOMC2011.rds"))
  {
    fit_b_after_noFOMC2011 <- readRDS("./Data/fit_b_after_noFOMC2011.rds")
  }else
  {
    fit_b_after_noFOMC2011 <- brm(data = data_after2011, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after_noFOMC2011, file = "./Data/fit_b_after_noFOMC2011.rds")
  }
  
  
  if(file.exists("./Data/fit_b_after_noFOMC2012.rds"))
  {
    fit_b_after_noFOMC2012 <- readRDS("./Data/fit_b_after_noFOMC2012.rds")
  }else
  {
    fit_b_after_noFOMC2012 <- brm(data = data_after2012, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after_noFOMC2012, file = "./Data/fit_b_after_noFOMC2012.rds")
  }
  
  if(file.exists("./Data/fit_b_after_noFOMC2013.rds"))
  {
    fit_b_after_noFOMC2013 <- readRDS("./Data/fit_b_after_noFOMC2013.rds")
  }else
  {
    fit_b_after_noFOMC2013 <- brm(data = data_after2013, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after_noFOMC2013, file = "./Data/fit_b_after_noFOMC2013.rds")
  }
  
  if(file.exists("./Data/fit_b_after_noFOMC2014.rds"))
  {
    fit_b_after_noFOMC2014 <- readRDS("./Data/fit_b_after_noFOMC2014.rds")
  }else
  {
    fit_b_after_noFOMC2014 <- brm(data = data_after2014, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after_noFOMC2014, file = "./Data/fit_b_after_noFOMC2014.rds")
  }
  
  if(file.exists("./Data/fit_b_after_noFOMC2015.rds"))
  {
    fit_b_after_noFOMC2015 <- readRDS("./Data/fit_b_after_noFOMC2015.rds")
  }else
  {
    fit_b_after_noFOMC2015 <- brm(data = data_after2015, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after_noFOMC2015, file = "./Data/fit_b_after_noFOMC2015.rds")
  }
  
  
  if(file.exists("./Data/fit_b_after_noFOMC2016.rds"))
  {
    fit_b_after_noFOMC2016 <- readRDS("./Data/fit_b_after_noFOMC2016.rds")
  }else
  {
    fit_b_after_noFOMC2016 <- brm(data = data_after2016, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after_noFOMC2016, file = "./Data/fit_b_after_noFOMC2016.rds")
  }
  
  
  if(file.exists("./Data/fit_b_after_noFOMC2017.rds"))
  {
    fit_b_after_noFOMC2017 <- readRDS("./Data/fit_b_after_noFOMC2017.rds")
  }else
  {
    fit_b_after_noFOMC2017 <- brm(data = data_after2017, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_after_noFOMC2017, file = "./Data/fit_b_after_noFOMC2017.rds")
  }
}

# data before
if(1==2)
{
  if(file.exists("./Data/fit_b_before2008.rds"))
  {
    fit_b_before2008 <- readRDS("./Data/fit_b_before2008.rds")
  }else
  {
    fit_b_before2008 <- brm(data = data_before2008, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before2008, file = "./Data/fit_b_before2008.rds")
  }
  
  if(file.exists("./Data/fit_b_before2009.rds"))
  {
    fit_b_before2009 <- readRDS("./Data/fit_b_before2009.rds")
  }else
  {
    fit_b_before2009 <- brm(data = data_before2009, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before2009, file = "./Data/fit_b_before2009.rds")
  }
  
  if(file.exists("./Data/fit_b_before2010.rds"))
  {
    fit_b_before2010 <- readRDS("./Data/fit_b_before2010.rds")
  }else
  {
    fit_b_before2010 <- brm(data = data_before2010, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before2010, file = "./Data/fit_b_before2010.rds")
  }
  
  if(file.exists("./Data/fit_b_before2011.rds"))
  {
    fit_b_before2011 <- readRDS("./Data/fit_b_before2011.rds")
  }else
  {
    fit_b_before2011 <- brm(data = data_before2011, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before2011, file = "./Data/fit_b_before2011.rds")
  }
  
  
  if(file.exists("./Data/fit_b_before2012.rds"))
  {
    fit_b_before2012 <- readRDS("./Data/fit_b_before2012.rds")
  }else
  {
    fit_b_before2012 <- brm(data = data_before2012, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before2012, file = "./Data/fit_b_before2012.rds")
  }
  
  if(file.exists("./Data/fit_b_before2013.rds"))
  {
    fit_b_before2013 <- readRDS("./Data/fit_b_before2013.rds")
  }else
  {
    fit_b_before2013 <- brm(data = data_before2013, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before2013, file = "./Data/fit_b_before2013.rds")
  }
  
  if(file.exists("./Data/fit_b_before2014.rds"))
  {
    fit_b_before2014 <- readRDS("./Data/fit_b_before2014.rds")
  }else
  {
    fit_b_before2014 <- brm(data = data_before2014, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before2014, file = "./Data/fit_b_before2014.rds")
  }
  
  if(file.exists("./Data/fit_b_before2015.rds"))
  {
    fit_b_before2015 <- readRDS("./Data/fit_b_before2015.rds")
  }else
  {
    fit_b_before2015 <- brm(data = data_before2015, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before2015, file = "./Data/fit_b_before2015.rds")
  }
  
  
  if(file.exists("./Data/fit_b_before2016.rds"))
  {
    fit_b_before2016 <- readRDS("./Data/fit_b_before2016.rds")
  }else
  {
    fit_b_before2016 <- brm(data = data_before2016, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before2016, file = "./Data/fit_b_before2016.rds")
  }
  
  
  if(file.exists("./Data/fit_b_before2017.rds"))
  {
    fit_b_before2017 <- readRDS("./Data/fit_b_before2017.rds")
  }else
  {
    fit_b_before2017 <- brm(data = data_before2017, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before2017, file = "./Data/fit_b_before2017.rds")
  }
  
}

# data before_noFOMC
if(1==2)
{
  if(file.exists("./Data/fit_b_before_noFOMC2008.rds"))
  {
    fit_b_before_noFOMC2008 <- readRDS("./Data/fit_b_before_noFOMC2008.rds")
  }else
  {
    fit_b_before_noFOMC2008 <- brm(data = data_before2008, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                   cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                   backend = "cmdstanr",
                                   prior = c(prior(normal(0, 10), class = Intercept),
                                             prior(normal(0, 10), class = b),
                                             prior(gamma(4, 1), class = nu),
                                             prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before_noFOMC2008, file = "./Data/fit_b_before_noFOMC2008.rds")
  }
  
  if(file.exists("./Data/fit_b_before_noFOMC2009.rds"))
  {
    fit_b_before_noFOMC2009 <- readRDS("./Data/fit_b_before_noFOMC2009.rds")
  }else
  {
    fit_b_before_noFOMC2009 <- brm(data = data_before2009, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                   cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                   backend = "cmdstanr",
                                   prior = c(prior(normal(0, 10), class = Intercept),
                                             prior(normal(0, 10), class = b),
                                             prior(gamma(4, 1), class = nu),
                                             prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before_noFOMC2009, file = "./Data/fit_b_before_noFOMC2009.rds")
  }
  
  if(file.exists("./Data/fit_b_before_noFOMC2010.rds"))
  {
    fit_b_before_noFOMC2010 <- readRDS("./Data/fit_b_before_noFOMC2010.rds")
  }else
  {
    fit_b_before_noFOMC2010 <- brm(data = data_before2010, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                   cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                   backend = "cmdstanr",
                                   prior = c(prior(normal(0, 10), class = Intercept),
                                             prior(normal(0, 10), class = b),
                                             prior(gamma(4, 1), class = nu),
                                             prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before_noFOMC2010, file = "./Data/fit_b_before_noFOMC2010.rds")
  }
  
  if(file.exists("./Data/fit_b_before_noFOMC2011.rds"))
  {
    fit_b_before_noFOMC2011 <- readRDS("./Data/fit_b_before_noFOMC2011.rds")
  }else
  {
    fit_b_before_noFOMC2011 <- brm(data = data_before2011, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                   cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                   backend = "cmdstanr",
                                   prior = c(prior(normal(0, 10), class = Intercept),
                                             prior(normal(0, 10), class = b),
                                             prior(gamma(4, 1), class = nu),
                                             prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before_noFOMC2011, file = "./Data/fit_b_before_noFOMC2011.rds")
  }
  
  
  if(file.exists("./Data/fit_b_before_noFOMC2012.rds"))
  {
    fit_b_before_noFOMC2012 <- readRDS("./Data/fit_b_before_noFOMC2012.rds")
  }else
  {
    fit_b_before_noFOMC2012 <- brm(data = data_before2012, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                   cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                   backend = "cmdstanr",
                                   prior = c(prior(normal(0, 10), class = Intercept),
                                             prior(normal(0, 10), class = b),
                                             prior(gamma(4, 1), class = nu),
                                             prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before_noFOMC2012, file = "./Data/fit_b_before_noFOMC2012.rds")
  }
  
  if(file.exists("./Data/fit_b_before_noFOMC2013.rds"))
  {
    fit_b_before_noFOMC2013 <- readRDS("./Data/fit_b_before_noFOMC2013.rds")
  }else
  {
    fit_b_before_noFOMC2013 <- brm(data = data_before2013, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                   cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                   backend = "cmdstanr",
                                   prior = c(prior(normal(0, 10), class = Intercept),
                                             prior(normal(0, 10), class = b),
                                             prior(gamma(4, 1), class = nu),
                                             prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before_noFOMC2013, file = "./Data/fit_b_before_noFOMC2013.rds")
  }
  
  if(file.exists("./Data/fit_b_before_noFOMC2014.rds"))
  {
    fit_b_before_noFOMC2014 <- readRDS("./Data/fit_b_before_noFOMC2014.rds")
  }else
  {
    fit_b_before_noFOMC2014 <- brm(data = data_before2014, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                   cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                   backend = "cmdstanr",
                                   prior = c(prior(normal(0, 10), class = Intercept),
                                             prior(normal(0, 10), class = b),
                                             prior(gamma(4, 1), class = nu),
                                             prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before_noFOMC2014, file = "./Data/fit_b_before_noFOMC2014.rds")
  }
  
  if(file.exists("./Data/fit_b_before_noFOMC2015.rds"))
  {
    fit_b_before_noFOMC2015 <- readRDS("./Data/fit_b_before_noFOMC2015.rds")
  }else
  {
    fit_b_before_noFOMC2015 <- brm(data = data_before2015, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                   cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                   backend = "cmdstanr",
                                   prior = c(prior(normal(0, 10), class = Intercept),
                                             prior(normal(0, 10), class = b),
                                             prior(gamma(4, 1), class = nu),
                                             prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before_noFOMC2015, file = "./Data/fit_b_before_noFOMC2015.rds")
  }
  
  
  if(file.exists("./Data/fit_b_before_noFOMC2016.rds"))
  {
    fit_b_before_noFOMC2016 <- readRDS("./Data/fit_b_before_noFOMC2016.rds")
  }else
  {
    fit_b_before_noFOMC2016 <- brm(data = data_before2016, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                   cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                   backend = "cmdstanr",
                                   prior = c(prior(normal(0, 10), class = Intercept),
                                             prior(normal(0, 10), class = b),
                                             prior(gamma(4, 1), class = nu),
                                             prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before_noFOMC2016, file = "./Data/fit_b_before_noFOMC2016.rds")
  }
  
  
  if(file.exists("./Data/fit_b_before_noFOMC2017.rds"))
  {
    fit_b_before_noFOMC2017 <- readRDS("./Data/fit_b_before_noFOMC2017.rds")
  }else
  {
    fit_b_before_noFOMC2017 <- brm(data = data_before2017, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag,
                                   cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                   backend = "cmdstanr",
                                   prior = c(prior(normal(0, 10), class = Intercept),
                                             prior(normal(0, 10), class = b),
                                             prior(gamma(4, 1), class = nu),
                                             prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_before_noFOMC2017, file = "./Data/fit_b_before_noFOMC2017.rds")
  }
}

# data overnight split
if(1==2)   
{
  if(file.exists("./Data/fit_b_onlarge.rds"))
  {
    fit_b_onlarge <- readRDS("./Data/fit_b_onlarge.rds")
  }else
  {
    fit_b_onlarge <- brm(data = data_onlarge, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_onlarge, file = "./Data/fit_b_onlarge.rds")
  }
  
  if(file.exists("./Data/fit_b_onsmall.rds"))
  {
    fit_b_onsmall <- readRDS("./Data/fit_b_onsmall.rds")
  }else
  {
    fit_b_onsmall <- brm(data = data_onsmall, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_onsmall, file = "./Data/fit_b_onsmall.rds")
  }
  
}

if(1==2) # seed2 (different seeds checked, seems no difference)
{
  if(file.exists("./Data/fit_b_onlarge_seed2.rds"))
  {
    fit_b_onlarge_seed2 <- readRDS("./Data/fit_b_onlarge_seed2.rds")
  }else
  {
    fit_b_onlarge_seed2 <- brm(data = data_onlarge, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 3333)
    saveRDS(fit_b_onlarge_seed2, file = "./Data/fit_b_onlarge_seed2.rds")
  }
  
  if(file.exists("./Data/fit_b_onsmall_seed2.rds"))
  {
    fit_b_onsmall_seed2 <- readRDS("./Data/fit_b_onsmall_seed2.rds")
  }else
  {
    fit_b_onsmall_seed2 <- brm(data = data_onsmall, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 3333)
    saveRDS(fit_b_onsmall_seed2, file = "./Data/fit_b_onsmall_seed2.rds")
  }
  
}

# data three years rolling
if(1==2)
{
  if(file.exists("./Data/fit_b_roll3_2009.rds"))
  {
    fit_b_roll3_2009 <- readRDS("./Data/fit_b_roll3_2009.rds")
  }else
  {
    fit_b_roll3_2009 <- brm(data = data_roll3_2009, family = student, 
                            r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2009, file = "./Data/fit_b_roll3_2009.rds")
  }
  
  if(file.exists("./Data/fit_b_roll3_2010.rds"))
  {
    fit_b_roll3_2010 <- readRDS("./Data/fit_b_roll3_2010.rds")
  }else
  {
    fit_b_roll3_2010 <- brm(data = data_roll3_2010, family = student, 
                            r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2010, file = "./Data/fit_b_roll3_2010.rds")
  }
  
  if(file.exists("./Data/fit_b_roll3_2011.rds"))
  {
    fit_b_roll3_2011 <- readRDS("./Data/fit_b_roll3_2011.rds")
  }else
  {
    fit_b_roll3_2011 <- brm(data = data_roll3_2011, family = student, 
                            r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2011, file = "./Data/fit_b_roll3_2011.rds")
  }
  
  
  if(file.exists("./Data/fit_b_roll3_2012.rds"))
  {
    fit_b_roll3_2012 <- readRDS("./Data/fit_b_roll3_2012.rds")
  }else
  {
    fit_b_roll3_2012 <- brm(data = data_roll3_2012, family = student, 
                            r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2012, file = "./Data/fit_b_roll3_2012.rds")
  }
  
  if(file.exists("./Data/fit_b_roll3_2013.rds"))
  {
    fit_b_roll3_2013 <- readRDS("./Data/fit_b_roll3_2013.rds")
  }else
  {
    fit_b_roll3_2013 <- brm(data = data_roll3_2013, family = student, 
                            r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2013, file = "./Data/fit_b_roll3_2013.rds")
  }
  
  if(file.exists("./Data/fit_b_roll3_2014.rds"))
  {
    fit_b_roll3_2014 <- readRDS("./Data/fit_b_roll3_2014.rds")
  }else
  {
    fit_b_roll3_2014 <- brm(data = data_roll3_2014, family = student, 
                            r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2014, file = "./Data/fit_b_roll3_2014.rds")
  }
  
  if(file.exists("./Data/fit_b_roll3_2015.rds"))
  {
    fit_b_roll3_2015 <- readRDS("./Data/fit_b_roll3_2015.rds")
  }else
  {
    fit_b_roll3_2015 <- brm(data = data_roll3_2015, family = student, 
                            r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2015, file = "./Data/fit_b_roll3_2015.rds")
  }
  
  
  if(file.exists("./Data/fit_b_roll3_2016.rds"))
  {
    fit_b_roll3_2016 <- readRDS("./Data/fit_b_roll3_2016.rds")
  }else
  {
    fit_b_roll3_2016 <- brm(data = data_roll3_2016, family = student, 
                            r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2016, file = "./Data/fit_b_roll3_2016.rds")
  }
  
  
  if(file.exists("./Data/fit_b_roll3_2017.rds"))
  {
    fit_b_roll3_2017 <- readRDS("./Data/fit_b_roll3_2017.rds")
  }else
  {
    fit_b_roll3_2017 <- brm(data = data_roll3_2017, family = student, 
                            r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2017, file = "./Data/fit_b_roll3_2017.rds")
  }
  
  if(file.exists("./Data/fit_b_roll3_2018.rds"))
  {
    fit_b_roll3_2018 <- readRDS("./Data/fit_b_roll3_2018.rds")
  }else
  {
    fit_b_roll3_2018 <- brm(data = data_roll3_2018, family = student, 
                            r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                            cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                            backend = "cmdstanr",
                            prior = c(prior(normal(0, 10), class = Intercept),
                                      prior(normal(0, 10), class = b),
                                      prior(gamma(4, 1), class = nu),
                                      prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_2018, file = "./Data/fit_b_roll3_2018.rds")
  }
  
}

# data three years rolling noFOMC
if(1==2)
{
  if(file.exists("./Data/fit_b_roll3_noFOMC2009.rds"))
  {
    fit_b_roll3_noFOMC2009 <- readRDS("./Data/fit_b_roll3_noFOMC2009.rds")
  }else
  {
    fit_b_roll3_noFOMC2009 <- brm(data = data_roll3_2009, family = student, 
                                  r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2009, file = "./Data/fit_b_roll3_noFOMC2009.rds")
  }
  
  if(file.exists("./Data/fit_b_roll3_noFOMC2010.rds"))
  {
    fit_b_roll3_noFOMC2010 <- readRDS("./Data/fit_b_roll3_noFOMC2010.rds")
  }else
  {
    fit_b_roll3_noFOMC2010 <- brm(data = data_roll3_2010, family = student, 
                                  r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2010, file = "./Data/fit_b_roll3_noFOMC2010.rds")
  }
  
  if(file.exists("./Data/fit_b_roll3_noFOMC2011.rds"))
  {
    fit_b_roll3_noFOMC2011 <- readRDS("./Data/fit_b_roll3_noFOMC2011.rds")
  }else
  {
    fit_b_roll3_noFOMC2011 <- brm(data = data_roll3_2011, family = student, 
                                  r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2011, file = "./Data/fit_b_roll3_noFOMC2011.rds")
  }
  
  
  if(file.exists("./Data/fit_b_roll3_noFOMC2012.rds"))
  {
    fit_b_roll3_noFOMC2012 <- readRDS("./Data/fit_b_roll3_noFOMC2012.rds")
  }else
  {
    fit_b_roll3_noFOMC2012 <- brm(data = data_roll3_2012, family = student, 
                                  r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2012, file = "./Data/fit_b_roll3_noFOMC2012.rds")
  }
  
  if(file.exists("./Data/fit_b_roll3_noFOMC2013.rds"))
  {
    fit_b_roll3_noFOMC2013 <- readRDS("./Data/fit_b_roll3_noFOMC2013.rds")
  }else
  {
    fit_b_roll3_noFOMC2013 <- brm(data = data_roll3_2013, family = student, 
                                  r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2013, file = "./Data/fit_b_roll3_noFOMC2013.rds")
  }
  
  if(file.exists("./Data/fit_b_roll3_noFOMC2014.rds"))
  {
    fit_b_roll3_noFOMC2014 <- readRDS("./Data/fit_b_roll3_noFOMC2014.rds")
  }else
  {
    fit_b_roll3_noFOMC2014 <- brm(data = data_roll3_2014, family = student, 
                                  r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2014, file = "./Data/fit_b_roll3_noFOMC2014.rds")
  }
  
  if(file.exists("./Data/fit_b_roll3_noFOMC2015.rds"))
  {
    fit_b_roll3_noFOMC2015 <- readRDS("./Data/fit_b_roll3_noFOMC2015.rds")
  }else
  {
    fit_b_roll3_noFOMC2015 <- brm(data = data_roll3_2015, family = student, 
                                  r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2015, file = "./Data/fit_b_roll3_noFOMC2015.rds")
  }
  
  
  if(file.exists("./Data/fit_b_roll3_noFOMC2016.rds"))
  {
    fit_b_roll3_noFOMC2016 <- readRDS("./Data/fit_b_roll3_noFOMC2016.rds")
  }else
  {
    fit_b_roll3_noFOMC2016 <- brm(data = data_roll3_2016, family = student, 
                                  r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2016, file = "./Data/fit_b_roll3_noFOMC2016.rds")
  }
  
  
  if(file.exists("./Data/fit_b_roll3_noFOMC2017.rds"))
  {
    fit_b_roll3_noFOMC2017 <- readRDS("./Data/fit_b_roll3_noFOMC2017.rds")
  }else
  {
    fit_b_roll3_noFOMC2017 <- brm(data = data_roll3_2017, family = student, 
                                  r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2017, file = "./Data/fit_b_roll3_noFOMC2017.rds")
  }
  
  if(file.exists("./Data/fit_b_roll3_noFOMC2018.rds"))
  {
    fit_b_roll3_noFOMC2018 <- readRDS("./Data/fit_b_roll3_noFOMC2018.rds")
  }else
  {
    fit_b_roll3_noFOMC2018 <- brm(data = data_roll3_2018, family = student, 
                                  r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon-I_fomc-I_fomc1lag-I_fomc2lag-I_fomc3lag-I_fomclags,
                                  cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                                  backend = "cmdstanr",
                                  prior = c(prior(normal(0, 10), class = Intercept),
                                            prior(normal(0, 10), class = b),
                                            prior(gamma(4, 1), class = nu),
                                            prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_roll3_noFOMC2018, file = "./Data/fit_b_roll3_noFOMC2018.rds")
  }
  
}

# data now year
if(1==2)
{
  if(file.exists("./Data/fit_b_now2008.rds"))
  {
    fit_b_now2008 <- readRDS("./Data/fit_b_now2008.rds")
  }else
  {
    fit_b_now2008 <- brm(data = data_now2008, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_now2008, file = "./Data/fit_b_now2008.rds")
  }
  
  if(file.exists("./Data/fit_b_now2009.rds"))
  {
    fit_b_now2009 <- readRDS("./Data/fit_b_now2009.rds")
  }else
  {
    fit_b_now2009 <- brm(data = data_now2009, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_now2009, file = "./Data/fit_b_now2009.rds")
  }
  
  if(file.exists("./Data/fit_b_now2010.rds"))
  {
    fit_b_now2010 <- readRDS("./Data/fit_b_now2010.rds")
  }else
  {
    fit_b_now2010 <- brm(data = data_now2010, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_now2010, file = "./Data/fit_b_now2010.rds")
  }
  
  if(file.exists("./Data/fit_b_now2011.rds"))
  {
    fit_b_now2011 <- readRDS("./Data/fit_b_now2011.rds")
  }else
  {
    fit_b_now2011 <- brm(data = data_now2011, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_now2011, file = "./Data/fit_b_now2011.rds")
  }
  
  
  if(file.exists("./Data/fit_b_now2012.rds"))
  {
    fit_b_now2012 <- readRDS("./Data/fit_b_now2012.rds")
  }else
  {
    fit_b_now2012 <- brm(data = data_now2012, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_now2012, file = "./Data/fit_b_now2012.rds")
  }
  
  if(file.exists("./Data/fit_b_now2013.rds"))
  {
    fit_b_now2013 <- readRDS("./Data/fit_b_now2013.rds")
  }else
  {
    fit_b_now2013 <- brm(data = data_now2013, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_now2013, file = "./Data/fit_b_now2013.rds")
  }
  
  if(file.exists("./Data/fit_b_now2014.rds"))
  {
    fit_b_now2014 <- readRDS("./Data/fit_b_now2014.rds")
  }else
  {
    fit_b_now2014 <- brm(data = data_now2014, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_now2014, file = "./Data/fit_b_now2014.rds")
  }
  
  if(file.exists("./Data/fit_b_now2015.rds"))
  {
    fit_b_now2015 <- readRDS("./Data/fit_b_now2015.rds")
  }else
  {
    fit_b_now2015 <- brm(data = data_now2015, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_now2015, file = "./Data/fit_b_now2015.rds")
  }
  
  
  if(file.exists("./Data/fit_b_now2016.rds"))
  {
    fit_b_now2016 <- readRDS("./Data/fit_b_now2016.rds")
  }else
  {
    fit_b_now2016 <- brm(data = data_now2016, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_now2016, file = "./Data/fit_b_now2016.rds")
  }
  
  
  if(file.exists("./Data/fit_b_now2017.rds"))
  {
    fit_b_now2017 <- readRDS("./Data/fit_b_now2017.rds")
  }else
  {
    fit_b_now2017 <- brm(data = data_now2017, family = student, r13~.-r_onfh-vixpct-usdpct-I_fomclags-vixpctlag-usdpctlag-usdlagclose-mon,
                         cores=min(4, parallel::detectCores()), iter=3000, chain=4, thin=1, control=list(max_treedepth = 12),
                         backend = "cmdstanr",
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b),
                                   prior(gamma(4, 1), class = nu),
                                   prior(cauchy(0, 1),  class = sigma)), seed = 2222)
    saveRDS(fit_b_now2017, file = "./Data/fit_b_now2017.rds")
  }
}



################
# post fitting #
################

# for one specific model
md_name <- "fit_tmp"   # <------- change model name each time
md <- get(md_name)
md_file <-  paste0("./Data/", md_name, ".rds")
if(!file.exists(md_file)) saveRDS(md, file = md_file)
print(md$fit, digits=4)
# plot(md)
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
if(1==2)
{
  md_vec <- c("fit_b", "fit_b_after2008", "fit_b_after2009", "fit_b_after2010",
              "fit_b_after2011", "fit_b_after2012", "fit_b_after2013", "fit_b_after2014",
              "fit_b_after2015", "fit_b_after2016", "fit_b_after2017")
  yr_vec <- c("All", "> 2008", "> 2009", "> 2010", "> 2011", "> 2012", "> 2013", "> 2014", "> 2015", "> 2016", "> 2017")
  
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
  
  
  pdf("./images/after_yr.pdf", 12, 12)
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
  md_vec <- c("fit_b", "fit_b_after_noFOMC2008", "fit_b_after_noFOMC2009", "fit_b_after_noFOMC2010",
              "fit_b_after_noFOMC2011", "fit_b_after_noFOMC2012", "fit_b_after_noFOMC2013", "fit_b_after_noFOMC2014",
              "fit_b_after_noFOMC2015", "fit_b_after_noFOMC2016", "fit_b_after_noFOMC2017")
  yr_vec <- c("All", "> 2008", "> 2009", "> 2010", "> 2011", "> 2012", "> 2013", "> 2014", "> 2015", "> 2016", "> 2017")
  
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
  
  
  pdf("./images/after_noFOMC_yr.pdf", 12, 12)
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
if(1==2)
{
  md_vec <- c("fit_b_before2008", "fit_b_before2009", "fit_b_before2010",
              "fit_b_before2011", "fit_b_before2012", "fit_b_before2013", "fit_b_before2014",
              "fit_b_before2015", "fit_b_before2016", "fit_b_before2017", "fit_b")
  yr_vec <- c("<= 2008", "<= 2009", "<= 2010", "<= 2011", "<= 2012", "<= 2013", "<= 2014", "<= 2015", "<= 2016", "<= 2017", "All")
  pdf("./images/before_yr.pdf", 12, 12)
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
  md_vec <- c("fit_b", "fit_b_before_noFOMC2008", "fit_b_before_noFOMC2009", "fit_b_before_noFOMC2010",
              "fit_b_before_noFOMC2011", "fit_b_before_noFOMC2012", "fit_b_before_noFOMC2013", "fit_b_before_noFOMC2014",
              "fit_b_before_noFOMC2015", "fit_b_before_noFOMC2016", "fit_b_before_noFOMC2017")
  yr_vec <- c("All", "> 2008", "> 2009", "> 2010", "> 2011", "> 2012", "> 2013", "> 2014", "> 2015", "> 2016", "> 2017")
  
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
  
  
  pdf("./images/before_noFOMC_yr.pdf", 12, 12)
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
  pdf("./images/on_plots.pdf", 12, 12)
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
if(1==2)
{
  md_vec <- c("fit_b_roll3_noFOMC2009", "fit_b_roll3_noFOMC2010",
              "fit_b_roll3_noFOMC2011", "fit_b_roll3_noFOMC2012", "fit_b_roll3_noFOMC2013", "fit_b_roll3_noFOMC2014",
              "fit_b_roll3_noFOMC2015", "fit_b_roll3_noFOMC2016", "fit_b_roll3_noFOMC2017", "fit_b_roll3_noFOMC2018", "fit_b")
  yr_vec <- c("2009(3)", "2010(3)", "2011(3)", "2012(3)", "2013(3)", "2014(3)", "2015(3)", "2016(3)", "2017(3)", "2018(3)", "All")
  
  post_data <- as.data.frame(fit_b_roll3_noFOMC2009$fit)
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
  
  
  pdf("./images/roll3_noFOMC.pdf", 12, 12)
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
if(1==2)
{
  md_name <- "fit_b_scaled"
  md <- get(md_name)
  
  # conditional effects
  pdf(paste0("./images/", md_name, "_plot_scaled.pdf"), 3, 3)
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
  pdf(paste0("./images/", md_name, "_post_scaled.pdf"), 7, 1.5)
  plot(md, ask=F, N=1)
  dev.off()
  
  # leave-one-out C.V.
  set.seed(12)
  (loo_rst <- brms::loo(md))
  pdf(paste0("./images/", md_name, "_conv.pdf"), 4, 3)
  plot(loo_rst)
  dev.off()

  set.seed(21)
  pdf(paste0("./images/", md_name, "_ppcheck.pdf"), 4, 3)
  brms::pp_check(md, ndraws=100) + ggplot2::xlim(-10, 10)
  dev.off()

  set.seed(1122)
  brms::pp_check(md, ndraws = 100, type = "scatter_avg_grouped", group="weekday") +
    ggplot2::geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)
  set.seed(4553)
  brms::pp_check(md, ndraws = 100, type = "scatter_avg_grouped", group="I_fomc2lag") +
    ggplot2::geom_abline(intercept = 0, slope = 1 , color = "red", lty = 2)  
}
