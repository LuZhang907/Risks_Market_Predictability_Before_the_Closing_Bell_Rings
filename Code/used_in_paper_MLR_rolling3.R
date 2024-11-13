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



## weekday and months
data$weekday <- wday(data$date) - 1
data$weekday <- as.factor(data$weekday)
data$mon <- month(data$date)
data$mon <- as.factor(data$mon)


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
if(1==1)
{
  names(data)
  # plot(data$r13)
  fit_m <- lm(r13~.-r_onfh-vixpct-usdpct-mon-vixpctlag-usdlagclose-usdpctlag, data = data)
  
  coeff <- data.frame(summary(fit_m)$coefficients[,1])
  Lci <- confint(fit_m)[,1]
  Uci <- confint(fit_m)[,2]
  
  rst <- data.frame( Lci,coeff, Uci)
  names(rst) <- c("2.5%", "coeff", "97.5%")
  rst$sig <- "No"
  rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
  xtable::xtable(rst, digits = -2, caption = "Multiple linear regression model with Student-t error terms.")
  saveRDS(fit_m, file = "./Data/fit_m.rds")
}

# data three years rolling
if(1==1)
{
  if(file.exists("./Data/fit_m_roll3_2009.rds"))
  {
    fit_m_roll3_2009 <- readRDS("./Data/fit_m_roll3_2009.rds")
  }else
  {
    fit_m_roll3_2009 <- lm(r13~.-r_onfh-vixpct-usdpct-mon-vixpctlag-usdlagclose-usdpctlag, data = data_roll3_2009)
    saveRDS(fit_m_roll3_2009, file = "./Data/fit_m_roll3_2009.rds")
  }
  
  if(file.exists("./Data/fit_m_roll3_2010.rds"))
  {
    fit_m_roll3_2010 <- readRDS("./Data/fit_m_roll3_2010.rds")
  }else
  {
    fit_m_roll3_2010 <- lm(r13~.-r_onfh-vixpct-usdpct-mon-vixpctlag-usdlagclose-usdpctlag, data = data_roll3_2010)
    saveRDS(fit_m_roll3_2010, file = "./Data/fit_m_roll3_2010.rds")
  }
  
  if(file.exists("./Data/fit_m_roll3_2011.rds"))
  {
    fit_m_roll3_2011 <- readRDS("./Data/fit_m_roll3_2011.rds")
  }else
  {
    fit_m_roll3_2011 <- lm(r13~.-r_onfh-vixpct-usdpct-mon-vixpctlag-usdlagclose-usdpctlag, data = data_roll3_2011)
    saveRDS(fit_m_roll3_2011, file = "./Data/fit_m_roll3_2011.rds")
  }
  
  
  if(file.exists("./Data/fit_m_roll3_2012.rds"))
  {
    fit_m_roll3_2012 <- readRDS("./Data/fit_m_roll3_2012.rds")
  }else
  {
    fit_m_roll3_2012 <- lm(r13~.-r_onfh-vixpct-usdpct-mon-vixpctlag-usdlagclose-usdpctlag, data = data_roll3_2012)
    saveRDS(fit_m_roll3_2012, file = "./Data/fit_m_roll3_2012.rds")
  }
  
  if(file.exists("./Data/fit_m_roll3_2013.rds"))
  {
    fit_m_roll3_2013 <- readRDS("./Data/fit_m_roll3_2013.rds")
  }else
  {
    fit_m_roll3_2013 <- lm(r13~.-r_onfh-vixpct-usdpct-mon-vixpctlag-usdlagclose-usdpctlag, data = data_roll3_2013)
    saveRDS(fit_m_roll3_2013, file = "./Data/fit_m_roll3_2013.rds")
  }
  
  if(file.exists("./Data/fit_m_roll3_2014.rds"))
  {
    fit_m_roll3_2014 <- readRDS("./Data/fit_m_roll3_2014.rds")
  }else
  {
    fit_m_roll3_2014 <- lm(r13~.-r_onfh-vixpct-usdpct-mon-vixpctlag-usdlagclose-usdpctlag, data = data_roll3_2014)
    saveRDS(fit_m_roll3_2014, file = "./Data/fit_m_roll3_2014.rds")
  }
  
  if(file.exists("./Data/fit_m_roll3_2015.rds"))
  {
    fit_m_roll3_2015 <- readRDS("./Data/fit_m_roll3_2015.rds")
  }else
  {
    fit_m_roll3_2015 <- lm(r13~.-r_onfh-vixpct-usdpct-mon-vixpctlag-usdlagclose-usdpctlag, data = data_roll3_2015)
    saveRDS(fit_m_roll3_2015, file = "./Data/fit_m_roll3_2015.rds")
  }
  
  
  if(file.exists("./Data/fit_m_roll3_2016.rds"))
  {
    fit_m_roll3_2016 <- readRDS("./Data/fit_m_roll3_2016.rds")
  }else
  {
    fit_m_roll3_2016 <- lm(r13~.-r_onfh-vixpct-usdpct-mon-vixpctlag-usdlagclose-usdpctlag, data = data_roll3_2016)
    saveRDS(fit_m_roll3_2016, file = "./Data/fit_m_roll3_2016.rds")
  }
  
  
  if(file.exists("./Data/fit_m_roll3_2017.rds"))
  {
    fit_m_roll3_2017 <- readRDS("./Data/fit_m_roll3_2017.rds")
  }else
  {
    fit_m_roll3_2017 <- lm(r13~.-r_onfh-vixpct-usdpct-mon-vixpctlag-usdlagclose-usdpctlag, data = data_roll3_2017)
    saveRDS(fit_m_roll3_2017, file = "./Data/fit_m_roll3_2017.rds")
  }
  
  if(file.exists("./Data/fit_m_roll3_2018.rds"))
  {
    fit_m_roll3_2018 <- readRDS("./Data/fit_m_roll3_2018.rds")
  }else
  {
    fit_m_roll3_2018 <- lm(r13~.-r_onfh-vixpct-usdpct-mon-vixpctlag-usdlagclose-usdpctlag, data = data_roll3_2018)
    saveRDS(fit_m_roll3_2018, file = "./Data/fit_m_roll3_2018.rds")
  }
  
}
################
# post fitting #
################

# for one specific model
md_name <- "fit_m_roll3_2009"   # <------- change model name each time
md <- get(md_name)
md_file <-  paste0("./Data/", md_name, ".rds")
if(!file.exists(md_file)) saveRDS(md, file = md_file)
#print(md$fit, digits=4)
# plot(md)

coeff <- data.frame(summary(md)$coefficients[,1])
Lci <- confint(md)[,1]
Uci <- confint(md)[,2]

rst <- data.frame( Lci,coeff, Uci)
names(rst) <- c("2.5%", "coeff", "97.5%")
rst$sig <- "No"
rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"

xtable::xtable(rst, digits = -2, caption = "Multiple linear regression model ")

var_vec <-row.names(rst)


# data three years rolling 
if(1==1)
{
  md_vec <- c("fit_m_roll3_2009", "fit_m_roll3_2010",
              "fit_m_roll3_2011", "fit_m_roll3_2012", "fit_m_roll3_2013", "fit_m_roll3_2014",
              "fit_m_roll3_2015", "fit_m_roll3_2016", "fit_m_roll3_2017", "fit_m_roll3_2018", "fit_m")
  yr_vec <- c("2009(3)", "2010(3)", "2011(3)", "2012(3)", "2013(3)", "2014(3)", "2015(3)", "2016(3)", "2017(3)", "2018(3)", "All")
  
  coeff <- data.frame(summary(fit_m_roll3_2009)$coefficients[,1])
  Lci <- confint(md)[,1]
  Uci <- confint(md)[,2]
  
  rst <- data.frame( Lci,coeff, Uci)
  names(rst) <- c("2.5%", "coeff", "97.5%")
  rst$sig <- "No"
  rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
  var_vec <-row.names(rst)
  
  
  pdf("./images/MLRroll3_.pdf", 12, 10)
  par(mfrow = c(4, 5), mar = c(5,3,4,1.2))
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
      coeff <- data.frame(summary(md)$coefficients[,1])
      Lci <- confint(md)[,1]
      Uci <- confint(md)[,2]
      
      rst <- data.frame( Lci,coeff, Uci)
      names(rst) <- c("2.5%", "coeff", "97.5%")
      rst$sig <- "No"
      rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
      rst_all <- rbind(rst_all, c(rst[var_now,], yr_vec[ii]))
    }
    rst_all <- data.frame(rst_all)
    names(rst_all) <- c("2.5%", "coeff", "97.5%", "sig", "Period")
    me <- unlist(rst_all$`coeff`)
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

