rm(list = setdiff(ls(),lsf.str()))
library(Rcpp)
library(data.table)
library(tidyr)
#library(moments)
library(tidyverse)
#library(cowplot)
library(ggplot2)
#library(ggpubr)
sourceCpp("./Code/iteration_matrix_generating.cpp", verbose = F, rebuild = T)

dat <- fread("./Data/market_timing_vis_data.csv")
dat <- data.frame(dat)
dat$year <- format(dat$date, "%Y")
dat$month <- format(dat$date, "%m")

# 2019 and after
dat <- dat[dat$year>=2009,]
head(dat)

cols <- c(names(dat)[grepl("d_",names(dat))],"r13","r_daily")
subdat <- dat[names(dat)%in%cols]
head(subdat)

sapply(subdat, function(x) c("# of Neural" = length(x[x==0]),
                          "# of Long" = length(x[x>0]),
                          "# of Short" = length(x[x<0])
                          
))


subdat$year <- dat$year
subdat2018 <- subdat[subdat$year=="2018",]
subdat2018$year <- NULL

sapply(subdat2018, function(x) c("# of Neural" = length(x[x==0]),
                             "# of Long" = length(x[x>0]),
                             "# of Short" = length(x[x<0])
                             
))

cum_onfh <- cumRet_calculation(dat$d_onfh,dat$r13)
cum_on <- cumRet_calculation(dat$d_on,dat$r13)
cum_onfh12 <- cumRet_calculation(dat$d_onfh12,dat$r13)
cum_12 <- cumRet_calculation(dat$d_12,dat$r13)
cum_on12 <- cumRet_calculation(dat$d_on12,dat$r13)
cum_lag <- cumRet_calculation(dat$d_lag,dat$r13)
cum_lagon <- cumRet_calculation(dat$d_lagon,dat$r13)
cum_lag12 <- cumRet_calculation(dat$d_lag12,dat$r13)
cum_10 <- cumRet_calculation(dat$d_10,dat$r13)
cum_11 <- cumRet_calculation(dat$d_11,dat$r13)
cum_1011 <- cumRet_calculation(dat$d_1011,dat$r13)

cum_alwayslong <- cumRet_benchmark(dat$r13)
#cum_buyandhold <- cumRet_benchmark(dat$r_daily)


pdf("./images/cumRet_returns_2019after_SPY.pdf", 9,6)
plot(cum_on, ylim = c(-0.25,0.5),xlim=c(0,3000),
     col = "blue", type = "l", lwd = 2, xaxt="n",
     ylab = "Cumulative Return",xlab = "")
lines(cum_onfh, col="#009999", lwd=2)
lines(cum_12, col = "green", lwd = 2)
lines(cum_lag, col = "#FF6600", lwd = 2)
lines(cum_10, col="#CC6666",lwd=2)
lines(cum_11, col = "#CC3999",lwd=2)

lines(cum_onfh12, col="#99CCFF", lwd =2)
lines(cum_on12, col = "#0099CC", lwd=2)
lines(cum_lag12, col = "#336699",lwd=2)
lines(cum_lagon, col = "#660000",lwd=2)
lines(cum_1011, col="black",lwd=2)

lines(cum_alwayslong, col="red", lwd = 2)
#lines(cum_buyandhold, col="grey", lwd =2)

legend(2450,0.40,
       legend=c(expression(eta(r[on])), 
                expression(eta(r[onfh])),
                expression(eta(r[12])),
                expression(eta(r[lag])),
                expression(eta(r[10])),
                expression(eta(r[11])),
                expression(eta(r[onfh],r[12])),
                expression(eta(r[on],r[12])),
                expression(eta(r[lag],r[12])),
                expression(eta(r[lag],r[on])),
                expression(eta(r[10],r[11])),
                "alwayslong"),
       col=c("blue", "#009999","green","#FF6600","#CC6666","#CC3999",
             "#99CCFF","#0099CC","#336699","#660000","black",
             "red"
             ), 
       lty=c(1,1,1,1,1,1,1,1,1,1,1,1),
       lwd=c(2,2,2,2,2,2,2,2,2,2,2,2),cex=0.8)

axis(side = 1, at = seq(0,2000,500),labels = c("2009","2010","2012","2015","2017") )
dev.off()


