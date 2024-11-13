rm(list = setdiff(ls(),lsf.str()))
library(Rcpp)
library(data.table)
library(tidyr)
library(moments)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(ggpubr)
sourceCpp("./Code/iteration_matrix_generating.cpp", verbose = F, rebuild = T)

dat <- fread("./Data/market_timing_vis_data.csv")
dat <- data.frame(dat)
dat$year <- format(dat$date, "%Y")
dat$month <- format(dat$date, "%m")

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


pdf("./images/cumRet_returns_SPY.pdf", 9,6)
plot(cum_on, ylim = c(-0.5,1.5),xlim=c(0,3400),
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

legend(2800,1.0,
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

axis(side = 1, at = seq(0,2500,500),labels = c("2007","2009","2011","2013","2015","2017") )
dev.off()


cumRet <- data.frame(cum_onfh, cum_on, cum_12, cum_on12,cum_onfh12,
                     cum_lag, cum_lagon, cum_lag12, cum_1011,cum_10,cum_11,
                     cum_alwayslong, dat$year)

yearRet <- cumRet %>% 
  group_by(dat.year) %>%
  slice(n())

yearRet <- data.frame(yearRet)
head(yearRet)

# Create a first line

pdf("./images/yearly_returns_onfh.pdf", 5,4)
plot(yearRet$dat.year, yearRet$cum_onfh, type = "b", frame = TRUE, pch = 19, lwd=2,cex=0.6, 
     lty=1, xlab = "", ylab = "Yearly return", ylim = c(-0.25,1.5))
# Add a second line
lines(yearRet$dat.year, yearRet$cum_alwayslong, pch = 19,cex=0.6, col="red",
      type = "b", lwd=2, lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(r[onfh])), "alwayslong"),
       lty=c(1,3),cex=0.8,lwd = c(2,2),pch=c(19,19))
dev.off()

#on
pdf("./images/yearly_returns_on.pdf", 5,4)
plot(yearRet$dat.year, yearRet$cum_on, type = "b",  pch = 19, lwd=2,cex=0.6, 
     xlab = "", ylab = "Yearly return", ylim = c(-0.25,1.5))
# Add a second line
lines(yearRet$dat.year, yearRet$cum_alwayslong, pch = 19,  col="red",
      type = "b", lwd=2,lty=3, cex=0.6)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(r[on])), "alwayslong"),
       lty=c(1,3),lwd = c(2,2),pch=c(19,19),cex=0.8)
dev.off()

#12
pdf("./images/yearly_returns_12.pdf", 5,4)
plot(yearRet$dat.year, yearRet$cum_12, type = "b",  pch = 19, lwd=2,cex=0.6, 
     xlab = "", ylab = "Yearly return", ylim = c(-0.25,1.5))
# Add a second line
lines(yearRet$dat.year, yearRet$cum_alwayslong, pch = 19, cex=0.6,
      col = "red", type = "b", lwd=2, lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(r[12])), "alwayslong"),
       lty=c(1,3),lwd = c(2,2),pch=c(19,19),cex=0.8, col=c("black","red"))
dev.off()

pdf("./images/yearly_returns_13lag.pdf", 5,4)
#13lag
plot(yearRet$dat.year, yearRet$cum_lag, type = "b",  pch = 19, lwd=2,cex=0.6, 
      xlab = "", ylab = "Yearly return", ylim = c(-0.25,1.5))
# Add a second line
lines(yearRet$dat.year, yearRet$cum_alwayslong, pch = 19, cex=0.6,
      col = "red", type = "b", lwd=2,lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(r[lag])), "alwayslong"),
       lty=c(1,3),lwd = c(2,2),pch=c(19,19),cex=0.8)

dev.off()

#onfh12
pdf("./images/yearly_returns_onfh12.pdf", 5,4)
plot(yearRet$dat.year, yearRet$cum_onfh12, type = "b",  pch = 19, lwd=2,cex=0.6, 
    xlab = "", ylab = "Yearly return", ylim = c(-0.25,1.5))
# Add a second line
lines(yearRet$dat.year, yearRet$cum_alwayslong, pch = 19, cex=0.6,  col="red",
      type = "b", lwd=2,lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(r[onfh], r[12])), "alwayslong"),
       lty=c(1,3),lwd = c(2,2),pch=c(19,19),cex=0.8)
dev.off()

#on12
pdf("./images/yearly_returns_on12.pdf", 5,4)
plot(yearRet$dat.year, yearRet$cum_on12, type = "b",  pch = 19, lwd=2,cex=0.6, 
     xlab = "", ylab = "Yearly return", ylim = c(-0.25,1.5))
# Add a second line
lines(yearRet$dat.year, yearRet$cum_alwayslong, pch = 19, cex=0.6,  col="red",
       type = "b", lwd=2,lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(r[on], r[12])), "alwayslong"),
       lty=c(1,3),lwd = c(2,2),pch=c(19,19),cex=0.8)
dev.off()

#lagon
pdf("./images/yearly_returns_lagon.pdf", 5,4)
plot(yearRet$dat.year, yearRet$cum_lagon, type = "b",  pch = 19, lwd=2,cex=0.6, 
     xlab = "", ylab = "Yearly return", ylim = c(-0.25,1.5))
# Add a second line
lines(yearRet$dat.year, yearRet$cum_alwayslong, pch = 19, cex=0.6,  col="red",
       type = "b", lwd=2,lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(r[lag], r[on])), "alwayslong"),
       lty=c(1,3),lwd = c(2,2),pch=c(19,19),cex=0.8)
dev.off()

#lag12
pdf("./images/yearly_returns_lag12.pdf", 5,4)
plot(yearRet$dat.year, yearRet$cum_lag12, type = "b",  pch = 19, lwd=2,cex=0.6, 
     xlab = "", ylab = "Yearly return", ylim = c(-0.25,1.5))
# Add a second line
lines(yearRet$dat.year, yearRet$cum_alwayslong, pch = 19, cex=0.6,  col="red",
       type = "b", lwd=2,lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(r[lag], r[12])), "alwayslong"),
       lty=c(1,3),lwd = c(2,2),pch=c(19,19),cex=0.8)
dev.off()

#1011
pdf("./images/yearly_returns_1011.pdf", 5,4)
plot(yearRet$dat.year, yearRet$cum_1011, type = "b",  pch = 19, lwd=2,cex=0.6, 
     xlab = "", ylab = "Yearly return", ylim = c(-0.25,1.5))
# Add a second line
lines(yearRet$dat.year, yearRet$cum_alwayslong, pch = 19, cex=0.6,  col="red",
       type = "b", lwd=2,lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(r[10], r[11])), "alwayslong"),
       lty=c(1,3),lwd = c(2,2),pch=c(19,19),cex=0.8)
dev.off()

# buyand hold
pdf("./images/yearly_returns_buyandhold.pdf", 5,4)
plot(yearRet$dat.year, yearRet$cum_buyandhold, type = "b",  pch = 19, lwd=2,cex=0.6, 
    xlab = "", ylab = "Yearly return", ylim = c(-0.25,1.5))
# Add a second line
lines(yearRet$dat.year, yearRet$cum_alwayslong, pch = 19, cex=0.6,  col="red",
      type = "b", lwd=2,lty=3)
# Add a legend to the plot
legend("topleft", legend=c("buyandhold", "alwayslong"),
       lty=c(1,3),lwd = c(2,2),pch=c(19,19),cex=0.8)
dev.off()


pdf("./images/yearly_returns_10.pdf", 5,4)
plot(yearRet$dat.year, yearRet$cum_10, type = "b",  pch = 19, lwd=2,cex=0.6, 
    xlab = "", ylab = "Yearly return", ylim = c(-0.25,1.5))
# Add a second line
lines(yearRet$dat.year, yearRet$cum_alwayslong, pch = 19, cex=0.6,  col="red",
       type = "b", lwd=2,lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(r[10])), "alwayslong"),
       lty=c(1,3),lwd = c(2,2),pch=c(19,19),cex=0.8)
dev.off()

pdf("./images/yearly_returns_11.pdf", 5,4)
plot(yearRet$dat.year, yearRet$cum_11, type = "b",  pch = 19, lwd=2,cex=0.6, 
    xlab = "", ylab = "Yearly return", ylim = c(-0.25,1.5))
# Add a second line
lines(yearRet$dat.year, yearRet$cum_alwayslong, pch = 19, cex=0.6,  col="red",
       type = "b", lwd=2,lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(r[11])), "alwayslong"),
       lty=c(1,3),lwd = c(2,2),pch=c(19,19),cex=0.8)
dev.off()

