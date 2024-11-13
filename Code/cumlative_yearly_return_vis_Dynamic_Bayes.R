rm(list = setdiff(ls(),lsf.str()))
library(Rcpp)
library(data.table)
library(tidyr)
#library(moments)
library(tidyverse)
#library(cowplot)
#library(ggplot2)
#library(ggpubr)
sourceCpp("./Code/iteration_matrix_generating.cpp", verbose = F, rebuild = T)

dat <- fread("./Data/market_timing_vis_data_dynamic.csv")
dat <- data.frame(dat)


head(dat)

dat$cum_on <- cumRet_calculation(dat$d_on,dat$r13)
dat$cum_Don <- cumRet_calculation(dat$d_Dron,dat$r13)
dat$cum_T33 <- cumRet_calculation(dat$d_A33,dat$r13)
dat$cum_T312 <- cumRet_calculation(dat$d_A312,dat$r13)
dat$cum_T53 <- cumRet_calculation(dat$d_A53,dat$r13)
dat$cum_T512 <- cumRet_calculation(dat$d_A512,dat$r13)
dat$cum_lag <- cumRet_calculation(dat$d_lag,dat$r13)
dat$cum_Dlag <- cumRet_calculation(dat$d_D13lag,dat$r13)
dat$cum_alwayslong <- cumRet_benchmark(dat$r13)

#subdat <- dat[dat$year>=2017,]

 pdf("./images/cumRet_returns_SPY_includedDynamic.pdf", 9,6)
 plot(dat$cum_on, ylim = c(-0.1,0.4),xlim=c(0,2200),
      col = "blue", type = "l", lwd = 1, xaxt="n",
      ylab = "Cumulative Return",xlab = "")
 lines(dat$cum_Don, col="grey",lwd=1,lty=2)
 lines(dat$cum_T33, col="yellow", lwd=1, lty=2)
 lines(dat$cum_T312, col="orange", lwd=1, lty=2)
 lines(dat$cum_T53, col="#CC6666", lwd=1, lty=2)
 lines(dat$cum_T512, col="#660066", lwd=1, lty=2)
 lines(dat$cum_lag, col="black",lwd=1,lty=1)
 lines(dat$cum_Dlag, col = "red",lwd=1, lty=2)
 lines(dat$cum_alwayslong, col="green", lwd = 1)
 legend("topleft",
        legend=c(expression(ring(eta)(r[on])), 
                 expression(eta(r[on])),
                 expression(eta(T[3][","][3])),
                 expression(eta(T[3][","][12])),
                 expression(eta(T[5][","][3])),
                 expression(eta(T[5][","][12])),
                 expression(ring(eta)(r[lag])),
                 expression(eta(r[lag])),
                 "AL"),
        col=c("blue", "grey","yellow","orange","#CC6666","#660066","black","red","green"), 
        lwd=c(1,1,1,1,1,1,1,1,1),lty=c(1,2,2,2,2,2,1,2,1),
        cex=1.0)
 
 axis(side = 1, at = seq(0,2000,500),labels = c("2010","2011","2014","2016","2018"))
 dev.off()


cumRet <- dat%>%select(cum_on, cum_Don, cum_Dlag, cum_lag,
                       cum_alwayslong, year, cum_T33, cum_T312, cum_T53,cum_T512)

yearRet <- cumRet %>% 
  group_by(year) %>%
  slice(n())

yearRet <- data.frame(yearRet)
head(yearRet)

#ron
pdf("./images/yearly_returns_on_rolling3.pdf", 5,4)
plot(yearRet$year, yearRet$cum_on, type = "b", frame = TRUE, pch = 19, lwd=1,cex=0.6, 
     lty=1, xlab = "", ylab = "Cumulative Return", ylim = c(-0.15,0.45))
lines(yearRet$year, yearRet$cum_alwayslong, pch = 19,cex=0.6, col="red",
      type = "b", lwd=1, lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(ring(eta)(r[on])), "AL"),
       lty=c(1,3),cex=1.0,lwd = c(2,2),pch=c(19,19), col = c("black","red"))
dev.off()

# dynamic ron
pdf("./images/yearly_returns_Dynamicon_rolling.pdf", 5,4)
plot(yearRet$year, yearRet$cum_Don, type = "b", frame = TRUE, pch = 19, lwd=1,cex=0.6, 
     lty=1, xlab = "", ylab = "Cumulative Return", ylim = c(-0.15,0.45))
# Add a second line
lines(yearRet$year, yearRet$cum_alwayslong, pch = 19,cex=0.6, col="red",
      type = "b", lwd=1, lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(r[on])), "AL"),
       lty=c(1,3),cex=1.0,lwd = c(2,2),pch=c(19,19), col = c("black","red"))
dev.off()

# lag
pdf("./images/yearly_returns_lag_rolling3.pdf", 5,4)
plot(yearRet$year, yearRet$cum_lag, type = "b", frame = TRUE, pch = 19, lwd=1,cex=0.6, 
     lty=1, xlab = "", ylab = "Cumulative Return", ylim = c(-0.15,0.45))
# Add a second line
lines(yearRet$year, yearRet$cum_alwayslong, pch = 19,cex=0.6, col="red",
      type = "b", lwd=1, lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(ring(eta)(r[lag])), "AL"),
       lty=c(1,3),cex=1.0,lwd = c(2,2),pch=c(19,19), col = c("black","red"))
dev.off()

#dynamic lag
pdf("./images/yearly_returns_Dynamiclag_rolling3.pdf", 5,4)
plot(yearRet$year, yearRet$cum_Dlag, type = "b", frame = TRUE, pch = 19, lwd=1,cex=0.6, 
     lty=1, xlab = "", ylab = "Cumulative Return", ylim = c(-0.15,0.45))
# Add a second line
lines(yearRet$year, yearRet$cum_alwayslong, pch = 19,cex=0.6, col="red",
      type = "b", lwd=1, lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(r[lag])), "AL"),
       lty=c(1,3),cex=1.0,lwd = c(2,2),pch=c(19,19), col = c("black","red"))
dev.off()

#dynamic aggressive
pdf("./images/yearly_returns_DynamicA33_rolling3.pdf", 5,4)
plot(yearRet$year, yearRet$cum_T33, type = "b", frame = TRUE, pch = 19, lwd=1,cex=0.6, 
     lty=1, xlab = "", ylab = "Cumulative Return", ylim = c(-0.15,0.45))
# Add a second line
lines(yearRet$year, yearRet$cum_alwayslong, pch = 19,cex=0.6, col="red",
      type = "b", lwd=1, lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(T[3][","][3])), "AL"),
       lty=c(1,3),cex=1.0,lwd = c(2,2),pch=c(19,19), col = c("black","red"))
dev.off()

pdf("./images/yearly_returns_DynamicA312_rolling3.pdf", 5,4)
plot(yearRet$year, yearRet$cum_T312, type = "b", frame = TRUE, pch = 19, lwd=1,cex=0.6, 
     lty=1, xlab = "", ylab = "Cumulative Return", ylim = c(-0.15,0.45))
# Add a second line
lines(yearRet$year, yearRet$cum_alwayslong, pch = 19,cex=0.6, col="red",
      type = "b", lwd=1, lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(T[3][","][12])), "AL"),
       lty=c(1,3),cex=1.0,lwd = c(2,2),pch=c(19,19), col = c("black","red"))
dev.off()

pdf("./images/yearly_returns_DynamicA53_rolling3.pdf", 5,4)
plot(yearRet$year, yearRet$cum_T53, type = "b", frame = TRUE, pch = 19, lwd=1,cex=0.6, 
     lty=1, xlab = "", ylab = "Cumulative Return", ylim = c(-0.15,0.45))
# Add a second line
lines(yearRet$year, yearRet$cum_alwayslong, pch = 19,cex=0.6, col="red",
      type = "b", lwd=1, lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(T[5][","][3])), "AL"),
       lty=c(1,3),cex=1.0,lwd = c(2,2),pch=c(19,19), col = c("black","red"))
dev.off()

pdf("./images/yearly_returns_DynamicA512_rolling3.pdf", 5,4)
plot(yearRet$year, yearRet$cum_T512, type = "b", frame = TRUE, pch = 19, lwd=1,cex=0.6, 
     lty=1, xlab = "", ylab = "Cumulative Return", ylim = c(-0.15,0.45))
# Add a second line
lines(yearRet$year, yearRet$cum_alwayslong, pch = 19,cex=0.6, col="red",
      type = "b", lwd=1, lty=3)
# Add a legend to the plot
legend("topleft", legend=c(expression(eta(T[5][","][12])), "AL"),
       lty=c(1,3),cex=1.0,lwd = c(2,2),pch=c(19,19), col = c("black","red"))
dev.off()

