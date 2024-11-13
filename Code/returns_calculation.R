rm(list = setdiff(ls(),lsf.str()))
library(data.table)
#library(flipTime)
library(dplyr)
library(stringr)
library(car)
library(lubridate)

dat <- fread("/Users/luzhang/Desktop/minutes_data_used_in_paper/SPY.csv")
dat <- data.frame(dat)

dat$date <- as.Date(dat$time)
dat$Time <- format(dat$time, format = "%H:%M:%S")
dim(dat)

head(dat)
str(dat)

keeps <- c("09:31:00","10:00:00","10:30:00","11:00:00","11:30:00","12:00:00",
           "12:30:00","13:00:00","13:30:00","14:00:00","14:30:00","15:00:00",
           "15:30:00","16:00:00")
keepsdat <- dat[dat$Time %in% keeps,]
head(keepsdat)
dim(keepsdat)
tail(keepsdat)

# count total time points per day
daypoints <- keepsdat %>% 
  group_by(date) %>%
  summarise(Freq = n())

daypoints <- data.frame(daypoints)
dim(daypoints)

# exclude the day which time points less then 14
dropdays <- daypoints[daypoints$Freq<14,]$date
data <- keepsdat[!keepsdat$date %in% dropdays,]
dim(data)

# calculate returns per day
#returns <- data %>% 
#   group_by(date) %>%
#   mutate(rs = close/lag(close)-1)

#returns <- data.frame(returns)
#head(returns)

rs <- data$close/lag(data$close)-1
returns <- data.frame(data$date,data$Time,rs)
names(returns) <- c("date","Time","rs")
head(returns) 

#write.csv(returns,"./Data/returns_test.csv",row.names = FALSE)

r_on <- returns[returns$Time=="09:31:00",]$rs
r1 <- returns[returns$Time == "10:00:00",]$rs
r2 <- returns[returns$Time == "10:30:00",]$rs
r3 <- returns[returns$Time == "11:00:00",]$rs
r4 <- returns[returns$Time == "11:30:00",]$rs
r5 <- returns[returns$Time == "12:00:00",]$rs
r6 <- returns[returns$Time == "12:30:00",]$rs
r7 <- returns[returns$Time == "13:00:00",]$rs
r8 <- returns[returns$Time == "13:30:00",]$rs
r9 <- returns[returns$Time == "14:00:00",]$rs
r10 <- returns[returns$Time == "14:30:00",]$rs
r11 <- returns[returns$Time == "15:00:00",]$rs
r12 <- returns[returns$Time == "15:30:00",]$rs
r13 <- returns[returns$Time == "16:00:00",]$rs # response variable
r13_lag <- lag(r13) # previous day last 30 minutes return

date <- returns[returns$Time == "16:00:00",]["date"]

rs <- cbind(date,r13_lag, r_on, r1, r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13)
rs <- data.frame(rs)

head(rs)
tail(rs)
dim(rs)

write.csv(rs,"./Data/returns_SPY.csv",row.names = FALSE)
