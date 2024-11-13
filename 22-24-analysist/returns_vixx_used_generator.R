rm(list = setdiff(ls(),lsf.str()))
library(data.table)
library(dplyr)
library(stringr)
library(car)
library(lubridate)

vix <- fread("/Users/luzhang/Desktop/Risks/data/VIX.csv")
usd <- fread("/Users/luzhang/Desktop/Risks/data/usd_index.csv")
dat <- fread("/Users/luzhang/Desktop/Risks/data/returns_QQQ.csv")

usd$Date <- as.integer(format(as.Date(usd$Date, format = "%d-%b-%y"), "%Y%m%d"))
usd <- usd[order(usd$Date),]

colnames(vix) <- c("Date","Open", "High", "Low", "Close")
vix$Date <- as.Date(vix$Date, format = "%m/%d/%Y")
vix$Date <- as.integer(format(vix$Date, "%Y%m%d"))
vix <- vix[order(vix$Date),]

dat <- dat[order(dat$date),]

vix <- data.frame(vix)
usd <- data.frame(usd)
dat <- data.frame(dat)

head(vix)
head(dat)
head(usd)


# previous day close price
vix$vixlagclose <- lag(vix$Close)
usd$usdlagclose <- lag(usd$Close)

# day to day close price change
vix$vixpct <- (vix$Close-lag(vix$Close))/lag(vix$Close)
usd$usdpct <- (usd$Close-lag(usd$Close))/lag(usd$Close)

vix$vixpctlag <- lag(vix$vixpct)
usd$usdpctlag <-  lag(usd$usdpct)

vix$date <- vix$Date
usd$date <- usd$Date


vix <- vix %>% select(date, vixlagclose, vixpctlag)
usd <- usd %>% select(date, usdlagclose, usdpctlag)


dim(dat)
dim(vix)
dim(usd)

vix_usd <- merge(vix, usd, by="date")
data <- merge(dat, vix_usd, by="date")
data <- na.omit(data)

dim(data)
head(data)

fwrite(data, "/Users/luzhang/Desktop/Risks/data/returns_vix_usd_QQQ.csv")
