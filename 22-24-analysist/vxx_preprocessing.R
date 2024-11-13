rm(list = setdiff(ls(),lsf.str()))
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)

dat <- fread("/Users/luzhang/Desktop/Risks/data/vxx.csv")
dat <- data.frame(dat)
colnames(dat) <- c("Time", "Open","High", "Low","Close", "Volume", "Date")
head(dat)
tail(dat)
dim(dat)

# convert milisecond records to time format
seconds <- dat$Time / 1000

# Convert seconds to time format (assuming the origin is midnight)
time <- as.POSIXct(seconds, origin = "1970-01-01", tz = "EDT") #UTC


# Format the time in "%H:%M:%S"
dat$Time <- format(time, "%H:%M:%S")
head(dat)

# 20160101-2024-09-30
dat <- dat %>%
  filter(Date >= 20151231 & Date <= 20240930)

# convert centi-prices to dollars
dat <- dat %>%
  mutate(across(c(Open, High, Low, Close), ~./10000))%>%
  group_by(Date) %>%
  slice(n())


dat <- data.frame(dat)

head(dat)
str(dat)



write.csv(dat,"/Users/luzhang/Desktop/Risks/data/vxx_sliced.csv",row.names = FALSE)
