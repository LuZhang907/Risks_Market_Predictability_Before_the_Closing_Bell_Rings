rm(list = setdiff(ls(),lsf.str()))
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)

dat <- fread("/Users/luzhang/Desktop/Risks/data/qqq.csv")
dat <- data.frame(dat)
colnames(dat) <- c("Time", "Open","High", "Low","Close", "Volume", "Date")
head(dat)
tail(dat)
dim(dat)

# convert milisecond records to time format
seconds <- dat$Time / 1000

# Convert seconds to time format (assuming the origin is midnight)
time <- as.POSIXct(seconds, origin = "1970-01-01", tz = "ETD") #UTC


# Format the time in "%H:%M:%S"
dat$Time <- format(time, "%H:%M:%S")
head(dat)

# 20160101-2024-09-30
dat <- dat %>%
  filter(Date >= 20151231 & Date <= 20240930)

# convert centi-prices to dollars
dat <- dat %>%
  mutate(across(c(Open, High, Low, Close), ~./10000))

head(dat)
str(dat)

keeps <- c("09:31:00","10:00:00","10:30:00","11:00:00","11:30:00","12:00:00",
           "12:30:00","13:00:00","13:30:00","14:00:00","14:30:00","15:00:00",
           "15:30:00","16:00:00")

all_combinations <- expand.grid(Date = unique(dat$Date), Time = keeps)

df_complete <- all_combinations %>%
  left_join(dat, by = c("Date", "Time"))

df_complete <- df_complete %>%
  arrange(Date, Time)

rows_with_na <- df_complete[rowSums(is.na(df_complete)) > 0, ] %>%
  select(Date, Time)

# for the rows with nas, back to orignal data
dat_join <- merge(dat, rows_with_na, by = c("Date","Time"), all=TRUE)


# Filling NA in Close column with next row's Open value
#dat_join <- dat_join %>%
#  mutate(close_ = ifelse(is.na(Close), ifelse(is.na(lead(Open)),lead(Open, n=2),lead(Open)), Close))

dat_join <- dat_join %>%
  mutate(close_ = ifelse(is.na(Close), lead(Open), Close))

dim(dat_join)


keepsdat <- dat_join[dat_join$Time %in% keeps,] %>% 
  select(Date, Time, close_)%>%
  filter(!is.na(close_))

head(keepsdat)
dim(keepsdat)
tail(keepsdat)

# count total time points per day
daypoints <- keepsdat %>% 
  group_by(Date) %>%
  summarise(Freq = n())

daypoints <- data.frame(daypoints)
dim(daypoints)
head(daypoints)

# exclude the day which time points less then 14
dropdays <- daypoints[daypoints$Freq<14,]$Date

data <- keepsdat[!keepsdat$Date %in% dropdays,]
dim(data)

#### combine with factor
# introduce the ex-dividend date and dividend
factor <-  fread("/Users/luzhang/Desktop/Risks/data/factor/qqq_factor.csv")
factor <- data.frame(factor)
head(factor)

factor$Ex.Dividend.Date <- as.integer(format(as.Date(factor$Ex.Dividend.Date, format = "%d-%b-%y"), "%Y%m%d"))
factor$Cash.Amount <- as.numeric(gsub("[$]", "", factor$Cash.Amount))

data_factor <- data %>%
  left_join(factor, by = c("Date"="Ex.Dividend.Date"))

data_factor <- data_factor %>%
  mutate(
    # Normal overnight return
    r_on = ifelse(
      Time == "09:31:00",
      ifelse(!is.na(Cash.Amount),  # Check if there's a dividend on this date
             (close_ + Cash.Amount) / lag(close_) - 1,  # Ex-dividend calculation
             close_ / lag(close_) - 1),              # Regular calculation
      NA
    )
  )

ron <- data_factor$r_on

r_on = data_factor %>%
  select(r_on) %>%
  filter(!is.na(r_on))

# calculate returns
rs <- data$close_/lag(data$close_)-1
returns <- data.frame(data$Date,data$Time,rs)
names(returns) <- c("date","Time","rs")
head(returns) 

#write.csv(returns,"./Data/returns_test.csv",row.names = FALSE)

#r_on <- returns[returns$Time=="09:31:00",]$rs
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

rs <- cbind(date,r13_lag, r1, r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13)
rs <- data.frame(rs)

rs <- rs[rs$date>=20160101,]
rs$r_on <- r_on$r_on

head(rs)
tail(rs)
dim(rs)

fwrite(rs,"/Users/luzhang/Desktop/Risks/data/returns_QQQ.csv")
