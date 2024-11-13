rm(list = setdiff(ls(),lsf.str()))
library(data.table)
library(dplyr)
library(stringr)
library(car)
library(moments)
library(lubridate)

data <- fread("./Data/returns_vix_usd_SPY.csv")
head(data)
dim(data)

data$date<-NULL

tableS <- sapply(data, function(x) c("obs" = round(length(x),0),
                                          "mean" = round(mean(x)*100,2),
                                            "sd" = round(sd(x)*100,2),
                                            "median" = round(median(x),2),
                                            "max" = round(max(x),4),
                                            "min" = round(min(x),4),
                                           "skewness" = round(skewness(x),2),
                                           "kurtosis" = round(kurtosis(x),2)
                                            
))

tableS <- data.frame(tableS)
tableS$variable <- rownames(tableS)

fwrite(tableS, "/Users/luzhang/Desktop/summary_table_SPY.csv")
