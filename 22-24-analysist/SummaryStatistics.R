rm(list = setdiff(ls(),lsf.str()))
library(data.table)
library(moments)


data <- fread("/Users/luzhang/Desktop/Risks/data/returns_vix_usd_QQQ.csv")
head(data)
dim(data)

data$date<-NULL

tableS <- sapply(data, function(x) c("obs" = round(length(x),0),
                                          "mean" = round(mean(x),6),
                                            "sd" = round(sd(x),6),
                                            "median" = round(median(x),6),
                                            "max" = round(max(x),6),
                                            "min" = round(min(x),6),
                                           "skewness" = round(skewness(x),2),
                                           "kurtosis" = round(kurtosis(x),2)
                                            
))

tableS <- data.frame(tableS)
tableS$variable <- rownames(tableS)

fwrite(tableS, "/Users/luzhang/Desktop/Risks/data/Summary_Table_QQQ.csv")
