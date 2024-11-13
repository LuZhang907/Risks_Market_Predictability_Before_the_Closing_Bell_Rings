rm(list = setdiff(ls(),lsf.str()))

fit_full <- readRDS("/Users/luzhang/Desktop/Risks/data/fit_b_non-scaled_roll3_2024_SPY.rds")
# for one specific model
md_name <- "fit_full"   # <------- change model name each time
md <- get(md_name)
#md_file <-  paste0("./Data/", md_name, ".rds")
#if(!file.exists(md_file)) saveRDS(md, file = md_file)
print(md$fit, digits=4)
#plot(md)

post_data <- as.data.frame(md$fit)
col_post <- names(post_data)
rst <- cbind(apply(post_data, 2, quantile, 0.025), apply(post_data, 2, quantile, 0.5), apply(post_data, 2, quantile, 0.975))
rst <- data.frame(rst)
names(rst) <- c("2.5%", "50%", "97.5%")
rst$sig <- "No"
rst$sig[sign(rst$`2.5%`) == sign(rst$`97.5%`)] <- "Yes"
rownames(rst) <- stringr::str_replace(rownames(rst), "b_", "")
rst <- rst[!(rownames(rst) %in% c("lprior", "lp__")),]
xtable::xtable(rst, digits = -2, caption = "Bayesian linear regression model with Student-t error terms.")
