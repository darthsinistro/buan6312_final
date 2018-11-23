rm(list=ls())

library(readstata13)
fatal <- read.dta13("car_fatalities.dta")

summary(fatal)

fatal_raw <- fatal

fatal$mrsv <- fatal$allsvn/fatal$allmort

depvar <- c("mrall","mra1517","mra1820","mra2124","mraidall","mralln","mrsv")
indepvar <- c("pop","spircons","unrate","perinc","beertax","mormon","sobapt","mlda","dry","yngdrv","vmiles","gspch","jaild","comserd")

dep <- fatal[,depvar]
indep <- fatal[,indepvar]

result <- data.frame(dep=rep(NA,7),adj=rep(NA,7))

# This is just to get an idea as to how much of the variability in different dependent variables is explained by the data given to us.

for(i in 1:7){
    mo_dep <- dep[,i]
    mo_df <- cbind(mo_dep,indep)
    # browser()
    model <- step(lm(mo_dep~., data=mo_df))
    # print(colnames(dep)[i],summary(model)$adj.r.squared)
    result$dep[i] <- colnames(dep)[i]
    result$adj[i] <- summary(model)$adj.r.squared
    rm(mo_df)
}
str(model)
