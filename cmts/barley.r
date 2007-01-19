data(US.pop, package="car")
data(Florida, package="car")
showData(Florida, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
data(barley, package="lattice")
showData(barley, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
showData(US.pop, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
showData(Xm12.5, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
showData(US.pop, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
data(Prestige, package="car")
showData(Prestige, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
Subsets.1 <- regsubsets(prestige~census+education+income, data=Prestige, nbest=2)
summary.HH(Subsets.1, statistic='adjr2')
subsets.HH(Subsets.1, statistic='adjr2', legend=FALSE)
RegModel.1.5 <- lm.regsubsets(Subsets.1, 5)  ## subset 5 has largest adjr2
summary(RegModel.1.5)
data()
Xr13.119 <- sqlQuery(channel = 3, select * from [Sheet1$])
names(Xr13.119) <- make.names(names(Xr13.119))
showData(Xr13.119, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
Xr13.119$X.100.Limit <- factor(Xr13.119$X.100.Limit, labels=c('dealer-won','dealer-lost'))
Xr13.119$X.3000.Limit <- factor(Xr13.119$X.3000.Limit, labels=c('dealer-won','dealer-lost'))
showData(Xr13.119, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)

# Excel
channel <- odbcConnect("bdr.xls") # if this was set up as a DSN
channel2 <-
 odbcDriverConnect("DRIVER=Microsoft Excel Driver (*.xls);DBQ=C:\bdr\hills.xls")
channel3 <- odbcConnectExcel("hills.xls")

channel <- odbcConnect("C:/HOME/stat/Data/Keller6Abrev/Manual~1/Keller6/Keller~1/Excel/CH12/Xr13-119.xls")
sqlSave(channel, USArrests, rownames = "State", verbose = TRUE)
# options(dec=".") # optional, if DBMS is not locale-aware
## note case of State, Murder, rape are DBMS-dependent.
sqlQuery(channel, paste("select State, Murder from USArrests",
                        "where Rape > 30 order by Murder"))
close(channel)
X13.119 <- sqlQuery(channel = 7, select * from [Sheet1$])
names(X13.119) <- make.names(names(X13.119))
StackedData <- stack(X13.119[, c("X.100.Limit","X.3000.Limit")])
names(StackedData) <- c("dealer", "limit")
StackedData$dealer <- factor(StackedData$dealer, labels=c('lost','won'))
showData(StackedData, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
.Table <- table(StackedData$dealer)
.Table  # counts for dealer
100*.Table/sum(.Table)  # percentages for dealer
.Table <- table(StackedData$limit)
.Table  # counts for limit
100*.Table/sum(.Table)  # percentages for limit
remove(.Table)
.Table <- xtabs(~limit+dealer, data=StackedData)
rowPercents(.Table)
prop.test(.Table, alternative='two.sided', conf.level=.95, correct=FALSE)
remove(.Table)
.Anova <- lm(yield ~ site, data=barley)
anova(.Anova)
tapply(barley$yield, barley$site, mean, na.rm=TRUE) # means
tapply(barley$yield, barley$site, sd, na.rm=TRUE) # std. deviations
tapply(barley$yield, barley$site, function(x) sum(!is.na(x))) # counts
remove(.Anova)
.Anova <- lm(yield ~ variety, data=barley)
anova(.Anova)
tapply(barley$yield, barley$variety, mean, na.rm=TRUE) # means
tapply(barley$yield, barley$variety, sd, na.rm=TRUE) # std. deviations
tapply(barley$yield, barley$variety, function(x) sum(!is.na(x))) # counts
.Pairs <- glht(.Anova, linfct = mcp(variety = "Tukey"))
confint(.Pairs)
plot(confint(.Pairs))
remove(.Pairs)
remove(.Anova)
par(oma=c(0,5,0,0))
.Anova <- lm(yield ~ variety, data=barley)
anova(.Anova)
tapply(barley$yield, barley$variety, mean, na.rm=TRUE) # means
tapply(barley$yield, barley$variety, sd, na.rm=TRUE) # std. deviations
tapply(barley$yield, barley$variety, function(x) sum(!is.na(x))) # counts
.Pairs <- glht(.Anova, linfct = mcp(variety = "Tukey"))
confint(.Pairs)
plot(confint(.Pairs))
remove(.Pairs)
remove(.Anova)
par(oma=c(0,8,0,0))
.Anova <- lm(yield ~ variety, data=barley)
anova(.Anova)
tapply(barley$yield, barley$variety, mean, na.rm=TRUE) # means
tapply(barley$yield, barley$variety, sd, na.rm=TRUE) # std. deviations
tapply(barley$yield, barley$variety, function(x) sum(!is.na(x))) # counts
.Pairs <- glht(.Anova, linfct = mcp(variety = "Tukey"))
confint(.Pairs)
plot(confint(.Pairs))
remove(.Pairs)
remove(.Anova)
par(oma=c(0,9,0,0))
.Anova <- lm(yield ~ variety, data=barley)
anova(.Anova)
tapply(barley$yield, barley$variety, mean, na.rm=TRUE) # means
tapply(barley$yield, barley$variety, sd, na.rm=TRUE) # std. deviations
tapply(barley$yield, barley$variety, function(x) sum(!is.na(x))) # counts
.Pairs <- glht(.Anova, linfct = mcp(variety = "Tukey"))
confint(.Pairs)
plot(confint(.Pairs))
remove(.Pairs)
remove(.Anova)
anova(lm(yield ~ variety, data=barley))
tapply(barley$yield, barley$variety, mean, na.rm=TRUE) # means
tapply(barley$yield, barley$variety, sd, na.rm=TRUE) # std. deviations
tapply(barley$yield, barley$variety, function(x) sum(!is.na(x))) # counts
.Pairs <- simint(yield ~ variety, type="Tukey", data=barley)
summary(.Pairs)
plot(.Pairs)
remove(.Pairs)

library(HH)
.Anova <- lm(yield ~ variety, data=barley)
anova(.Anova)
.Pairs <- glht.mmc(.Anova, linfct = mcp(variety = "Tukey"))
plot(.Pairs)
par(oma=c(0,0,0,9))
plot(.Pairs, ry=c(20,45), 
library(HH)
.Anova <- lm(yield ~ variety, data=barley)
anova(.Anova)
.Pairs <- glht.mmc(.Anova, linfct = mcp(variety = "Tukey"))
plot(.Pairs)
par(oma=c(0,0,0,9))
plot(.Pairs, ry=c(20,45), cex=.7, iso.name=FALSE, x.offset=-2)
par(oma=c(0,9,0,0))
plot(.Pairs$mca, xaxs="i", xlim=par()$usr[1:2], cex.axis=.7)

