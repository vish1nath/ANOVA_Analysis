market <- read.csv("~/Documents/SN/market.csv")
attach(market)

sapply(split(Price,Team),summary)
sapply(split(Price,Team),sd)
boxplot(split(Price,Team))

sapply(split(Price,Position),summary)
sapply(split(Price,Position),sd)
boxplot(split(Price,Position))
table(Position,Team)
value=lm(Price~ Position + Team + Position*Team)
fit=aov(Price~ Position + Team + Position*Team)
library(car)
Anova(fit,type=3)


std.resb <- rstandard(fit)
hatb <- hatvalues(fit)
cooksb <- cooks.distance(fit)
plot(fitted(fit),std.resb,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(std.resb, ylab="Standardized residuals")
cbind(std.resb,hatb,cooksb)
qqPlot(fit,id.method="identify",simulate=TRUE, main="Q-Q Plot")
model.tables(fit,"mean")

absres <- abs(sresa)
anova(lm(absres ~ Position + Team + Position*Team))
log.Price <- log10(Price)
boxplot(split(log.Price,Team))
boxplot(split(log.Price,Position))

fit1=aov(Price~ Position + Team + Position*Team)
summary(lm(Price~ Position + Team + Position*Team))
Anova(fit1,type=3)
levene <- aov(abs(model.diag$std.res) ~ POS+TEAM+POS*TEAM)
std.resc <- rstandard(fit1)
hatc <- hatvalues(fit1)
cooksc <- cooks.distance(fit1)
plot(fitted(fit1),std.resc,xlab="Fitted values",ylab="Standardized residuals")
qqnorm(std.resc, ylab="Standardized residuals")
cbind(std.resc,hatc,cooksc)

levene <- aov(abs(fit1.diag$std.res) ~ Position+Team+Position*Team)
Anova(levene,type=3)

fit2= aov(log.Price ~ Position + Team)
Anova(fit2,type=3)
model.tables(fit2, "mean")
library(multcomp)
summary(glht(fit2, linfct=mcp(Position="Tukey")))
summary(glht(fit2, linfct=mcp(Team="Tukey")))


