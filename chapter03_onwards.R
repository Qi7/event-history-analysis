library(eha)
fit <- logrank(Surv(enter, exit, event)
               , group = sex
               , data = oldmort)
fit

# more than 2 groups
fit <- logrank(Surv(enter, exit, event)
               , group = civ
               , data = oldmort[oldmort$sex == 'male', ])
fit

ch <- child[, c('birthdate', 'sex', 'socBranch', 'enter', 'exit', 'event')]

head(ch)

str(ch)

res <- coxreg(Surv(exit, event) ~ sex + socBranch + birthdate
              , data = ch)
print(summary(res), digits = 4)

ch$cohort <- floor(toTime(ch$birthdate))
fit <- coxreg(Surv(exit, event) ~ sex + socBranch + cohort
              , data = ch)
print(summary(fit), digits = 4, short=TRUE)

plot(fit)

# chapter 4 explanatory variables and regression
age.group <- cut(child$m.age, c(15, 25, 35, 51))
table(age.group, useNA = "ifany")

# coxregression
ch <- child
ch$age_group <- age.group
ch <- age.window(ch, c(0, 1)) # right censor at age one
fit <- coxreg(Surv(enter, exit, event) ~ age_group, data = ch)
print(summary(fit), short = TRUE)

# interaction
om <- oldmort
om$farmer <- ifelse(oldmort$ses.50 == 'farmer', 'yes', 'no')
om$farmer <- factor(om$farmer)
head(om)

summary(om)

fit <- coxreg(Surv(enter, exit, event) ~ sex+farmer, data=om)
print(summary(fit), short = TRUE)

# stratified analysis
par(las=1)
fit2 <- coxreg(Surv(enter, exit, event) ~ strata(sex) + farmer,
               data = om)
plot(fit2, fun = 'cumhaz', xlim = c(60, 85),
     lty = 1:2, xlab = "Age")
abline(h=0)

# change to *
fit4 <- coxreg(Surv(enter, exit, event) ~ sex*farmer, data = om)
print(x <- summary(fit4), short = TRUE)

# one factor and one continuous covariate
fit5 <- coxreg(Surv(enter, exit, event) ~ sex + birthdate, data=om)
print(summary(fit5), short = TRUE)

fit6 <- coxreg(Surv(enter, exit, event) ~ sex*birthdate, data=om)
round(summary(fit6)$coefficients[, 1:2], 5)

# two continuous covariates
om$birthdate <- om$birthdate - 1810
fit8 <- coxreg(Surv(enter, exit, event) ~ birthdate * imr.birth, data=om)
res <- round(summary(fit8)$coefficients[, 1:2], 5)
res
