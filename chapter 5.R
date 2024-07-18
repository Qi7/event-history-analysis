library(eha)

library(dplyr)

swe <- full_join(swedeaths, swepop, by = c('age', 'sex', 'year')) %>%
  select(year, age, sex, deaths, pop) %>%
  filter(year %in% c(2019, 2020)) %>%
  filter(age > 60 & age <=90)

swe$age <- factor(swe$age)
swe$year <- factor(swe$year)
fit <- glm(deaths ~ offset(log(pop)) + year + sex + age,
           family = poisson, data = swe)
summary(fit)

drop1(fit, test = 'Chisq')

round(summary(fit)$coefficients[c(1:3), ], 3)

round(summary(fit)$coefficients[c(19:21), ], 3)

fit1 <- glm(deaths ~ offset(log(pop)) + sex + year*age
            , family = poisson
            , data = swe)
drop1(fit1, test='Chisq')

beta <- coefficients(fit)[2:3]
alpha <- coefficients(fit)[-(2:3)]
alpha
alpha[2:length(alpha)] <- alpha[2:length(alpha)] + alpha[1]

lambda.2019 <- exp(alpha)
lambda.2020 <- exp(alpha + beta[1])

# plot hazard functions

par(las = 1)
ages <- c(61:90)
ages
plot(ages, lambda.2019, ylim = c(0, 0.15), type='S'
, xlab='age', ylab="mortality")
lines(ages, lambda.2020, type='S', lty=2)
abline(h=0)
legend("topleft", legend = c(2019, 2020), lty = 1:2)


y2019 <- swe[swe$year == 2019, ]
y2019 <- aggregate(y2019[, c('deaths', 'pop')], by = y2019['age'], FUN = sum)

y2020 <- swe[swe$year == 2020, ]
y2020 <- aggregate(y2020[, c('deaths', 'pop')], by = y2020['age'], FUN = sum)

rate2019 <- y2019$deaths / y2019$pop
rate2020 <- y2020$deaths / y2020$pop

par(las = 1)
plot(ages, rate2019, ylim = c(0, 0.20), type='S'
, xlab='age', ylab="mortality")
lines(ages, rate2020, type='S', lty=2)
abline(h=0)
