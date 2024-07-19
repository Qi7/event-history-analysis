# we can only analyze piecewise constant functions in the R package.
# like marriage status, the time-varing covariates can be interal. And we should be cautious about reverse causality and confounding.

library(eha)

data(scania)

scand <- make.communal(scania, logrye[, 2, drop=FALSE], start=1801.75)
scand[scand$id == 1, ]

fit <- coxreg(Surv(enter, exit, event) ~ ses + sex + foodprices, data=scand)

# ties
first <- fert[fert$parity == 1, ]

## default method: efron
fit.e <- coxreg(Surv(next.ivl, event) ~ year + age , data = first, 
                method = "efron")

## Breslow 
fit.b <- coxreg(Surv(next.ivl, event) ~ year + age, data = first,
                method = "breslow")

## the hybrid mppl
fit.mp <- coxreg(Surv(next.ivl, event) ~ year + age, data = first,
                 method = "mppl", coxph = FALSE)

## true discrete
fit.ml <- coxreg(Surv(next.ivl, event) ~ year + age, data = first,
                 method = "ml", coxph = FALSE)


# risk set 
rs <- risksets(Surv(first$next.ivl, first$event))
tt <- table(rs$n.events)
tt


# the Nelson-Aalen plot
par(las = 1)
plot(rs$risktimes, cumsum(rs$n.events/rs$size), 
  type = 'S', xlab = "Duration (years)", ylab = "Cum. hazards")
abline(h = 0)

# Survival function 
par(las = 1)
sur <- exp(-cumsum(rs$n.events/rs$size))
plot(rs$risktimes, sur, type = 'S', xlab = "Duration (years)", ylab = "Surviving fraction")
abline(h = 0)