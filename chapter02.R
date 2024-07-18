library(eha)

head(mort)

par(mfrow = c(1,2))
with(mort, plot(Surv(enter, exit, event), fun = 'cumhaz'
                , main='Cumulative hazards function'
                , xlab = 'Duration'))
with(mort, plot(Surv(enter, exit, event),
                main = "Survival function"
                , xlab = "Duration"))

# parametric estimation
par(mfrow = c(1, 2), las = 1)
fit.w <- phreg(Surv(enter, exit, event) ~ 1, data = mort)
plot(fit.w, fn = 'cum', main='')
plot(fit.w, fn = 'sur', main = '')
f
