library (greta)

x <- iris$Petal.Length
y <- iris$Sepal.Length

group <- as.numeric(iris$Species)

coef_mean <- normal(0, 10)
coef_sd <- cauchy(0, 1, truncation = c(0, Inf))
coef <- normal(coef_mean, coef_sd, dim = 3)

intercept <- normal(0, 10)
sd <- cauchy(0, 1, truncation = c(0, Inf))

y_hat <- intercept + x * coef[group]

distribution(y) <- normal(y_hat, sd)

m <- model(coef)

draws <- mcmc(m, n_samples = 500)

bayesplot::mcmc_trace(draws)
summary(draws)
