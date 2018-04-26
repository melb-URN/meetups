head(iris)

x <- as_data(iris$Petal.Length)
y <- as_data(iris$Sepal.Length)

library (greta)

coef <- normal(0, 10)
int <- normal(0, 10)
sd <- lognormal(0, 3)

mu <- abs(int +  coef * x)
distribution(y) <- normal(mu, sd)

m <- model(coef)

draws <- mcmc(m, n_samples = 100)

plot(draws)
bayesplot::mcmc_trace(draws)

plot(m)
