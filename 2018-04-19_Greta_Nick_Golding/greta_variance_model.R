library (greta)

x <- iris$Petal.Length
y <- iris$Sepal.Length
z <- iris$Sepal.Width

group <- as.numeric(iris$Species)

# mean model
coef <- normal(0, 10, dim = 3)
intercept <- normal(0, 10)
y_hat <- intercept + x * coef[group]

# variance model
coef_sd <- normal(0, 10)
intercept_sd <- normal(0, 10)
sd <- exp(intercept_sd + coef_sd * z)

distribution(y) <- normal(y_hat, sd)

m <- model(coef, coef_sd)

draws <- mcmc(m, n_samples = 300)




# make greta array for predictions
z_pred <- seq(min(z), max(z), length.out = 100)
sd_pred <- exp(intercept_sd + coef_sd * z_pred)

# predict given fixed values
calculate(sd_pred[1:4],
          list(intercept_sd = -1, coef_sd = 5))

# predict from the draws
pred_draws <- calculate(sd_pred, draws)

sd_pred_est <- summary(pred_draws)$statistics[, 1]
sd_pred_sd <- summary(pred_draws)$statistics[, 2]

plot(sd_pred_est ~ z_pred, type = "l")
lines(z_pred, (sd_pred_est + sd_pred_sd), lty = 2)
lines(z_pred, (sd_pred_est - sd_pred_sd), lty = 2)
