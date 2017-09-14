
#################################################################
##
## Fit normal distribution (parameter recovery) -------------------------------------
##
#################################################################



## NORMAL distribution --------------------

set.seed(1234)

N <- 1000
N_nu <- 20
Mu <- 0.5             # mean
sd <- 0.1             # standard deviation
x <- rnorm(N, Mu, sd) # data
  y <- sample(x, N_nu)  # sub sample of data for prediction

plot(density(x))
  plot(density(y))


library(ggplot2)
library(dplyr)
library(rstan)

parallel::detectCores()

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



# format data
moddata <- list(N=N, x=x) 

# specify model (idealy as .stan file)

model1 <- "
data{
  int<lower=0>        N;  // No. obs
  vector<lower=0>[N]  x;  // data
}
parameters{
 real mu;             // mean
 real<lower=0> sigma; // standard deviation
}
model{
 x ~ normal(mu, sigma);
}
"

# complie

compiled_model <- stan_model(model_code=model1, verbose=FALSE) 

?stan_model

# run samples

fit <- sampling(compiled_model, data=moddata, iter=100, chains=3, seed=1234) # cores=

fit

        mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
mu      0.49    0.00 0.01   0.47   0.48   0.49   0.50   0.51  2738    1
sigma   0.10    0.00 0.01   0.09   0.10   0.10   0.11   0.12  2793    1
lp__  178.35    0.02 1.02 175.64 177.94 178.67 179.09 179.36  1756    1


# traceplot
plot(fit, plotfun = "trace", pars = c("mu", "sigma", "lp__"), inc_warmup = TRUE) + geom_line(size=2) + theme(strip.text =element_text(size=14))

# density
plot(fit, plotfun = "hist", pars = c("mu", "sigma"), inc_warmup = FALSE, ci_level = 0.95) 
stan_dens(fit, pars = c("mu", "sigma")) + facet_wrap(~parameter, scales="free_x") + geom_density(aes(xmin=0))


y2 <- rstan::extract(fit, "mu")[[1]]
plot(density(y2))


# rstan::extract
rstan::extract(fit, pars = c("mu", "sigma"), inc_warmup = FALSE) %>% tbl_df

    # permuted: A logical scalar indicating whether the draws after the warmup period in each
      # chain should be permuted and merged. If FALSE, the original order is kept. For
      # each stanfit object, the permutation is fixed (i.e., extracting samples a second
      # time will give the same sequence of iterations).
    # inc_warmup: A logical scalar indicating whether to include the warmup draws. This argument
      # is only relevant if permuted is FALSE.


      
      
## MORE SAMPLES -----------

fit <- sampling(compiled_model, data=moddata, iter=1500, chains=3,  seed=1984) 



fit

# traceplot
plot(fit, plotfun = "trace", pars = c("mu", "sigma"), inc_warmup = TRUE) + geom_line(size=2) + theme(strip.text =element_text(size=14))

# density

plot(fit, plotfun = "hist", pars = c("mu", "sigma"), inc_warmup = FALSE, ci_level = 0.95) 
stan_dens(fit, pars = c("mu", "sigma")) + facet_wrap(~parameter, scales="free_x") + geom_density(aes(xmin=0))





#########################################
## Compare model notations
#########################################


model2 <- "
data{
  int<lower=0>        N;
  vector<lower=0>[N]  x;
}
parameters{
   real            mu;    // mean vect notation 
   real<lower=0>   sigma; // standard deviation std vector     
   real            mu2;    // mean vect increment notation
   real<lower=0>   sigma2; // standard deviation 
   real            mu3;    // mean loop notation
   real<lower=0>   sigma3; // standard deviation loop notation
}
model{
   // vector notation
     x ~ normal(mu, sigma);
   // increment vector
     target+= normal_lpdf(x | mu2, sigma2);
   // loop style  
     for(i in 1:N){
      x[i] ~ normal(mu3, sigma3);
      }
  }
"

compiled_model2 <- stan_model(model_code=model2, verbose=FALSE) 

fit2 <- sampling(compiled_model2, data=moddata, iter=1000, chains=3, seed=1234) 

fit2


# vector vs increment:
# In both cases, the effect is to add terms to the target log density. The only difference is
# that the example with the sampling (~) notation drops all additive constants in the log
# density; the constants are not necessary for any of Stan’s sampling, approximation,
# or optimization algorithms.



