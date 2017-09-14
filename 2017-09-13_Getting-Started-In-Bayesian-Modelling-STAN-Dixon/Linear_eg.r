

## NB watch that 'extract' is used in tidyr and rstan


###################
##
## DATA -------------
##
###################


# ASSOCIATION OF ROAD RACING STATISTICS
# WORKING GROUP ON ROAD RECORDS
# Mens World Records (as recognized by the ARRS)
# http://www.arrs.net/WG_Rec_ENG.htm




# library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

Distance <- c(5, 8, 10, 12, 15, 16, 20, 21.1, 25, 30, 42.2, 50, 100)
Time <- c("0:12:59.5", "0:22:02.2", "0:26:43.7", "0:33:46.0", "0:41:12.5", "0:44:23.0", "0:56:01.0", "0:58:23.0", "1:11:50.0", "1:28:00.0", "2:02:56.4", "2:43:38.0", "6:16:41.0")
Name <- c("Sammy Kipketer (KEN)", "Peter Githuka (KEN)", "Leonard Patrick Komon (KEN)", "Simon Kigen (KEN)", "Leonard Patrick Komon (KEN)", "Haile Gebreselasie (ETH)", "Zerisenay Tadesse (ERI)", "Zerisenay Tadesse (ERI)", "Samuel Kiplimo Kosgei (KEN)", "Takayuki Matsumiya (JPN)", "Dennis Kipruto Kimetto (KEN)", "Thompson Magawana (RSA)", "Jean-Paul Praet (BEL)")
Date <- c("26/03/2000", "20/07/1996", "26/09/2010", "19/05/1985", "21/11/2010", "4/09/2005", "8/10/2006", "21/03/2010", "9/05/2010", "27/02/2005", "28/09/2014", "2/04/1988", "12/09/1992")
Race <- c("Carlsbad CA USA", "Kingsport TN USA", "Utrecht NED", "Portland OR USA", "Nijmegen NED", "Tilburg NED", "Debrecen HUN", "Lisbon POR", "Berlin GER", "Kumamoto JPN", "Berlin GER", "Cape Town RSA", "Winschoten NED")

run_df <- data_frame("Distance"=Distance,"Time"=Time,"Name"=Name,"Date"=Date,"Race"=Race)



# clean data and add feature
run <- run_df %>% mutate(
     Minutes=as.numeric(substr(Time, 1, 1))*60 + 
              as.numeric(substr(Time, 3, 4)) + 
               as.numeric(substr(Time, 6, 9))/60, Hours=Minutes/60,
     Country=gsub("^.+\\(|\\)$", "", Name))
     
     

     
     
################
## RAW PLOTS -------------------------------
################

# raw scale

data_plot <- run %>% ggplot(aes(Distance, Minutes)) + geom_point(size=3.5) +
                    labs(title="Running records") + 
                    scale_x_continuous(breaks=c(5,10,15,20,25,30,42.2,50,100)) +
                    theme(text=element_text(size=14))   # + stat_smooth(method="lm")

data_plot
                    
    # log(y)
    run %>% ggplot(aes(Distance, log(Minutes))) + geom_point(size=2.5)

    # log(x)
    run %>% ggplot(aes(log(Distance), Minutes)) + geom_point(size=2.5)

    # log(x, y)
    run %>% ggplot(aes(log(Distance), log(Minutes), colour=Country)) + geom_point(size=3.5) + # + stat_smooth(method="lm") +
        labs(title="Running records")






############
## lm        ----------------------------------------------------
############


  library(sweep) # used for converting forecast to ggplot-able 

  # subset data up to 30km
  runf <- run[1:10,] %>% select(Distance, Minutes)

  lmfit <- lm(Minutes~Distance, data=runf)

  lmfit

  # (Intercept)     Distance  
       # -2.488        2.963  
       
  # create forecast using forecast pckg

  fcast <- forecast::forecast(lmfit, newdata=data.frame(Distance=c(42.2, 50, 100)))


    # plot(Minutes~Distance, data=run, pch=19)
    # abline(lmfit)

  # plot forecast vs data
  plot(fcast, xlim=c(0, 101), ylim=c(0, 400), cex=2, pch=19)
  points(Minutes[11:13]~Distance[11:13], data=run, pch=1, cex=2.5, lwd=2, col="red")

  lm_fcast <- sw_sweep(fcast) %>% bind_cols(select(run, Distance)) %>% rename(Minutes=data) %>% filter(key=="forecast")

  lm_plot <-  run %>% ggplot(aes(Distance, Minutes)) + 
      geom_linerange(data=lm_fcast, aes(x=Distance, ymin=lo.95, ymax=hi.95), stat="identity", colour="blue", size=10, alpha=.4) +
      geom_linerange(data=lm_fcast, aes(x=Distance, ymin=lo.80, ymax=hi.80), stat="identity", colour="grey", size=10) +
      geom_point(data=lm_fcast, colour="blue", size=4) +
      geom_abline(slope= coefficients(lmfit)[2][[1]], intercept= coefficients(lmfit)[1][[1]]) +
      geom_point(size=4.5, shape=17) + 
      theme(text=element_text(size=16)) +
      labs(title="Linear model of running records") + scale_x_continuous(breaks=c(5,10,15,20,25,30,42.2,50,100)) 

  lm_plot






######################
##
## STAN ------------------------------------------
##
######################


library(rstan)


parallel::detectCores()

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


run


# format data for stan
rmoddata <- list(N=10, 
                  X=run$Distance[1:10],  # Distance
                  Y=run$Minutes[1:10]    # Time (Y)
                  ) 

                  
rmodel <- "
data {
  int<lower=0> N;       // Num obs
  vector[N]    Y;       // Obs times (Minutes)
  vector[N]    X;       // Obs Distances 
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  // priors -------
  alpha ~ normal(0,10);    
  beta ~ normal(0,10);
  sigma ~ cauchy(0,5);
  // model --------
  Y ~ normal(alpha + beta * X, sigma);
}
"

# compile model
rcompiled_model <- stan_model(model_code=rmodel, verbose=FALSE) 


# run samples

fitr <- sampling(rcompiled_model, data=rmoddata, iter=1000, chains=3, seed=1234) 

fitr

       # mean se_mean   sd   2.5%   25%   50%   75% 97.5% n_eff Rhat
# alpha -2.47    0.04 1.00  -4.49 -3.04 -2.46 -1.85 -0.56   514 1.01
# beta   2.96    0.00 0.06   2.86  2.93  2.96  2.99  3.07   534 1.01
# sigma  1.21    0.02 0.39   0.74  0.96  1.14  1.35  2.25   494 1.01
# lp__  -6.16    0.08 1.53 -10.18 -6.78 -5.77 -5.07 -4.51   386 1.01


print(fitr, c("alpha"), probs=c(0.01, 0.5, 0.99)) # GIVE DIFFERENT SUMMARY --------------------

       # mean se_mean sd    1%   50%   99% n_eff Rhat
# alpha -2.47    0.04  1 -4.98 -2.46 -0.01   514 1.01



# traceplot
plot(fitr, plotfun = "trace", pars = c("alpha", "beta", "sigma"), inc_warmup = TRUE) + geom_line(size=2) + theme(strip.text =element_text(size=14))

# density
plot(fitr, plotfun = "hist", pars = c("alpha", "beta", "sigma"), inc_warmup = FALSE) 
stan_dens(fitr, pars = c("alpha", "beta", "sigma")) + facet_wrap(~parameter, scales="free") 


###############
## loo / WAIC -------------
###############


                  
rmodel_1 <- "
data {
  int<lower=0> N;         // Num obs
  vector[N]    Y;         // Obs times
  vector[N]    X;         // Obs Distances 
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
    // priors -------
  alpha ~ normal(0,10);    
  beta ~ normal(0,10);
  sigma ~ cauchy(0,5);
    // model --------
  Y ~ normal(alpha + beta * X, sigma);
}
generated quantities{
vector[N] log_lik;    // for LOO and WAIC
  for(i in 1:N){
   log_lik[i] = normal_lpdf(Y[i] | alpha + beta * X[i], sigma);  // pointwise log-likelihood
  }
}
"

# compile model
rcompiled_model_1 <- stan_model(model_code=rmodel_1, verbose=FALSE) 


# run samples

fitr_1 <- sampling(rcompiled_model_1, data=rmoddata, iter=1500, chains=3, seed=1234) 

fitr_1

       # mean se_mean   sd   2.5%   25%   50%   75% 97.5% n_eff Rhat
# alpha -2.47    0.04 1.00  -4.49 -3.04 -2.46 -1.85 -0.56   514 1.01
# beta   2.96    0.00 0.06   2.86  2.93  2.96  2.99  3.07   534 1.01
# sigma  1.21    0.02 0.39   0.74  0.96  1.14  1.35  2.25   494 1.01
# lp__  -6.16    0.08 1.53 -10.18 -6.78 -5.77 -5.07 -4.51   386 1.01


print(fitr_1, c("alpha"), probs=c(0.01, 0.5, 0.99)) # GIVE DIFFERENT SUMMARY --------------------

       # mean se_mean sd    1%   50%   99% n_eff Rhat
# alpha -2.47    0.04  1 -4.98 -2.46 -0.01   514 1.01




library(loo)
log_lik_1 <- extract_log_lik(fitr_1)
loo(log_lik_1) 
waic(log_lik_1)





##############################
##
## PREDICTION -----------
##
##############################



rmoddata2 <- list(N=10, 
                  X=run$Distance[1:10], 
                  Y=run$Minutes[1:10], 
                  N_nu=3,                      # number of new data points to predict for
                  X_nu=run$Distance[11:13]     # new X values for prediction
                  ) 

rmodel2 <- "
data {
  int<lower=0> N;          // Num obs
  vector[N]    Y;          // Obs times
  vector[N]    X;          // Obs Distances 
  int<lower=0> N_nu;       // Num new obs 
  vector[N_nu] X_nu;       // New X to predict on
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
   // priors -------
  alpha ~ normal(0,10);    
  beta ~ normal(0,10);
  sigma ~ cauchy(0,5);
   // model --------
  Y ~ normal(alpha + beta * X, sigma);
}
generated quantities {
vector[N_nu]  Y_nu;                       // predicted new times
  for(i in 1:N_nu){
    Y_nu[i] = normal_rng(alpha + X_nu[i]*beta, sigma);
  }
}
"


rcompiled_model2 <- stan_model(model_code=rmodel2, verbose=FALSE) 


# run samples

fitr2 <- sampling(rcompiled_model2, data=rmoddata2, iter=1000, chains=3, seed=1234) 

fitr2

            # mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
# alpha      -2.42    0.05 0.98  -4.27  -3.04  -2.47  -1.79  -0.50   393 1.00
# beta        2.96    0.00 0.06   2.85   2.92   2.96   2.99   3.06   411 1.00
# sigma       1.24    0.02 0.38   0.73   0.97   1.16   1.42   2.19   520 1.00
# Y_nu[1] 122.36    0.07 2.01 118.12 121.20 122.43 123.57 126.33   737 1.00
# Y_nu[2] 145.50    0.09 2.28 141.01 144.10 145.49 146.94 150.04   629 1.00
# Y_nu[3] 293.38    0.23 4.81 283.73 290.34 293.42 296.41 302.84   453 1.00



# traceplot
plot(fitr2, plotfun = "trace", pars = c("alpha", "beta", "sigma", "Y_nu"), inc_warmup = TRUE) + geom_line(size=2) + theme(strip.text =element_text(size=14))

# density
plot(fitr2, plotfun = "hist", pars = c("alpha", "beta", "sigma"), inc_warmup = FALSE) 
stan_dens(fitr2, pars = c("alpha", "beta", "sigma", "Y_nu")) + facet_wrap(~parameter, scales="free") 



## PLOT PREDICTIONS ------------

# extract samples

rstan::extract(fitr2, pars = c("Y_nu"), inc_warmup = FALSE) %>% data.frame() %>% head()

f2_df <- rstan::extract(fitr2, pars = c("Y_nu"), inc_warmup = FALSE) %>% data.frame() %>% gather(params, value)

f2_df %>% head

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
} 


f2_pred <- f2_df %>%  group_by(params) %>% 
                            summarise(Mode=getmode(value), 
                                      Mean = mean(value), 
                                      SD = sd(value), 
                                      Med=median(value), 
                                      Q2.5 = quantile(value, 0.025), 
                                      Q10 = quantile(value, 0.01), 
                                      Q25 = quantile(value, 0.25), 
                                      Q50 = quantile(value, 0.5), 
                                      Q75 = quantile(value, 0.75), 
                                      Q90 = quantile(value, 0.9), 
                                      Q97.5 = quantile(value, 0.975) 
                                      ) %>% 
                              mutate(Distance=c(42.2, 50.0, 100.0))


# plot  predictions 

data_plot + geom_errorbar(data=f2_pred, aes(ymin=Q2.5, ymax=Q97.5, y=NULL), size=2, colour="red", width=2) +
             geom_point(data=f2_pred, aes(y=Q50), size=3, colour="red")






#########################################
##
## EVALUATING PREDICTIONS WITH LPPD ----------
##
########################################


rmoddata3 <- list(N=10, 
                  X=run$Distance[1:10], 
                  Y=run$Minutes[1:10], 
                  N_nu=3,  
                  X_nu=run$Distance[11:13],
                  Y_nu=run$Minutes[11:13]    # hold out times
                  ) 



rmodel3 <- "
data {
  int<lower=0> N;          // Num obs
  vector[N]    Y;          // Obs times
  vector[N]    X;          // Obs Xances 
  int<lower=0> N_nu;       // Num new obs 
  vector[N_nu] X_nu;       // New X to predict on
  vector[N_nu] Y_nu;       // New observed time to evaluate on
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  // priors -------
  alpha ~ normal(0,10);    
  beta ~ normal(0,10);
  sigma ~ cauchy(0,5);
  // model --------
  Y ~ normal(alpha + beta * X, sigma);
}
generated quantities {
 vector[N_nu]  Y_pred;      // predicted new times
 real          log_p_new;   // posterior predictive log density test data 
log_p_new = 0;
  for(i in 1:N_nu){
    Y_pred[i] = normal_rng(alpha + X_nu[i]*beta, sigma);                        // predictions
    log_p_new = log_p_new + normal_lpdf(Y_nu[i] | alpha + X_nu[i]*beta, sigma); // likelihood for out of sample predictions
  }
}
"


rcompiled_model3 <- stan_model(model_code=rmodel3, verbose=FALSE) 


# run samples

fitr3 <- sampling(rcompiled_model3, data=rmoddata3, iter=1000, chains=3, seed=1234) 

fitr3


# need to correct with log_sum_exp - E(log(y_nu|y)) vs log(E(y_nu|y))


fitr3_samples <- rstan::extract(fitr3);

log_sum_exp <- function(u) {
  max_u <- max(u);
  a <- 0;
  for (n in 1:length(u)) {
    a <- a + exp(u[n] - max_u);
  }
  return(max_u + log(a));
}

# log posterior predictive density (log(E(y_nu|y)))

-log(length(fitr3_samples$log_p_new)) + log_sum_exp(fitr3_samples$log_p_new) 

# > [1] -382.7557



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##














