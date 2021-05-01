# Loading function dependencies
source("HM804_GP.R")
source("noise_input_estimation.R")
source("optimization_gp.R")
source("simulation_functions.R")

# Having the true values of y and x
N <- n <- 60
n_test <- 1000
true_sigma_y <- 0.05
true_sigma_x <- 0.3


# Simulation scenarios
simulated_data <- sim_square_wave(n = n,
                                  sigma_y = true_sigma_y,
                                  sigma_x = true_sigma_x,
                                  n_test = n_test,
                                  seed = 24)
# 
# simulated_data <- sim_sin(n = n,
#                                   sigma_y = true_sigma_y,
#                                   sigma_x = true_sigma_x,n_test = n_test,
#                                   seed =43)
# 
# simulated_data <- sim_exp_sin(n = n,
#                           sigma_y = true_sigma_y,
#                           sigma_x = true_sigma_x,n_test = n_test,
#                           seed =43)
# 
# simulated_data <- sim_tan_sin(n = n,
#                               sigma_y = true_sigma_y,
#                               sigma_x = true_sigma_x,n_test = n_test,
#                               seed =43)
# 
# simulated_data <- sim_tanh_cos(n = n,
#                               sigma_y = true_sigma_y,
#                               sigma_x = true_sigma_x,n_test = n_test,
#                               seed =3)

# 
# 
# simulated_data <- sim_log_sin(n = n,
#                                sigma_y = true_sigma_y,
#                                sigma_x = true_sigma_x,n_test = n_test,
#                                seed =48)





x <- simulated_data$x
y <- matrix(simulated_data$y, ncol = 1)


x_star <- simulated_data$x_test
# x_star <- matrix(seq(min(x),max(x),length.out = 1000),ncol=1)


optim_param <- optim(
  par = runif(3), fn = loglike_gp, method = "L-BFGS-B", lower = c(0.001, 0.001, 0.001),
  x = x, y = y
)


# Using the estimated parameters
est_sigma_f <- optim_param$par[1]
est_length_parameter <- optim_param$par[2]
est_sigma_y <- optim_param$par[3]


# Mean function
f_mean_estimated <- mean_f(
  x = x,
  y = y,
  x_star = x_star,
  sigma_y = est_sigma_y,
  sigma_f = est_sigma_f,
  length_parameter = est_length_parameter
)


# Standard deviation function
var_estimated_matrix <- variance_f(
  x = x,
  y = y,
  x_star = x_star,
  sigma_y = est_sigma_y,
  sigma_f = est_sigma_f,
  length_parameter = est_length_parameter
)

sd_var_estimated <- sqrt(diag(var_estimated_matrix))

# Derivative from f train

# partial_f_derivative_train_prior <- analytic_derivative(
#   x = x, y = y, x_star = x,
#   sigma_y = est_sigma_y, sigma_f = est_sigma_f, length_parameter = est_length_parameter+est_sigma_x_noise
# ) %>% matrix(ncol=1)

partial_f_derivative_train <- numerical_derivative(
  x = x, y = y,  f_mean = ,eps = 1e-12,
  sigma_y = est_sigma_y, sigma_f = est_sigma_f, length_parameter = est_length_parameter
) %>% matrix(ncol=1)

# partial_f_derivative_train


# Derivative from f_star
partial_f_derivative <- analytic_derivative(
  x = x, y = y, x_star = x_star,
  sigma_y = est_sigma_y, sigma_f = est_sigma_f, length_parameter = est_length_parameter
)


# Calculate the optim parameters with the noise
optim_param_noise <- optim(
  par = c(0.1, 0.1, 0.1, 0.1),
  fn = loglike_gp_noise_input,
  method = "L-BFGS-B",
  lower = c(0.0001, 0.0001, 0.0001, 0.0001),
  x = x, y = y, partial_f = partial_f_derivative_train
)



# Using the estimated parameters
est_sigma_f_noise <- optim_param_noise$par[1]
est_length_parameter_noise <- optim_param_noise$par[2]
est_sigma_y_noise <- optim_param_noise$par[3]
est_sigma_x_noise <- optim_param_noise$par[4]



pred_f_noise_x <- mean_f_noise_x(
  x = x,
  x_star = x_star,
  y = y,
  sigma_x = est_sigma_x_noise,
  partial_f = partial_f_derivative_train,
  sigma_y = est_sigma_y_noise,
  sigma_f = est_sigma_f_noise,
  length_parameter = est_length_parameter_noise
)

var_f_noise_x_matrix <- var_f_noise_x(
  x = x,
  x_star = x_star,
  y = y,
  sigma_x = est_sigma_x_noise,
  partial_f = partial_f_derivative,
  sigma_y = est_sigma_y_noise,
  sigma_f = est_sigma_f_noise,
  length_parameter = est_length_parameter_noise
)


partial_f_derivative_train_prior <- numerical_derivative(
  x = x, y = y,  f_mean = pred_f,eps = 1e-8,
  sigma_y = est_sigma_y, sigma_f = est_sigma_f, length_parameter = est_length_parameter
) %>% matrix(ncol=1)


optim_param_noise_prior <- optim(
  par = c(0.1, 0.1, 0.1, 0.1),
  fn = loglike_gp_noise_input,
  method = "L-BFGS-B",
  lower = c(0.0001, 0.0001, 0.0001, 0.0001),
  x = x, y = y, partial_f = partial_f_derivative_train_prior
)

est_sigma_f_noise_prior <- optim_param_noise_prior$par[1]
est_length_parameter_noise_prior <- optim_param_noise_prior$par[2]
est_sigma_y_noise_prior <- optim_param_noise_prior$par[3]
est_sigma_x_noise_prior <- optim_param_noise_prior$par[4]


mean_f_noise_x_prior <- pred_f_noise_x_prior(x = x,
                                             y = y,
                                             x_star = x_star,
                                             partial_f = partial_f_derivative_train_prior,
                                             sigma_f = est_sigma_f_noise_prior,
                                             length_parameter = est_length_parameter_noise_prior,
                                             sigma_y = est_sigma_y_noise_prior,
                                             sigma_x = est_sigma_x_noise_prior)


variance_f_noise_prior_value <-var_f_noise_x_prior(x = x,
                                                   y = y,
                                                   x_star = x_star,
                                                   partial_f = partial_f_derivative_train_prior,
                                                   sigma_y = est_sigma_y_noise_prior,
                                                   sigma_f = est_sigma_f_noise_prior,
                                                   length_parameter = est_length_parameter_noise_prior,
                                                   sigma_x = est_sigma_x_noise_prior,
                                                   mean_f_noise_x_prior = mean_f_noise_x_prior)


print(optim_param_noise$par)
print(optim_param$par)
print(optim_param_noise_prior$par)

