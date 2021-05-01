# Getting the optim parameter for sigma_f via likelihood
loglike_gp <- function(par, x, y) {
  
  # sigma_y <- par[1]
  sigma_f <- par[1]
  length_parameter <- par[2]
  sigma_y <- par[3]
 
  
  N <- nrow(x)
  
  K_y <- K(
    x = x,
    length_parameter = length_parameter,
    sigma_f = sigma_f,
    sigma_y = sigma_y
  )
  
  log_like <- c(-0.5 * crossprod(y, solve(K_y, y)) -
                  0.5 * determinant(K_y, logarithm = TRUE)$modulus - 0.5 * N * log(2 * pi))
  
  return(-log_like)
}

# Getting the optim parameter for sigma_f via likelihood
loglike_gp_noise_input <- function(par, x, y, partial_f) {
  
  sigma_f <- par[1]
  length_parameter <- par[2]
  sigma_y <- par[3]
  sigma_x <- par[4]

  N <- nrow(x)
  
  # Sigma_x <- diag(sigma_x^2, nrow = ncol(x))
  
  input_noise_correction <- diag(c(sigma_x^2*partial_f^2), nrow = N)
  # input_noise_correction <- diag(diag(partial_f %*% tcrossprod(Sigma_x, partial_f)), nrow = N)
  

  
  K_y <- K(
    x = x,
    length_parameter = length_parameter,
    sigma_f = sigma_f,
    sigma_y = sigma_y
  ) + input_noise_correction
  
  log_like <- c(-0.5 * crossprod(y, solve(K_y, y)) -
                  0.5 * determinant(K_y, logarithm = TRUE)$modulus - 0.5 * N * log(2 * pi))
  
  return(-log_like)
}


# Function to use the data as input and return the optim-hyperparameters

mle_std_gp <- function(x,y,seed = NULL){
  
  # Setting a seed
  set.seed(seed)
  
  optim_param <- optim(
  par = runif(n = 3,min = 0.001,max = 2), fn = loglike_gp, method = "L-BFGS-B", lower = c(0.001, 0.001, 0.001),
  x = x, y = y)
  
  return(list( est_sigma_f = optim_param$par[1],
               est_length_parameter = optim_param$par[2],
               est_sigma_y = optim_param$par[3]) )
}

# Log-like for the input noise
# This mean estimation will consider a test point with distribution.
# var_gp_std <-variance_f(
#   x = x,
#   y = y,
#   x_star = x_star,
#   sigma_y = est_sigma_y,
#   sigma_f = est_sigma_f,
#   length_parameter = est_length_parameter
# )

mle_NIGP <- function(x,y,partial_f_train){
  
  optim_param_noise <- optim(
    par = runif(n = 4,min = 0.0001,max = 3),
    fn = loglike_gp_noise_input,
    method = "L-BFGS-B",
    lower = c(0.0001, 0.0001, 0.0001, 0.0001),
    x = x, y = y, partial_f = partial_f_train
  )
  
  return(list( est_sigma_f = optim_param_noise$par[1],
               est_length_parameter = optim_param_noise$par[2],
               est_sigma_y = optim_param_noise$par[3],
               est_sigma_x = optim_param_noise$par[4]))

}

# TRUE_Y <- sapply(x_star,true_y_f)





# Calculating the derivatives of sigma and length parameter
loglike_partial_sigma_f <- function(sigma_f, x, y, sigma_y, length_parameter) {
  
  # Calculating K_y
  K_y <- K(x = x, sigma_f = sigma_f, length_parameter = length_parameter, sigma_y = sigma_y)
  
  # Calculating \alpha = K_y^-1y
  alpha <- solve(K_y, y)
  
  0.5 * sum(diag((tcrossprod(alpha) - solve(K_y)) %*% K(
    x = x,
    sigma_f = sqrt(sigma_f),
    length_parameter = length_parameter, sigma_y = sigma_y
  )))
}

# Calculating the derivatives of length parameter
loglike_partial_length_parameter <- function(length_parameter, sigma_y, x, y, sigma_f) {
  
  # Calculating K_y
  K_y <- K(x = x, sigma_f = sigma_f, length_parameter = length_parameter, sigma_y = sigma_y)
  
  
  # Calculating \alpha = K_y^-1y
  alpha <- solve(K_y, y)
  
  K_partial_l <- -(length_parameter^-3) * (sigma_f^2) * exp(-1 / (2 * length_parameter^2) * outer(c(x), c(x), "-")^2) %*% outer(c(x), c(x), "-")^2
  
  0.5 * sum(diag((tcrossprod(alpha) - solve(K_y)) %*% K_partial_l))
}

# gradient <- function(par, x, y, sigma_y){
#   
#   sigma_f <- par[1]
#   length_parameter <- par[2]
#   
#   c(loglike_partial_sigma_f(length_parameter = length_parameter,
#                             sigma_y = sigma_y,
#                             x = x,
#                             y = y,
#                             sigma_f = sigma_f),
#     loglike_partial_length_parameter(length_parameter = length_parameter,
#                                      sigma_y = sigma_y,
#                                      x = x,
#                                      y = y,
#                                      sigma_f = sigma_f))
#   
# }


# optim(par = c(0.4,0.1,0.7),fn = loglike_gp,method = "L-BFGS-B",lower=c(0.001,0.001,0.001),
#        x = x, y = y)
