# Generating simulated data function
sim_data <- function(n, p, sigma_y, sigma_f, length_parameter, seed = NULL) {
  
  
  # Setting a seed
  set.seed(seed)
  
  # Creating the input variance
  x <- replicate(p, runif(min = 0, max = 6, n = N) ) %>% apply(2, sort)
  
  # Creating the covariance matrix structure
  K_x <- K(
    x = x, sigma_f = sigma_f,
    length_parameter = length_parameter, sigma_y = sigma_y
  )
  
  
  # Generating a random sample for y
  y <- MASS::mvrnorm(
    n = 1,
    mu = rep(0, n),
    Sigma = K_x
  )
  
  
  
  return(list(
    x = x,
    y = y
  ))
}

# Simulating a near-square-wave
sim_square_wave <- function(n, n_test, sigma_y, sigma_x, seed = NULL) {
  
  # Setting a seed
  set.seed(seed)
  
  # Generating the x
  true_x <- matrix(sort(runif(-10, 10, n = n)), ncol = 1)
  # true_x <- matrix(sort(runif(-10, 10, n = n)), ncol = 1)
  
  
  x <- apply(true_x, 2, function(z) {
    z + rnorm(n, mean = 0, sd = sigma_x)
  })
  
 
  # True x grid
  true_x_grid <- sort(seq(from = min(x), to = max(x), length.out = 1000))
  
  true_y_f <- function(x) {
    (sin(x * 1) + 1 / 3 * sin(3 * x * 1) + (1/5) *sin (5 * x * 1))
  } #+ 1/5*sin(5*x)
  
  true_y <- sapply(true_x, true_y_f)
  
  
  y <- true_y + rnorm(n, mean = 0, sd = sigma_y)
  
  true_y_grid <- sapply(true_x_grid, true_y_f)

  # Generating the test set
  true_x_test <- matrix(sort(seq(from = -10, to = 10, length.out = n_test)),nrow = n_test)
  x_test <- apply(true_x_test, 2, function(z) {
    z + rnorm(n_test, mean = 0, sd = sigma_x)
  })
  
  x_test <- apply(x_test,2,sort)   
  
  true_y_test <- sapply(true_x_test,true_y_f)
  y_test <- true_y_test + rnorm(n_test, mean = 0, sd = sigma_y)
  # y_test <- true_y_test
  
  
  return(list(
    x = x, y = y,
    x_test = x_test, y_test = y_test,
    true_x = true_x, true_y = true_y,
    true_x_grid = true_x_grid, true_y_grid = true_y_grid
  ))
}

# Simulation sin
sim_sin <- function(n, n_test, sigma_y, sigma_x, seed = NULL) {
  
  # Setting a seed
  set.seed(seed)
  
  # Generating the x
  true_x <- matrix(sort(runif(-10, 10, n = n)), ncol = 1)

  
  x <- apply(true_x, 2, function(z) {
    z + rnorm(n, mean = 0, sd = sigma_x)
  })
  
  true_x_grid <- sort(seq(from = min(x), to = max(x), length.out = 1000))


  true_y_f <- function(x) {
    sin(x)
  } 
  
  true_y <- sapply(true_x, true_y_f)
  
  y <- true_y + rnorm(n, mean = 0, sd = sigma_y)
  
  true_y_grid <- sapply(true_x_grid, true_y_f)

  # Generating the test set
  true_x_test <- matrix(sort(seq(from = -10, to = 10, length.out = n_test)),nrow = n_test)
  x_test <- apply(true_x_test, 2, function(z) {
    z + rnorm(n_test, mean = 0, sd = sigma_x)
  })
  
  x_test <- apply(x_test,2,sort)   
  
  true_y_test <- sapply(true_x_test,true_y_f)
  y_test <- true_y_test + rnorm(n_test, mean = 0, sd = sigma_y)
  
  # y_test <- true_y_test
  
  return(list(
    x = x, y = y,
    true_x = true_x, true_y = true_y,
    x_test = x_test, y_test = y_test,
    true_x_grid = true_x_grid, true_y_grid = true_y_grid
  ))
}


# Exp sin
sim_exp_sin <- function(n, n_test, sigma_y, sigma_x, seed = NULL){
  
  # Setting a seed
  set.seed(seed)
  
  # Generating the x
  true_x <- matrix(sort(runif(-10, 10, n = n)), ncol = 1)
  
  
  x <- apply(true_x, 2, function(z) {
    z + rnorm(n, mean = 0, sd = sigma_x)
  })
  
  true_x_grid <- sort(seq(from = min(x), to = max(x), length.out = 1000))
  
  
  true_y_f <- function(x) {
    exp(-0.2*x)*sin(x)
  } 
  
  true_y <- sapply(true_x, true_y_f)
  
  y <- true_y + rnorm(n, mean = 0, sd = sigma_y)
  
  true_y_grid <- sapply(true_x_grid, true_y_f)
  
  # Generating the test set
  true_x_test <- matrix(sort(seq(from = -10, to = 10, length.out = n_test)),nrow = n_test)
  x_test <- apply(true_x_test, 2, function(z) {
    z + rnorm(n_test, mean = 0, sd = sigma_x)
  })
  
  x_test <- apply(x_test,2,sort)   
  
  true_y_test <- sapply(true_x_test,true_y_f)
  y_test <- true_y_test + rnorm(n_test, mean = 0, sd = sigma_y)
  # y_test <- true_y_test
  
  return(list(
    x = x, y = y,
    true_x = true_x, true_y = true_y,
    x_test = x_test, y_test = y_test,
    true_x_grid = true_x_grid, true_y_grid = true_y_grid
  ))
  
}

# Tan sin
sim_tan_sin <- function(n, n_test, sigma_y, sigma_x, seed = NULL){
  
  # Setting a seed
  set.seed(seed)
  
  # Generating the x
  true_x <- matrix(sort(runif(-10, 10, n = n)), ncol = 1)
  
  
  x <- apply(true_x, 2, function(z) {
    z + rnorm(n, mean = 0, sd = sigma_x)
  })
  
  true_x_grid <- sort(seq(from = min(x), to = max(x), length.out = 1000))
  
  
  true_y_f <- function(x) {
    tan(0.15*x)*sin(x)
  } 
  
  true_y <- sapply(true_x, true_y_f)
  
  y <- true_y + rnorm(n, mean = 0, sd = sigma_y)
  
  true_y_grid <- sapply(true_x_grid, true_y_f)
  
  # Generating the test set
  true_x_test <- matrix(sort(seq(from = -10, to = 10, length.out = n_test)),nrow = n_test)
  x_test <- apply(true_x_test, 2, function(z) {
    z + rnorm(n_test, mean = 0, sd = sigma_x)
  })
  
  x_test <- apply(x_test,2,sort)   
  
  true_y_test <- sapply(true_x_test,true_y_f)
  y_test <- true_y_test + rnorm(n_test, mean = 0, sd = sigma_y)
  # y_test <- true_y_test
  
  return(list(
    x = x, y = y,
    true_x = true_x, true_y = true_y,
    x_test = x_test, y_test = y_test,
    true_x_grid = true_x_grid, true_y_grid = true_y_grid
  ))
  
}


# x^2 tanh(cos(x))
sim_tanh_cos <- function(n, n_test, sigma_y, sigma_x, seed = NULL){
  
  # Setting a seed
  set.seed(seed)
  
  # Generating the x
  true_x <- matrix(sort(runif(-10, 10, n = n)), ncol = 1)
  
  
  x <- apply(true_x, 2, function(z) {
    z + rnorm(n, mean = 0, sd = sigma_x)
  })
  
  true_x_grid <- sort(seq(from = min(x), to = max(x), length.out = 1000))
  
  
  true_y_f <- function(x) {
    0.2*(x^2)*tanh(cos(x))
  } 
  
  true_y <- sapply(true_x, true_y_f)
  
  y <- true_y + rnorm(n, mean = 0, sd = sigma_y)
  
  true_y_grid <- sapply(true_x_grid, true_y_f)
  
  # Generating the test set
  true_x_test <- matrix(sort(seq(from = -10, to = 10, length.out = n_test)),nrow = n_test)
  x_test <- apply(true_x_test, 2, function(z) {
    z + rnorm(n_test, mean = 0, sd = sigma_x)
  })
  
  x_test <- apply(x_test,2,sort)   
  
  true_y_test <- sapply(true_x_test,true_y_f)
  y_test <- true_y_test + rnorm(n_test, mean = 0, sd = sigma_y)
  # y_test <- true_y_test
  
  return(list(
    x = x, y = y,
    x_test = x_test, y_test = y_test,
    true_x = true_x, true_y = true_y,
    true_x_grid = true_x_grid, true_y_grid = true_y_grid
  ))
  
}

# 0.5*log(sin)
sim_log_sin <- function(n, n_test, sigma_y, sigma_x, seed = NULL){
  
  # Setting a seed
  set.seed(seed)
  
  # Generating the x
  true_x <- matrix(sort(runif(-10, 10, n = n)), ncol = 1)
  
  
  x <- apply(true_x, 2, function(z) {
    z + rnorm(n, mean = 0, sd = sigma_x)
  })
  
  true_x_grid <- sort(seq(from = min(x), to = max(x), length.out = 1000))
  
  
  true_y_f <- function(x) {
    0.5*log((x^2)*(sin(2*x)+2) +1)
  } 
  
  true_y <- sapply(true_x, true_y_f)
  
  y <- true_y + rnorm(n, mean = 0, sd = sigma_y)
  
  true_y_grid <- sapply(true_x_grid, true_y_f)
  
  # Generating the test set
  true_x_test <- matrix(sort(seq(from = -10, to = 10, length.out = n_test)),nrow = n_test)
  x_test <- apply(true_x_test, 2, function(z) {
    z + rnorm(n_test, mean = 0, sd = sigma_x)
  })
  
  x_test <- apply(x_test,2,sort)   
  
  true_y_test <- sapply(true_x_test,true_y_f)
  y_test <- true_y_test + rnorm(n_test, mean = 0, sd = sigma_y)
  # y_test <- true_y_test
  
  return(list(
    x = x, y = y,
    x_test = x_test, y_test = y_test,
    true_x = true_x, true_y = true_y,
    true_x_grid = true_x_grid, true_y_grid = true_y_grid
  ))
  
}
