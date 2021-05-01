# Plotting data
par(mfrow=c(1,2))

# Plotting the line with the SD parameters
plot(NA, ylim = c(-3,3), xlim = range(x_star),pch = 3,xlab="x",ylab="y")

# Plotting the estimation from estimated values
lines(simulated_data$true_x_grid, simulated_data$true_y_grid, col = "blue")
lines(x_star, f_mean_estimated, type = "l", col = "darkgreen")
lines(x_star, f_mean_estimated + 1.96 * sd_var_estimated, lty = 2, col = "darkgreen")
lines(x_star, f_mean_estimated - 1.96 * sd_var_estimated, lty = 2, col = "darkgreen")
points(x,y,pch=3)
title("Standard GP")

plot(NA, ylim = c(-3,3), xlim = range(x_star),pch = 3,xlab="x",ylab="y")
lines(simulated_data$true_x_grid, simulated_data$true_y_grid, col = "blue")
lines(x_star, pred_f_noise_x, col = "red")
lines(x_star, pred_f_noise_x + 1.96 * sqrt(diag(var_f_noise_x_matrix)), col = "red", lty = "dashed")
lines(x_star, pred_f_noise_x - 1.96 * sqrt(diag(var_f_noise_x_matrix)), col = "red", lty = "dashed")
points(x,y,pch=3)
title("NIGP")



sum(c(simulated_data$y_test) - pred_f_noise_x)

# Verifying  the loglikelihood 
mean(dnorm(x = simulated_data$y_test,mean = pred_f_noise_x,
      sd = sqrt(diag(var_f_noise_x_matrix)),log = TRUE )) %>% print

mean(dnorm(x = simulated_data$y_test,mean = f_mean_estimated,
           sd = sqrt(diag(var_estimated_matrix)),log = TRUE )) %>% print


# Veryfing the RMSE
rmse(pred = f_mean_estimated,obs = simulated_data$y_test) %>% print
rmse(pred = pred_f_noise_x,obs = simulated_data$y_test) %>% print

