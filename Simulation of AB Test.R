rm(list = ls()) 
N <- 17636   
n <- N/2   

p_control <- .2
ate_norm <- rnorm(n,0,0.01)
p_treat <- p_control+ate_norm
true_success_rate <- c()
for (i in 1:5000){
  tempcontrol <- runif(n) 
  success_control <- as.integer(tempcontrol < p_control)
  temptreat <- runif(n) 
  success_treat <- as.integer(temptreat < p_treat[i])
  results_t <- t.test(success_treat, success_control, alternative = "greater")
  if (results_t$p.value < 0.5 ){
    true_success_rate <- c(true_success_rate, p_treat[i])
  } else {
    true_success_rate <- c(true_success_rate, p_control)
  }
}

hist(true_success_rate)
mean(true_success_rate)
