### Task 1 - finding stationary distribution of Markov Chain

transition_matrix <- matrix(c(0.4, 0.3, 0.6, 0.7), 
                            nrow = 2)
steps <- 1000000
state <- 1
state_counter <- c(1, 0)

for (i in 1:steps) {
  next_state <- sample(1:2, 1, prob = transition_matrix[state, ])
  state_counter[next_state] <- state_counter[next_state] + 1
  state <- next_state
}

stationary_dist <- state_counter / sum(state_counter)
cat("Stationary distribution:", stationary_dist)

######################################################
### Task 2 - Travelling salesman problem using simulated annealing algorithm

set.seed(225)
n <- 10
earnings_matrix <- matrix(sample(1:n, size = n^2, replace = TRUE), nrow = n)

earnings <- function(route, earnings_matrix) {
  total <- 0
  for (i in 1:(length(route) - 1)) {
    total <- total + earnings_matrix[route[i], route[i + 1]]
  }
  return(total)
}

iter <- 1000000
current_route <- sample(1:n)
best_route <- current_route
best_earnings <- earnings(current_route, earnings_matrix)
lambda <- rep(NA, iter)

for (i in 1:iter) {
  new_route <- current_route
  swap <- sample(1:n, size = 2)
  new_route[swap] <- new_route[swap[2:1]]
  new_earnings <- earnings(new_route, earnings_matrix)
  lambda[i] <- log(1 + i) 

  if (new_earnings > best_earnings) {
    current_route <- new_route
    best_route <- current_route
    best_earnings <- new_earnings
  } else
  {
    if (runif(1) < exp(lambda[i] * (best_earnings - new_earnings))) {
      current_route <- new_route
    }
  }
}
cat("маршрут:", best_route, "заработок:", best_earnings)

######################################################
### Task 3

M <- matrix(c(10, 5, 4, 5), nrow=2)
fisher.test(M, alternative = "g")

######################################################
### Task 4

n <- 10 ** 6
x <- runif(n)
res <- sum((log(x, 10) * (x ** 2) * ((2 - x) ** 3)))/n
res

######################################################
### Task 5 - Getting to know bootstrapping and jackknife

data(Boston, package="MASS")
medv <- Boston$medv

## a
u = mean(medv)
u

## b
se <- sd(medv) / sqrt(length(medv))
se
# станд. ошибка медианы медианной цены жилых домов Бостона 0.4088611 тыс.$

## c
n <- 10000
ses <- numeric(n)

for (i in 1:n) {
  samp = sample(medv, length(medv), replace = TRUE)
  ses[i] = sd(samp) / sqrt(length(samp))
}

boot_se <- mean(ses)
boot_se
se - boot_se
# по бутстрепу получили отличие 0.0007759098 тыс.$ (очень малое)

## d
mean_inter <- c(u - 2 * boot_se, u + 2 * boot_se)
mean_inter

## e
med <- median(medv)
med

## f
meds <- numeric(n)

for (i in 1:n) {
  samp = sample(medv, length(medv), replace = TRUE)
  meds[i] = median(samp)
}
med_se <- sd(meds)
med_se
# станд. ошибка медианы медианной цены жилых домов 0.3767 тыс.$

## g
quant_10 = quantile(medv, probs = 0.1)
quant_10
## h
quants <- numeric(n)

for (i in 1:n) {
  samp = sample(medv, length(medv), replace = TRUE)
  quants[i] <- quantile(samp, probs = 0.1)
}

quant_se <- sd(quants)
quant_se
# станд. ошибка оценки 0.1 квантиля 0.5 тыс.$