# binimial density function
x = 1:9
y1 = dbinom(x, size = 9, prob = 0.4)
y = dbinom(x, size = 9, prob = 0.5)
y2 = dbinom(x, size = 9, prob = 0.6)
y3 = dbinom(x, size = 9, prob = 0.7)
plot(x, y, type = "l")
lines(x, y1, col = "purple")
lines(x, y2, col = "green")
lines(x, y3, col = "red")


## 2.4.1 Grid Approximation

# set number of points in grids, more points means more precision
n_grid = 50

# define grid
p_grid <- seq(from=0, to=1, length.out = n_grid)

# define prior (a uniform prior distribution)
prior <- rep(1, n_grid)

# alternative prior distribution
prior2 <- ifelse(p_grid < .5, 0, 1)
prior2 <- exp(-5 *abs(p_grid-.5))

# compute likelihood at each value in grid
likelihood <- dbinom(6, size = 9, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood*prior
unstd.posterior2 <- likelihood*prior2

# standardized the posterior, so that it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)
posterior2 <- unstd.posterior2/sum(unstd.posterior2)

# visualization
plot(p_grid, posterior, type = "b",
     xlab="probability of water", ylab="posterior probability", ylim = c(0, 0.12))
lines(p_grid, posterior2, type = "b", col = "red")
mtext(paste(n_grid,"points"))