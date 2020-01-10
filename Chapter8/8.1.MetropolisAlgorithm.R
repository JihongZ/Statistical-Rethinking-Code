#########################################
# Markov Kingdom
#########################################
num_week <- 1e5
positions <- rep(0, num_week)
current <- 10
for (i in 1:num_week) {
  # record current position
  positions[i] = current

  # flip coin to generate proposal
  proposal <- current + sample(c(-1,1), size = 1)

  # now make sure he loops around the archipelago
  if(proposal < 1) proposal <- 10
  if(proposal >10) proposal <- 1

  # move ?
  prob_move <- proposal/current
  current <- ifelse(runif(1) < prob_move, proposal, current)

  # pause 3 second to see each move
  Sys.sleep(1)
  print(paste("Step", i, ": Island",current))
}


#########################################
# 8.3 map2stan
#########################################
library(rethinking)
data("rugged")
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000),]

m8.1 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data = dd
)
precis(m8.1)

## HMC
dd.trim <- dd[, c("log_gdp", "rugged", "cont_africa")]
str(dd.trim)

m8.1stan <- map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ),
  data = dd.trim
)
precis(m8.1stan)

## parallelize
m8.1stan_4chains <- map2stan(m8.1stan, chain=4, cores=4)
precis(m8.1stan)


post <- extract.samples(m8.1stan)
str(post)
pairs(post)
pairs(m8.1stan)
WAIC(m8.1stan)
plot(m8.1stan)
stancode(m8.1stan)
