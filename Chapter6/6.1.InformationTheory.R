## load packages
library(rethinking)
options(mc.cores = parallel::detectCores())
## True data-generating process
N <- 20
kseq <- 1:5 # number of parameters
dev <- sapply(kseq, function(k) {
  print(k)
  r <- mcreplicate(1e4, sim.train.test(N=N, k=k))
  c(mean(r[1,]), mean(r[2,]), sd(r[1,]), sd(r[2,]))
})

## Figure 6.7
plot(1:5, dev[1,],
     ylim=c(min(dev[1:2,])-5, max(dev[1:2,])+10),
     xlim=c(1, 5.1),
     xlab = "number of parameter",
     ylab = "deviance",
     pch=16, col=rangi2
     )
mtext(concat("N = ", N))
text(x =1.1, y =50, "training data", col = rangi2)
text(x =1.1, y =65+1, "test data")
points( (1:5)+0.1, dev[2,])
lines( (1:5)+0.1, dev[2,])
lines( 1:5, dev[1,], col=rangi2)
for (i in kseq) {
  pts_in <- dev[1,i] + c(-1, 1)*dev[3,i]
  pts_out <- dev[2,i] + c(-1, 1)*dev[4,i]
  lines(c(i,i), pts_in, col=rangi2)
  lines(c(i,i)+0.1, pts_out)
}



# 6.5.1 Model Comparison
data("milk")
str(milk)

d <- milk[complete.cases(milk),]
d$neocortex <- d$neocortex.perc / 100
dim(d)

a.start <- mean(d$kcal.per.g)
sigma.start <- log(sd(d$kcal.per.g))

# a model with neither predictors
m6.11 <- map(
  alist(
    kcal.per.g ~ dnorm(a, exp(log.sigma))
  ),
  data = d, start = list(a=a.start, log.sigma=sigma.start)
)

# a model with only neocortex
m6.12 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bn*neocortex
  ),
  data = d, start = list(a=a.start, bn=0, log.sigma=sigma.start)
)

# a model with only log mass
m6.13 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bm*log(mass)
  ),
  data = d, start = list(a=a.start, bm=0, log.sigma=sigma.start)
)

# a model with both neocrotex and log mass
m6.14 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bn*neocortex + bm*log(mass)
  ),
  data = d, start = list(a=a.start, bn=0, bm=0, log.sigma=sigma.start)
)

# Calculate WAIC
WAIC(m6.11)
WAIC(m6.12)
WAIC(m6.13)
WAIC(m6.14)

milk.models <- compare(m6.11, m6.12, m6.13, m6.14)
milk.models

plot(milk.models, SE=TRUE, dSE=TRUE)



#  Comparnf the posterior densities of parameters for the four models
coeftab(m6.11, m6.12, m6.13, m6.14)
plot(coeftab(m6.11, m6.12, m6.13, m6.14))




#########################################
## Model Averaging
#########################################
# compute counterfactual predictions
# neocortex from .5 to .8
nc.seq <- seq(from=0.5, to=0.8, length.out = 30)
d.predict <- list(
  kcal.per.g = rep(0, 30),
  neocortex = nc.seq,
  mass = rep(4.5, 30)
)

pred.m6.14 <- link(m6.14, data = d.predict)
mu <- apply(pred.m6.14, 2, mean)
mu.PI <- apply(pred.m6.14, 2, PI)

# plot it all
plot(kcal.per.g ~ neocortex, d, col=rangi2)
lines(nc.seq, mu, lty =2)
lines(nc.seq, mu.PI[1,], lty=2)
lines(nc.seq, mu.PI[2,], lty=2)


milk.ensemble <- ensemble(m6.11, m6.12, m6.13, m6.14, data=d.predict)
mu <- apply(milk.ensemble$link, 2, mean)
mu.PI <- apply(milk.ensemble$link, 2, PI)
lines(nc.seq, mu)
shade(mu.PI, nc.seq)
