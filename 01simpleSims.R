
n <- 1000 # nGenes
eD <- rlnorm(n, 0, 1) # diploid expression
eH <- rlnorm(n, 0, 1) # haploid expression
plot(eD, eH)
plot(eD, eH, log="xy")
plot(log(eD), log(eH))

# expression measure
ee <- (eD + eH)/2 # overall expression is a linear combintion

# ratio
rr <- eD/eH # expression ratio
l2rr <- log2(rr)
plot(ee, rr, log="xy")
plot(eD, rr, log="xy")
plot(eH, rr, log="xy")
plot(eH, l2rr, log="x")

pi01 <- 0.1 * ee + 0.01 * rr + rlnorm(n,0,.7)
hist(pi01/500)
summary(lm(pi01 ~ rr))

summary(lm(pi01 ~ rr + ee))

summary(lm(log(pi01) ~   log(rr)))
summary(lm((pi01) ~   (rr)))
summary(lm(pi01 ~ rr + eD))
summary(lm(log(pi01) ~ log(rr) + log(eD)))
summary(lm(pi01 ~ rr + ee + eH))
summary(lm(log(pi01) ~ log(rr) + log(ee) + log(eH)))
summary(lm(pi01 ~ rr + eH + eD))
summary(lm(log(pi01) ~ log(rr) + log(eD) + log(eH)))
plot(pi01, ee)
plot(pi01, ee, log="xy")
plot(pi01, rr, log="xy")


log(2) + log(3) == log(6)


aa <- seq(0.1, 1, 0.1)
bb <- seq(0.1, 1, 0.1)
persp(outer(aa, bb, function(x, y) x + y))

persp(outer(aa, bb, function(x, y) x + y + x * y))
persp(outer(aa, bb, function(x, y) x + y + 1/(x / y)))
persp(outer(aa, bb, function(x, y)  x * y))
?persp
contour(outer(aa, bb, function(x, y)  x / y))
