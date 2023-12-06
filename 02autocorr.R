
library(mgcv)
setwd("~/Marchantia/expression/")


mmmm <- readRDS("mmmm.Rds")


nnnn <- mmmm[mmmm$chr=="chr6",]
nnnn <- nnnn[is.finite(nnnn$l10X0), ]
plot(l10X0~mid, data=nnnn)
#plot(pi0~mid, data=nnnn)



pi.lo <- loess(l10X0~mid, data=nnnn[is.finite(nnnn$l10X0),], span = 1/10)

points(nnnn[is.finite(nnnn$l10X0),]$mid, predict(pi.lo), type="l", lwd=2)

lm00 <- lm(l10X0 ~ 1, data = nnnn)

gam00 <- gam(l10X0 ~  s(mid), data = nnnn)
summary(lm00)
summary(gam00)
par(mfrow=c(2,2))
plot(lm00)
gam.check(gam00)
par(mfrow=c(1,1))

hist(resid(lm00), col ="#00FF0030")
hist(resid(gam00), add=T, col ="#FF000030")

?acf


acf(nnnn$l10X0)

# autocorrelation arises from smothong window analyses
nn <- 1000
rr <- rnorm(nn)
acf(rr)
?Matrix::band
mat <- Matrix::band(matrix(1, nn, nn), -20, 20)

plot(rr)
points((mat %*% rr)/41, pch=2, col=2)
overlapping <- as.vector(mat %*% rr)
nonOverlapping <- overlapping[seq(1, nn, by=42)]

acf(overlapping)
acf(nonOverlapping)

plot(overlapping/41)
plot(nonOverlapping/41)
