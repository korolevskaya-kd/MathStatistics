mu <- c(0,0)  
p = 0.9
p1 = -0.9
Sigma <- matrix(c(1,p,p, 1), 2)
Sigma2 <-  matrix(c(100,100*p1,100*p1, 100), 2)
n = 20
j = 1000
R <-c()
RS <-c()
RQ <-c()
while (j > 0)
{
  j = j - 1;
  normir <- mvrnorm(n, mu = mu, Sigma = Sigma )
  #normir <- (0.9 * mvrnorm(n, mu = mu, Sigma = Sigma) + 0.1 * mvrnorm(n, mu = mu, Sigma = Sigma2)) #смесь распределений
  r = cor(normir[, 1], normir[, 2], method = "pearson") #пирсона
  rs = cor(normir[, 1], normir[, 2], method = "spearman") #Спирмена
  R = append(R, r);
  RS = append(RS, rs);
  n12 = normir[normir[,1] > median(normir[,1]),]
  n1 = n12[n12[,2] > median(normir[,2]),]
  n34 = normir[normir[,1] < median(normir[,1]),]
  n3 = n34[n34[,2] < median(normir[,2]),]
  count13 = length(n1[,1]) + length(n3[,1])
  rq = (count13 - (n - count13)) / n
  RQ = append(RQ, rq);
}
Ezr = sum(R) / 1000
Ezr_2 = sum(R^2) / 1000
Dzr = Ezr_2 - Ezr^2
Ezrs = sum(RS) / 1000
Ezrs_2 = sum(RS^2) / 1000
Dzrs = Ezrs_2 - Ezrs^2
Ezrq = sum(RQ) / 1000
Ezrq_2 = sum(RQ^2) / 1000
Dzrq = Ezrq_2 - Ezrq^2

Ezr
Ezr_2
Dzr
Ezrs
Ezrs_2
Dzrs
Ezrq
Ezrq_2
Dzrq



mu <- c(0,0)  
p = 0
Sigma <- matrix(c(1,p,p, 1), 2)
n = 20
#set.seed(100)
selection <- mvrnorm(n = n, mu = mu, Sigma = Sigma ) 
require(ellipse)
confidence.ellipse <- ellipse(Sigma,centre=mu,level=0.99,npoints=100)
plot(confidence.ellipse,type="l", xlim=c(-3, 3),ylim=c(-3, 3), main = "rho = 0.9")
par(new=TRUE)
plot(selection,  axes = FALSE, ann = FALSE, col="blue", pch=19)




