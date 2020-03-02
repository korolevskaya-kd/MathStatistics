  #Построение гистограммы и функции плотности распределения
  size = 1000; 
  kk = 1 + 3.3 * log(size)
  xx <- rnorm(n = size, mean = 0, sd  = 1);  #нормальное распределение
  #xx <- rcauchy(n = size, location=0, scale=1) #распределение Коши
  #xx <- rpois(n = size, lambda = 10)  #распределение Пуасона
  #xx <- runif(n = size,-sqrt(3),sqrt(3))   #равномерное распределение
  #D <- DExp(rate = 1/sqrt(2)) #распределение Лапласа
  #xx <-r(D)(size)            #распределение Лапласа
  xx
  hist(xx,probability = TRUE, main = paste("Нормальное распределение. n = 1000"),breaks = kk);
  a = min(xx)
  b = max(xx)
  step = (b-a) / kk
  x = array(c(T,F),dim=c(kk))
  for(i in 1:kk + 1){
    x[i] =round( a + i * step, 0) 
  }
  curve(dnorm(x), col = "blue", add = T) #график плотности вероятности нормального распределения
  #curve(dcauchy(x), col = "blue", add = T) #график плотности вероятности распределения Коши
  #lines(x, dpois(x, lambda = 10), col = "blue")  #график плотности вероятности распределения Пуасона
  #curve(dunif(x, -sqrt(3),sqrt(3)), col = "blue", add = T) #график плотности вероятности равномерного распределения
  #curve(d(D)(x), col = "blue", add = T) #график плотности вероятности распределения Лапласа


nn = 10;
X = array(c(T,F),dim=c(1000)) #массив для выборочного среднего
MedX = array(c(T,F),dim=c(1000)) #массив для выборочной медианы 
ZR = array(c(T,F),dim=c(1000)) #массив для полусуммы экстремальных выборочных элементов
ZQ = array(c(T,F),dim=c(1000)) #массив для полусуммы квартилей
ZTr = array(c(T,F),dim=c(1000)) #массив для усеченного среднего
for(j in 1:1000){
  selection <- rnorm(n = nn, mean = 0, sd  = 1);  #нормальное распределение
  #D <- DExp(rate = 1/sqrt(2)) #распределение Лапласа
  #selection <-r(D)(nn)            #распределение Лапласа
  #selection <- rcauchy(n = nn, location=0, scale=1) #распределение Коши
  #selection <- rpois(n = nn, lambda = 10)  #распределение Пуасона
  #selection <- runif(n = nn,-sqrt(3),sqrt(3))   #равномерное распределение
  
  selection = sort(selection)
  
  #выборочное среднее
  s = 0
  s = sum(selection)
  X[j] = s/nn
  
  #выборочная медиана (n - всегда четное)
  MedX[j] = (selection[nn / 2] + selection[nn / 2 + 1]) / 2
  
  #Полусумма экстремальных выборочных элементов:
  a = min(selection);
  b = max(selection);
  ZR[j] = (a + b) / 2
  
  #полусумма квартилей
  m = nn %% 4 #mod(n, 4);
  if (m == 0){
    i = nn / 4;
  }
  if( m != 0){
    i = floor(nn / 4) + 1;
  }
  z1 = selection[i];
  z2 = selection[nn - i + 1];
  ZQ[j] = (z1 + z2)/2
  
  #усеченное среднее
  r = nn / 4;
  koef = 1 / (nn - 2 * r);
  s = 0;
  arr = array(c(T,F),dim=c(nn - 2*r))
  for (i in floor(r+1):ceiling(nn-r)){
    arr[i - r] = selection[i];
  }
  s = sum(arr)
  ZTr[j] = koef * s
}
XX = sum(X) / 1000  
medx = sum(MedX) / 1000
zr = sum(ZR) / 1000
zq = sum(ZQ) / 1000
ztr = sum(ZTr) / 1000


#выборочная дисперсия

DX = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DX[i] = (X[i] - XX) *  (X[i] - XX)
}
dx = sum(DX) / 1000

DMedX = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DMedX[i] = (MedX[i] - medx) *  (MedX[i] - medx)
}
dmedx = sum(DMedX) / 1000


DZR = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZR[i] = (ZR[i] - zr) *  (ZR[i] - zr)
}
dzr = sum(DZR) / 1000


DZQ = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZQ[i] = (ZQ[i] - zq) *  (ZQ[i] - zq)
}
dzq = sum(DZQ) / 1000


DZTr = array(c(T,F),dim=c(1000))
for (i in 1:1000){
  DZTr[i] = (ZTr[i] - ztr) *  (ZTr[i] - ztr)
}
dztr = sum(DZTr) / 1000

XX
medx
zr
zq
ztr
dx
dmedx
dzr
dzq
dztr