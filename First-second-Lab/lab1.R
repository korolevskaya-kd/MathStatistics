  #���������� ����������� � ������� ��������� �������������
  size = 1000; 
  kk = 1 + 3.3 * log(size)
  xx <- rnorm(n = size, mean = 0, sd  = 1);  #���������� �������������
  #xx <- rcauchy(n = size, location=0, scale=1) #������������� ����
  #xx <- rpois(n = size, lambda = 10)  #������������� �������
  #xx <- runif(n = size,-sqrt(3),sqrt(3))   #����������� �������������
  #D <- DExp(rate = 1/sqrt(2)) #������������� �������
  #xx <-r(D)(size)            #������������� �������
  xx
  hist(xx,probability = TRUE, main = paste("���������� �������������. n = 1000"),breaks = kk);
  a = min(xx)
  b = max(xx)
  step = (b-a) / kk
  x = array(c(T,F),dim=c(kk))
  for(i in 1:kk + 1){
    x[i] =round( a + i * step, 0) 
  }
  curve(dnorm(x), col = "blue", add = T) #������ ��������� ����������� ����������� �������������
  #curve(dcauchy(x), col = "blue", add = T) #������ ��������� ����������� ������������� ����
  #lines(x, dpois(x, lambda = 10), col = "blue")  #������ ��������� ����������� ������������� �������
  #curve(dunif(x, -sqrt(3),sqrt(3)), col = "blue", add = T) #������ ��������� ����������� ������������ �������������
  #curve(d(D)(x), col = "blue", add = T) #������ ��������� ����������� ������������� �������


nn = 10;
X = array(c(T,F),dim=c(1000)) #������ ��� ����������� ��������
MedX = array(c(T,F),dim=c(1000)) #������ ��� ���������� ������� 
ZR = array(c(T,F),dim=c(1000)) #������ ��� ��������� ������������� ���������� ���������
ZQ = array(c(T,F),dim=c(1000)) #������ ��� ��������� ���������
ZTr = array(c(T,F),dim=c(1000)) #������ ��� ���������� ��������
for(j in 1:1000){
  selection <- rnorm(n = nn, mean = 0, sd  = 1);  #���������� �������������
  #D <- DExp(rate = 1/sqrt(2)) #������������� �������
  #selection <-r(D)(nn)            #������������� �������
  #selection <- rcauchy(n = nn, location=0, scale=1) #������������� ����
  #selection <- rpois(n = nn, lambda = 10)  #������������� �������
  #selection <- runif(n = nn,-sqrt(3),sqrt(3))   #����������� �������������
  
  selection = sort(selection)
  
  #���������� �������
  s = 0
  s = sum(selection)
  X[j] = s/nn
  
  #���������� ������� (n - ������ ������)
  MedX[j] = (selection[nn / 2] + selection[nn / 2 + 1]) / 2
  
  #��������� ������������� ���������� ���������:
  a = min(selection);
  b = max(selection);
  ZR[j] = (a + b) / 2
  
  #��������� ���������
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
  
  #��������� �������
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


#���������� ���������

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