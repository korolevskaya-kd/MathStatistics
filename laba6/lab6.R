a = -1.8;
b = 2;
h = 0.2;
n = 20;
x = seq(a, b, by = h)
e = rnorm(n, mean = 0, sd = 1)
y = 2 + 2 * x + e

#возмущения
#y[1] = y[1] + 10;
#y[n] = y[n] - 10;

#МНК
x_ = mean(x)
y_ = mean(y)
x_2 = x^2
x_2_ = mean(x_2)
y_2 = y^2
y_2_ = mean(y_2)
xy = x * y
xy_ = mean(xy)
b1 = (xy_ - x_ * y_) / (x_2_ - x_^2)
b0 = y_ - x_ * b1
b1
b0

#МНМ
mas_rQ = sign(x - median(x)) * sign(y - median(y))
rQ = mean(mas_rQ)
l = n / 4;
j = n - l + 1;
xl = x[l];
xj = x[j];
yl = y[l];
yj = y[j];
q_x = (xj - xl) / 2;
q_y = (yj - yl) / 2;
b1r = rQ * q_y / q_x;
b0r = median(y) - median(x) * b1r;
b1r
b0r

plot(x, y)
abline(a = 2, b = 2, col = "red")
abline(a = b0, b = b1)
abline(a = b0r, b = b1r, col = "blue")
legend("topleft", c("Выборка", "Модель", "МНК","МНМ"), col = c("black","red","black","blue"),
      ,lty = c(-1, 1, 1, 1), pch = c(21, NA, NA, NA))

