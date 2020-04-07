n = 20;
x = rnorm(n, 0, 1)
x_ = mean(x)
s = sqrt(sum((x-x_)^2) / n)

#доверительный интервал для матожидания
t = 2.093; #t(20)
#t = 1.984; #t(100)
left_m = x_ - s*t/sqrt(n-1)
right_m = x_ + s*t/sqrt(n-1)

#доверительный интервал для ср.кв.откл
t1 = 32.852; #t1(20)
t2 = 8.907;  #t2(20)
#t1 = 128.422;  #t1(100)
#t2 = 73.361;   #t2(100)
left_s = s*sqrt(n)/sqrt(t1)
right_s = s*sqrt(n)/sqrt(t2)

u = 1.96

#доверительный интервал для матожидания асимптотически
left_ma = x_ - s*u/sqrt(n)
right_ma = x_ + s*u/sqrt(n)

#доверительный интервал для ср.кв.откл асимптотически
m_4 = sum((x-x_)^4)/n
e = m4/s^4 - 3
left_sa = s * (1 - 0.5 * u * sqrt((e+2)/n))
right_sa = s * (1 + 0.5 * u * sqrt((e+2)/n))

left_m
right_m
left_s
right_s
left_ma
right_m
left_sa
right_sa

