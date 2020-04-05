n = 100
selection = rnorm(n,0,1)
x_ = mean(selection)
s_2 = sum((selection - x_)^2)/n
sigma = sqrt(s_2)


k = ceiling(1 + 3.3 * log10(n))
a = min(selection)
b = max(selection)
ab = seq(a + (b-a)/k, b-(b-a)/k, by = (b-a)/k)
ab[1] = -Inf
ab[k - 1] = Inf
n_i <- c()
p_i <- c()
np_i <-c()
n_inp_i <- c()
ans <- c()
for(i in 1:(k-2))
{
  n_i[i] = length(selection[selection <= ab[i+1] & selection >= ab[i]])
  p_i[i] = pnorm(ab[i + 1])- pnorm(ab[i]);
  np_i[i] = n * p_i[i]
  n_inp_i[i] = n_i[i] - np_i[i]
  ans[i] = (n_inp_i[i]^2) / np_i
}
n_i
sum(n_i)
p_i
sum(p_i)
np_i
sum(np_i)
n_inp_i
sum(n_inp_i)
ans
sum(ans)