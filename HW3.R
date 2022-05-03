library(TSA)
library(stats)

##4.5##
noise <- rnorm(5000)
yt <- list();
yt <- append(a1,0)
for (i in 2:200){
  yt <- append(a1,a1[[i-1]]*.3 + noise[i]);}
yt <- unlist(a1)
acf(yt)


##4.9##
y = ARMAacf(ar = c(-0.5, -0.6), lag.max=20)

plot(y, x = 1:21, type = "h", ylim = c(-1,1))

##4.19##
ARMAacf(ar = -0.5, lag.max = 7)
ARMAacf(ma = -c(0.5, -0.25, 0.125, -0.0625, 0.03125, -0.0015625))

##4.20##
ARMAacf(ma = -c(1, -0.5, 0.25, -0.125, 0.0625, -0.03125, 0.015625))
ARMAacf(ar = -0.5, ma = -0.5, lag.max = 8)
