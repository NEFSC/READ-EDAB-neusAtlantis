
t.s = 1964:1998
t.h = 1998:2020
t.p = 2020:2100

y.s = rnorm(length(t.s),1,0.1)
y.h = rnorm(length(t.h),1,0.1)
y.p = rnorm(length(t.p),1,0.1)
y.p[1] = y.h[length(y.h)]
y.h[1] = y.s[length(y.s)]

png(here::here('Figures','NEUS_Timeline.png'),width = 8, height = 3, units = 'in', res = 300)
plot(0,0,type = 'n',xlim =c(1960,2100),ylim = c(0.5,1.5), frame.plot = F, yaxt = 'n',xlab ='', ylab = '')
lines(t.s,y.s,col = 'lightgreen',lwd =2)
lines(t.h,y.h,col = 'red2',lwd =2)
lines(t.p,y.p,col = 'blue2',lwd =2)
dev.off()