### 30 Years ###
P = 950415
r = .06448/12
n = 360

Mort30 = P*((1+r)^n)*r/((1 + r)^n -1)
Mort30
M30tot = Mort30*n
M30int = Mort30*n - P
M30Bar = matrix(c(P,0,0,M30int,P,M30int),2,3, byrow = F)

M30 = barplot(M30Bar,main = "Total cost of Average Home in San Diego 2022 (30-yr)",
              col = c("tomato1","seagreen2"),
              names.arg = c("Principal Borrowed","Interest","Total Cost"),
              cex.axis = .7, cex.names = .75, cex.main = .95,
              ylab = "Amount in US Dollars($)")

### 20 Years ###
P = 950415
r = .05997/12
n = 240

Mort20 = P*((1+r)^n)*r/((1 + r)^n -1)
Mort20
M20tot = Mort20*n
M20int = Mort20*n - P
M20Bar = matrix(c(P,0,0,M20int,P,M20int),2,3, byrow = F)

M20 = barplot(M20Bar,main = "Total cost of Average Home in San Diego 2022 (20-yr)",
              col = c("tomato1","seagreen2"),
              names.arg = c("Principal Borrowed","Interest","Total Cost"),
              cex.axis = .8, cex.names = .75, cex.main = .95,
              ylab = "Amount in US Dollars($)", ylim = c(0,1750000))

### 15 Years ###

P = 950415
r = .05759/12
n = 180

Mort15 = P*((1+r)^n)*r/((1 + r)^n -1)
Mort15
M15tot = Mort15*n
M15int = Mort15*n - P
M15Bar = matrix(c(P,0,0,M15int,P,M15int),2,3, byrow = F)

M15 = barplot(M15Bar,main = "Total cost of Average Home in San Diego 2022 (15-yr)",
              col = c("tomato1","seagreen2"),
              names.arg = c("Principal Borrowed","Interest","Total Cost"),
              cex.axis = .8, cex.names = .75, cex.main = 1,
              ylab = "Amount in US Dollars($)", ylim = c(0,1750000))

### 10 Years ###

P = 950415
r = .0549/12
n = 120

Mort10 = P*((1+r)^n)*r/((1 + r)^n -1)
Mort10

M10tot = Mort10*n
M10int = Mort10*n - P
M10Bar = matrix(c(P,0,0,M10int,P,M10int),2,3, byrow = F)

M10 = barplot(M10Bar,main = "Total cost of Average Home in San Diego 2022 (10-yr)",
              col = c("tomato1","seagreen2"),
              names.arg = c("Principal Borrowed","Interest","Total Cost"),
              cex.axis = .8, cex.names = .75, cex.main = 1,
              ylab = "Amount in US Dollars($)", ylim = c(0,1750000))

### Monthly Payment Comparison ###

MonthPayVal = c(Mort10,Mort15,Mort20,Mort30)
Interest = c(5.49,5.759,5.997,6.448)
MonthPay = barplot(MonthPayVal, main ="Monthly Payment of Average Home in San Diego 2022",
                   names.arg = c("10-Year", "15-Year","20-Year","30-Year"),
                   col = c("hot pink","purple","cyan","green"),
                   cex.axis = .75,cex.names = 1, cex.main = .95,
                   ylab = "Amount in US Dollars($)", ylim = c(0,12500))

text(MonthPay, MonthPayVal/.93, labels = round(MonthPayVal, digits = 2), cex = 1.1)
text(MonthPay, MonthPayVal/2, labels = c("5.49%","5.759%","5.997%","6.448%"), cex =1.1)


### Total Interest Amount ###

interests = c(M10int,M15int,M20int,M30int)

intinc = c(abs(M10int - M15int)/M10int,abs(M15int - M20int)/M15int,
           abs(M20int - M30int)/M20int)
intinc
InterestIncrease = barplot(interests, col = c("lightsalmon","mediumaquamarine"),
                           main = "Percent Increase per loan", 
                           names.arg = c("10-yr loan","15-yr loan",
                                    "20-yr loan","30-yr loan"),
                           ylab = "Amount in USD($)")
text(InterestIncrease,interests/2,labels = c("","64.26%","45.08%","75.68%"))

### Potential profits ###

GrowthRate = 1.2496
years30 = seq(0,30,by=1)
MAp30A = 0
MAp30A[1] = P + 500000
for (i in seq(2,31,by = 1)){
  MAp30A[i] = MAp30A[i-1]*(1.0845)
}

MAp30B= 0
MAp30B[1] = P - 500000
for (i in seq(2,31,by = 1)){
  MAp30B[i] = MAp30B[i-1]*(1.0845)
}

MAp30= 0
MAp30[1] = P
for (i in seq(2,31,by = 1)){
  MAp30[i] = MAp30[i-1]*(1.0845)
}

P1 = 950415 + 500000
P2 = P - 500000
r = .06448/12
n = 360

Mort30A = P1*((1+r)^n)*r/((1 + r)^n -1)
M30Atot = Mort30A*n

Mort30B = P2*((1+r)^n)*r/((1 + r)^n -1)
M30Btot = Mort30B*n

plot(years30,MAp30A, type = 'l', col = 'red', lwd = 2,ylim = c(0,15000000),
     main = "Is it worth getting a 30 year loan?", ylab = "Total Value in USD($)"
     ,xlab = "Years",cex.axis = .8, cex.main = .9, xlim = c(0,30))


lines(years30,MAp30B, col = 'blue', lwd = 2)
lines(years30,MAp30, col = 'green', lwd = 2)
lines(years30,M30tot + years30*0, col = 'green', lty = 2, lwd = 2)
lines(years30,M30Atot + years30*0, col = 'red', lty = 2, lwd = 2)
lines(years30,M30Btot + years30*0, col = 'blue', lty = 2, lwd = 2)
lines(11.25 + years30*0,seq(-10000000,16000000,length.out = 31), lwd = 2)
legend(-1,15595000,c("$1,450,000 - Above Average","$950,000 - Average","$450,000 - Below Average",
                        "Total Payment over 30 Years - Above Average", "Total Payment over 30 Years - Average",
                        "Total Payment over 30 Years - Below Average"),lty = c(1,1,1,2,2,2),
       col = c("red","blue","green","red","blue", "green"),cex = .6)


