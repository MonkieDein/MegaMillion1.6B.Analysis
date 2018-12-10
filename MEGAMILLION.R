remove(list=ls())

odd.win.jackpot = 1/(70*69*68*67*66/2/3/4/5)*(1/25)
odd.win.1M = 1/(70*69*68*67*66/2/3/4/5)*(24/25)
# 1 / 70C5 *  5C4 *65C1 * (1/25) yellowball
odd.win.10K = (1*2*3*4*5)/(70*69*68*67*66)*(5*65)/25
odd.win.500 = (1*2*3*4*5)/(70*69*68*67*66)*(5*65)*(24/25)
odd.win.200 = (1*2*3*4*5)/(70*69*68*67*66)*(5*4/2*65*64/2)*(1/25)
odd.win.10 =  (1*2*3*4*5)/(70*69*68*67*66)*(5*4/2*65*64/2)*(24/25) + (1*2*3*4*5)/(70*69*68*67*66)*(5*4/2*65*64*63/3/2)*(1/25)
odd.win.4 = (1*2*3*4*5)/(70*69*68*67*66)*(5*4/2*65*64*63/3/2)*(24/25)
odd.win.2 = 1/(70*69*68*67*66/65/64/63/62/61*25)

odd.inv = 1/c(odd.win.jackpot,odd.win.1M,odd.win.10K,odd.win.500,odd.win.200,odd.win.10,odd.win.4,odd.win.2)

MegaMillion = data.frame(Prize = c(1600000000,1000000,10000,500,200,10,4,2),W.B = c("5+1","5+0","4+1","4+0","3+1","3+0 or 2+1","1+1","0+1"), 
                         ODD.fraction = c(odd.win.jackpot,odd.win.1M,odd.win.10K,odd.win.500,odd.win.200,odd.win.10,odd.win.4,odd.win.2), odd.inverse = odd.inv)

lumpsum.ratio = 1600000000/904900000

Total.Revenue=sum(MegaMillion$Prize * MegaMillion$ODD.fraction * MegaMillion$odd.inverse[1])

# LumpSum Assumption
Total.LumpSum = Total.Revenue / lumpsum.ratio 

Highest.tax.bracket = 0.37
Mass.State.tax = 0.05

Total.tax = 0.37 + 0.05

Revenue.after.tax = Total.LumpSum * (1-Total.tax) 
Cost = MegaMillion$odd.inverse[1] * 2

Total.Profit = Revenue.after.tax - Cost

# Annuity Assumption
Total.Annuity = Total.Revenue 

Highest.tax.bracket = 0.37
Mass.State.tax = 0.05

Total.tax = Highest.tax.bracket + Mass.State.tax

Revenue.after.tax = Total.Annuity * (1-Total.tax) 
Annual.Revenue = Total.Annuity/30 * (1-Total.tax) 
# Revenue.after.tax / lumpsum.ratio

Cost = MegaMillion$odd.inverse[1] * 2
Annual.Cost = Cost/30


Total.Profit = Revenue.after.tax - Cost


# AN-i = (1- (1/(1+i))^30 )/ i
# Annual.Revenue * AN-i = Immediate.Revenue 
# Annual.Revenue * AN-i = Cost
# Annual.Revenue * AN-i - Cost = 0 ____ to find the expected interest rate that make the cost equal
#
f <- function (i) 32927036 * ((1- (1/(1+i))^30 )/ i) - 605150700
i.equal =  uniroot(f, lower = 0.00001, upper = 5, tol = 1e-9)$root

Annual.Income.After.Pay.Bank=32927036 *0.005

f <- function (i) 32927036 *0.995 * ((1- (1/(1+i))^30 )/ i) - 605150700
i.bank =  uniroot(f, lower = 0.00001, upper = 5, tol = 1e-9)$root


