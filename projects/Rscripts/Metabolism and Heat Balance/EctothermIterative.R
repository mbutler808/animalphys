m = 61235*10^3

smr = 1.5*(m)^.8/1000   #lizard equation
#smr = 1.9*(m)^.82/1000   #Varanus equation
#smr = 1.3*(m)^.86/1000   #Varanus equation
#smr = 6*m^0.65/1000
#smr = 2.5*(m)^.7/1000
rmr = 1.2*smr
amr = 3.4*rmr

C = 2603*m^.148/1000
C = C*25

iterate <- function( Hm, C, ta, times) {

  Hm_ta <- Hm*(2.5^((ta - 20)/10))	
  dat <- data.frame(dt=0, tb=ta, ta=ta, Hm=Hm)
  
  for (i in 1:times) {
	  dt = Hm/C
  	  tb <- ta + dt
	  Hm = Hm_ta *(2.5^((tb - ta)/10))
	  dd <- data.frame(dt=dt, tb=tb, ta=ta, Hm=Hm)
	  dat <- rbind(dat, dd)
  }
  return (dat)	

}



ta = 23
smrta = smr*(2.5^((ta-20)/10))

rmr25 = rmr*(2.5^((25-20)/10))
rmr23 = rmr*(2.5^((ta-20)/10))

# Try RMR @ ta:
iterate(Hm=smr, C, ta, times=10)
iterate(Hm=rmr, C, ta, times=10)

iterate(Hm=amr, C, ta=20, times=10)
amr22 = amr *(2.5^((22 - 20)/10))
iterate(Hm=amr22, C, ta=22, times=10)

#Now try RMR@22:
rmr22 = rmr *(2.5^((22 - 20)/10))
iterate(Hm=rmr22, C, ta=22, times=10)

#Now try RMR@12:
rmr12 = rmr *(2.5^((12 - 20)/10))
iterate(Hm=rmr12, C, ta=12, times=10)

#Now try SMR at ta=20:
iterate(Hm=smr, C, ta=20, times=10)

