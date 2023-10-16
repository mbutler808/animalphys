l = 5.5*100   # 5.5 m
r = 50   #0.5m
v = (pi*r^2)*l
sa = 2*pi*r*l+2*pi*r^2
m = 165000
m = 1300*1000
m = 4000*1000
sa = 10.8*100*100  # m2 to cm2

#smr = 1.9*(m)^.82/1000   #Varanus equation
smr = 1.5*(m)^.80/1000   #Varanus equation
rmr = 1.2*smr
amr = 3*smr

C = 2603*m^.148/1000
#C = C*25

Hm=smr
tb=20
ta=36
ta=25.5  # (36+15)/2
Hm = smr *(2.5^((ta - tb)/10))
ta=15
Hm15 = smr *(2.5^((ta - tb)/10))
dmr = 4.5*1.5*Hm15 + 1.83*2*Hm15 + .5*3.38*Hm15 + .17*9.6*Hm15 + 17*2.6*Hm15
sa=
	  
calcHe <- function( Hm, Xair, Xbody, r) {
#	Xair = 11.5   #@50%RH   # if ta=25
#	Xbody = 23  #@100%RH  # if ta=25
	Lair_hr = Hm/(20*.2*.2)
	REWL = Lair_hr*(Xbody-Xair)/1000   # this is g water lost per hour
	CEWL = sa*(Xbody-Xair)/1000/r/1000   # this is g per sec
	CEWL = CEWL*60*60 # this is g per hour
	EWL = REWL+CEWL
	He = EWL*2.4  # this is kJ/hr   
	return(c(He, REWL, CEWL))
}

iterate <- function( Hm, Xair=12.83*.8, Xbody=12.83, r, C, ta, times) {
	
  dat <- data.frame(dt=0, tb=ta, ta=ta, C, Hm=Hm, He=He)
  Hm_ta <- Hm
  
  for (i in 1:times) {
	  He = calcHe(Hm, Xair, Xbody, r)[1]
	  dt = (Hm-He)/C
  	  tb <- ta + dt
	  Hm = Hm_ta *(2.5^((tb - ta)/10))
	  dd <- data.frame(dt=dt, tb=tb, ta=ta, C, Hm=Hm, He)
	  dat <- rbind(dat, dd)
  }
  return (dat)	
}

ta=15
times=10

He = calcHe(Hm15, Xair=12.83*.8, Xbody=12.83, r=200)
iterate(Hm=Hm15, Xair=12.83*.8, Xbody=12.83, r=50, C=C, ta=15, times=15)

iterate(Hm=rmr, Xair=12.5, Xbody=23, 2*C, ta=25, times=15)
iterate(Hm=smr, Xair=11.5, Xbody=23, C, ta=10, times=15)

iterate(Hm=rmr, Xair=15.2, Xbody=30.4, 1.25*C, ta=25, times=10)  #tb=28.6


# Try RMR @ 20C:
iterate(Hm=rmr, He, C, ta=20, times=1)
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

