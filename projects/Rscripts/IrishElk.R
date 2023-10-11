m = 544.3*1000

bmr <- (63.6 * m^0.76 )/1000
rmr <- 2*bmr
amr <- 4*bmr
mmr <- 5*bmr

dmr <- 19*rmr + 4*amr + 1*mmr
Hm <- dmr/24

cc = 20.5*m^-0.426  
tb <- 37.6
ta <- 5

dt <- tb-ta
Hc = cc*m*dt/1000

r = 58.25
l=210
SA <- 2*pi*r*l 
Hr <- 0.180 * SA
## Need to factor in hours of sunlight

xexp <- 44
xinsp <- 3.4

LO2 <- dmr/20
Lair <- LO2/.2/.25
REWL <- Lair*(xexp - xinsp)/1000

xair <- xinsp
xbody <- xexp

resistance <- 200

CEWL <- (xbody - xair)/1000*(SA)/resistance
CEWL <- CEWL/1000 *60*1440

EWL <- REWL + CEWL

He <- EWL*2.48 / 24

Hs <- Hm - Hc + Hr - He
DeltaT <- Hs*1000/m/4

#Cold Stress
Hr <- 0
ta <- -10
dt <- tb-ta
Hc = cc*m*dt/1000

xexp <- 44
xinsp <- 1

LO2 <- dmr/20
Lair <- LO2/.2/.25
REWL <- Lair*(xexp - xinsp)/1000

xair <- xinsp
xbody <- xexp

resistance <- 200

CEWL <- (xbody - xair)/1000*(SA)/resistance
CEWL <- CEWL/1000 *60*1440

EWL <- REWL + CEWL

He <- EWL*2.48 / 24

EWL <- REWL + CEWL

He <- EWL*2.48 / 24

Hs <- Hm - Hc + Hr - He
DeltaT <- Hs*1000/m/4


