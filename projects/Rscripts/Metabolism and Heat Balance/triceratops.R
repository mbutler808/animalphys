m = 8845053 

#bmr <- (80.5 * m^0.73 )/1000
# Rattites
bmr <- (50.4 * m^0.73 )/1000
rmr <- 1.5*bmr
amr <- 2*bmr
mmr <- 10*bmr

dmr <- 10*rmr + 10*amr + 4*mmr
Hm <- rmr

cc = 20.5*m^-0.426 
tb <- 41
ta <- 27

dt <- tb-ta
Hc = cc*m*dt/1000

r=137
l=914
SA <- 2*pi*r*l
Hr <- 0.180 * SA
## Need to factor in hours of sunlight

xexp <- 53
xinsp <- 25*.5

LO2 <- rmr*24/20
Lair <- LO2/.2/.23
REWL <- Lair*(xexp - xinsp)/1000

xair <- xinsp
xbody <- xexp

resistance <- 158

CEWL <- (xbody - xair)/1000*(SA)/resistance
CEWL <- CEWL/1000 *60*1440

EWL <- REWL + CEWL

He <- EWL*2.48 / 24
Hr <- 0
Hs <- Hm - Hc + Hr - He
DeltaT <- Hs*1000/m/4


#Activity
ta <- 32
xinsp <- 33*.8
xair <- xinsp
xbody <- xexp

Hr <- .180*SA
Hm <- mmr

dt <- tb-ta
Hc = cc*m*dt/1000

LO2 <- Hm*24/20
Lair <- LO2/.2/.23
REWL <- Lair*(xexp - xinsp)/1000

xair <- xinsp
xbody <- xexp

resistance <- 158

CEWL <- (xbody - xair)/1000*(SA)/resistance
CEWL <- CEWL/1000 *60*1440

EWL <- REWL + CEWL

He <- EWL*2.48 / 24

Hs <- Hm - Hc + Hr - He
DeltaT <- Hs*1000/m/4


