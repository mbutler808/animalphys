m = 24

bmr <- (80.5 * m^0.73 )/1000
rmr <- 1.54*bmr
amr <- 7.713*bmr
mmr <- 9*bmr

dmr <- 10*rmr + 14*amr
Hm <- rmr

cc = 19*m^-0.583  
tb <- 40
ta <- 10

dt <- tb-ta
Hc = cc*m*dt/1000

SA <- 103.368
Hr <- 0.180 * SA
## Need to factor in hours of sunlight

xexp <- 51.1
xinsp <- 2.82

LO2 <- rmr*24/20
Lair <- LO2/.2/.413
REWL <- Lair*(xexp - xinsp)/1000

xair <- xinsp
xbody <- xexp

resistance <- 225

CEWL <- (xbody - xair)/1000*(SA)/resistance
CEWL <- CEWL/1000 *60*1440

EWL <- REWL + CEWL

He <- EWL*2.48 / 24
Hr <- 0
Hs <- Hm - Hc + Hr - He
DeltaT <- Hs*1000/m/4

#Activity
Hr <- 0
Hm <- amr
LO2 <- Hm*24/20
Lair <- LO2/.2/.413
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


SA = 25.525
Hr <- .180*SA*4/24


