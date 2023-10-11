# mass

m = 4019.2

# metabolic model

bmr <- (63.6 * m^0.76 )/1000
rmr <- 1.2*bmr
amr2 <- 3.38*bmr
amr5 <- 9.67*bmr
mmr <- 10*bmr

dmr <- 19*rmr + 5*amr2 + 1*amr5

Hm <- dmr/24

# Hc 

cc = 20.5*m^-0.426  
tb <- 37
ta <- 20

dt <- tb-ta
Hc = cc*m*dt/1000

# Hr

r = 5
l=50
SA <- 2*pi*r*l 
Hr <- 0.180 * SA
## Need to factor in hours of sunlight

# He

xexp <- 47.97
xinsp <- 8.6

LO2 <- dmr/20
Lair <- LO2/.2/.23
REWL <- Lair*(xexp - xinsp)/1000

xair <- xinsp
xbody <- xexp

resistance <- 150
SA=2111.15

CEWL <- (xbody - xair)/1000*(SA)/resistance
CEWL <- CEWL/1000 *60*1440

EWL <- REWL + CEWL

He <- EWL*2.48 / 24

# Heat Balance 

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


