m = 110

bmr <- (138 * m^0.72 )/1000
rmr <- 1.5*bmr
amr <- 2.5*bmr
mmr <- 4*bmr

dmr <- mmr + 3*amr + 20*rmr
Hm <- dmr/24

cc = 19*m^-0.583  /1000
tb <- 40
ta <- 16.5

dt <- tb-ta
Hc = cc*m*dt

r = 1.3716*100
l=6.096*100
SA <- 2*pi*r*l + 2*pi*r^2
Hr <- 0.180 * SA

xexp <- 42.8
xinsp <- 8.2

LO2 <- dmr/20
Lair <- LO2/.2/.21
REWL <- Lair*(xexp - xinsp)/1000

xair <- xinsp
xbody <- xexp

resistance <- 1

CEWL <- (xbody - xair)/1000/(exposedSA)/resistance
CEWL <- CEWL/1000 *60*1440

EWL <- REWL + CEWL

He <- EWL*2.48 / 24

Hs <- Hm - Hc + Hr - He
DeltaT <- Hs*1000/m/4

#Fully submerged
Hr <- 0
CEWL <- 0

EWL <- REWL + CEWL

He <- EWL*2.48 / 24

Hs <- Hm - Hc + Hr - He
DeltaT <- Hs*1000/m/4


# With insulation fully submerged

i <- .5

#SA in m2
SA <- 2*pi*r*l /100/100
# i is in C m^2 sec/J
# Hc in kJ/hr
Hc <- (1/i)* SA * dt / 1000 * 60 * 60

Hs <- Hm - Hc + Hr - He
DeltaT <- Hs*1000/m/4




