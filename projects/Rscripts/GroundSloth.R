m = 4000*1000

bmr <- (63.9 * m^0.66 )/1000
rmr <- 2*bmr
amrwalk <- (575/400)*rmr
amrgraze <- (680/400)*rmr

dmr <- 16*rmr + 3*amrwalk + 5*amrgraze
Hm <- dmr/24

cc = 20.5*m^-0.426  
tb <- 28.5
ta <- -.5
#ta <- 31.5
dt <- tb-ta
Hc = cc*m*dt/1000

r = 1.371*100
l=6.096*100
SA <- 2*pi*r*l + 2*pi*r^2
Hr <- 0.180 * SA

RH <- .636
xexp <- 30*100
xinsp <- 4.98*RH

LO2 <- dmr/20
Lair <- LO2/.2/.21
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




