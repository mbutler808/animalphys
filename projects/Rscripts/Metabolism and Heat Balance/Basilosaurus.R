m = 10000000
m = 6480000

bmr <- (63.6 * m^0.76 )/1000
rmr <- 1.5*bmr
amrf <- 2.5*rmr
amrt <- 8.6*rmr

dmr <- 5.04*amrf + 16.896*amrt + 2.064*rmr
Hm <- dmr/24

cc = 20.5*m^-0.426 * 25 /1000
tb <- 37
ta <- 18.5
dt <- tb-ta
Hc = cc*m*dt

r=50
l=750
exposedSA <- 2*pi*r*l/3
Hr <- 0.180 * exposedSA

xexp <- 42.8
xinsp <- 8.2

LO2 <- dmr/20
Lair <- LO2/.2/.21
REWL <- Lair*(xexp - xinsp)/1000

xair <- xinsp
xbody <- xexp

resistance <- 1

CEWL <- (xbody - xair)/1000*(exposedSA)/resistance
CEWL <- CEWL/1000 *60*1440

EWL <- REWL + CEWL

He <- REWL*2.48 / 24

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




