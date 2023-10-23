m = 6 # tiny fish, 8cm, 6g
smr = 2.5*m^.7

dmr = smr*1.5*24/1000 # 315.4 J/d  or .315 kJ/d
dmr = dmr*2.5^((22-20)/10)  # dmr at 22  .3789 kJ/d

dm=.35
pdm = .5
fdm = .15
cdm = .1

pf = pdm*dm   # 17.5%
ff = fdm*dm
cf = cdm*dm

# fish excrete ammonia
ed = pf*19.67 + ff*39.2 + cf*15.9   # 6.06 kJ/g

food_assim = dmr/ed              # 0.06256 g/day
food_cons = food_assim/.85  # 0.0736 g/day

pg = pf*food_assim # 0.010948
fg = ff*food_assim # 0.0032844
cg = cf*food_assim # 0.0021896

pmol = pg/135  # 8.10963e-05  mol/day
fmol = fg/256  # 1.282969-05
cmol = cg/180  # 1.216444e-05

prate = 86*2.5^((22-20)/10)  # 103.2967 nmol/min/cm^2
prate = prate*10^(-9)*60*24 # .0001487472 mol/day/cm^2

sa = pmol/prate   # .5452 cm^2
d= .45 # 9mm 

l = sa/pi/d  # 0.386 cm  or 3.8mm



