m = 250*1000 # tiny fish, 8cm, 6g

dmr = 37982.483
dm=1
pdm = .226
fdm = .0336
cdm = 0

pf = pdm*dm   # 17.5%
ff = fdm*dm
cf = cdm*dm

# cats excrete urea
ed = pf*20.1 + ff*39.2 + cf*15.9   # 6.06 kJ/g

food_assim = dmr/ed              # 0.06256 g/day
food_cons = food_assim/.857  # 0.0736 g/day

pg = pf*food_assim # 0.010948
fg = ff*food_assim # 0.0032844
cg = cf*food_assim # 0.0021896

pmol = pg/135  # 8.10963e-05  mol/day
fmol = fg/256  # 1.282969-05
cmol = cg/180  # 1.216444e-05

prate = 400*2.5^((37.6-37)/10)  # 103.2967 nmol/min/cm^2
prate = prate*10^(-9)*60*24 # .0001487472 mol/day/cm^2

sa = pmol/prate   # .5452 cm^2
d= 3 # 9mm 

l = sa/pi/d  # 0.386 cm  or 3.8mm



