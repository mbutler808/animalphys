## REWL for an 8g Anolis sagrei at SMR in Honolulu

## Air breathed per hour

m = 8 # mass of Anolis sagrei 8g 
SMR = 1.5*m^0.8  # interspecific scaling equation for all lizards Withers 1992, m in g, SMR in J/hr
SMR = SMR/1000 # to convert J/hr to kJ/hr

LO2 = SMR / 20 # animals produce 20kJ energy/LO2. 

Lair = LO2 /(.21 * .2)  # air is about 21% O2 (vol/vol), lizards have extraction efficiency of about 20%

## Environmental parameters

	Ta = 26 # average Oct temp in Honolulu in C
	RHa = .75 # relative humidity in Honolulu
	Tb = 20 # SMR temperature (is this a good assumption?)
	RHb = 1  # relative humidity for expired air

	X20 = 17.3 # water vapor density for 20C at 100%RH in mg/L  
	X26 = 24.48 # WVD for 26C at 100% RH in mg/L. 
			  # It is 23mg/L for 25C (Nave, 2023) 
			  # for 30.4mg/L for 30C 

	# for expired air, chi is at Tb and RHb (20C, 100%RH) in mg/L
	Xinsp = X26 * RHa  # 18.24 mg/L
	Xexp = X20         # 17.3 mg/L

## Water lost via respiration

REWL = Lair*(Xexp - Xinsp)  # REWL is negative, water condensing on anole! 

##########################################
### Retry at Tb at 26C, Ta remains at 26C:

Tb = 26
SMR26 = SMR * (2.5)^((Tb-20)/10)   #Q10 from 20 to 26C

# Xinsp is the same
Xexp = X26 # expiring air at 26C, 100% RH

# Recalculate Lair
LO2 = SMR26 / 20 # animals produce 20kJ energy/LO2. 
Lair = LO2 /(.21 * .2)  # air is about 21% O2 (vol/vol), lizards have extraction efficiency of about 20%

# Recalculate REWL
REWL = Lair*(Xexp - Xinsp)  # now lizard is losing 0.0573 mg/hr of water


