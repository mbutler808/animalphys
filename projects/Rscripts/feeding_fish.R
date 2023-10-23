m = 6 # tiny fish

smr = 2.5*m^.7
dmr = smr*1.5*24  # 315.5 J/day or .3155 kJ/day

dm = .35
pdm = .5
fdm = .15
cdm = .1

pf = pdm*dm
ff = fdm*dm
cf = cdm*dm

# fish excrete ammonia
ed = pf*19.67 + ff*39.2 + cf*15.9 