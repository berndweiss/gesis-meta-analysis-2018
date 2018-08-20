
source("run_me_first.R")

## Please insert your solution here. Of course, feel free to add new code cells.

## Ave Caesar, morituri te salutant.

## Please insert your solution here. Of course, feel free to add new code cells.

## Remove the ## in the next line to execute the help command. 
## ?dat.bcg 
trial	numeric	trial number
author	character	author(s)
year	numeric	publication year
tpos	numeric	number of TB positive cases in the treated (vaccinated) group
tneg	numeric	number of TB negative cases in the treated (vaccinated) group
cpos	numeric	number of TB positive cases in the control (non-vaccinated) group
cneg	numeric	number of TB negative cases in the control (non-vaccinated) group
ablat	numeric	absolute latitude of the study location (in degrees)
alloc	character	method of treatment allocation (random, alternate, or systematic assignment)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)

## Please insert your solution here. Of course, feel free to add new code cells.

head(dat.bcg)

## Please insert your solution here. Of course, feel free to add new code cells.

## Please insert your solution here. Of course, feel free to add new code cells.

## Please insert your solution here. Of course, feel free to add new code cells.

## Please insert your solution here. Of course, feel free to add new code cells.

## Please insert your solution here. Of course, feel free to add new code cells.

## Please insert your solution here. Of course, feel free to add new code cells.

## Please insert your solution here. Of course, feel free to add new code cells.
