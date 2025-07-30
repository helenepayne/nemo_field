
#AlyrataNorway2.zip

#Phenotypic data on Arabidopsis lyrata grown in
#a transplant experiment in Norway (Leinonen, Remington, Savolainen 2011 Evolution) 

##################################
AlyrNorway_SpMaF12Aster.txt
##################################

#data file suitable for aster life history models in R; based on the data file AlyrNorway_SpMaF12.txt

#Description of columns
#Note that year 1 refers to the growing season after first winter, not the planting year
#individuals with missing data were excluded
#"summer" refers to a time period from the beginning of June until August and "Winter" time from August until May
#Data collected by the plant genetics group, university of Oulu - Finland
#if questions, please contact Päivi H. Leinonen (e-mail: paivi.h.leinonen@oulu.fi)


ID: individual ID of the plants in the field based on their randomized position
Block: indicates in which of the eight planting blocks each plant was in
Pop: Population (A=Spiterstulen; B=Mayodan; C=F1; D=F2)
CrossPop: population of origin, or hybrid class (A=Spiterstulen; B=Mayodan; CA=F1 with Sp cytoplasm, CB=F1 with Ma cytoplasm; DA=F2 with Sp cytoplasm; DB=F2 with Ma cytoplasm)
SurvWin1: survival status of each plant after the first winter (0=dead; 1=alive)
ReprS1: reproductive status of each plant during the first summer (0=did not produce seed; 1= did produce seed)
TotFr1: total number of fruits per plant in the first summer
Seed1: total seed number in the sampled fruits in the first summer
Samp1: number of sampled fruits in the first summer
FruitSeed1: ("Seed1"/"Samp1")*"TotFr1"
FruitSeedcr1: Cubic root of "FruitSeed1
FruitSeedR1: "Fruitseed1" rounded to the nearest integer
SurvWin2: survival status of each plant after the second winter (0=dead; 1=alive)
ReprS2: flowering status of each plant during the second summer (0=did not produce seed; 1= did produce seed)
TotFr2: total number of fruits per plant in the second summer
Seed2: total seed number in the sampled fruits in the second summer
Samp2: number of sampled fruits in the second summer
FruitSeed2: ("Seed2"/"Samp2")*"TotFr2"
FruitSeedcr2: Cubic root of "FruitSeed2
FruitSeedR2: "Fruitseed2" rounded to the nearest integer
SurvWin3: survival status of each plant after the third winter (0=dead; 1=alive)
ReprS3: flowering status of each plant during the third summer (0=did not produce seed; 1= did produce seed)
TotFr3: total number of fruits per plant in the third summer
Seed3: total seed number in the sampled fruits in the third summer
Samp3: number of sampled fruits in the third summer
FruitSeed3: ("Seed3"/"Samp3")*"TotFr3"
FruitSeedcr3: Cubic root of "FruitSeed3
FruitSeedR3: "Fruitseed2" rounded to the nearest integer

##################################
AlyrNorway_SpMa_Table2_aster.R
##################################

#R scripts for aster life-history analyses with notes

