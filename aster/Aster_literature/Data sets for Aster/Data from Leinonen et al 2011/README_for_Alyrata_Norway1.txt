#AlyrataNorway1.zip

#Phenotypic data on local adaptation and hybrid fitness of Arabidopsis lyrata grown in
#a transplant experiment in Norway (Leinonen, Remington, Savolainen 2011 Evolution) 
#if questions, please contact Päivi H. Leinonen (e-mail: paivi.h.leinonen@oulu.fi)

##################################

##################################


#Description of columns in the data file AlyrNorway_SpMaF12.txt
#Note that year 1 refers to the growing season after first winter, not the planting year
#missing data are coded as "NA"
#"summer" refers to a time period from the beginning of June until August and "Winter" time from August until May
#Data collected by the plant genetics group, university of Oulu - Finland



ID: individual ID of the plants in the field based on their randomized position
Block: indicates in which of the eight planting blocks each plant was in
Pop: population of origin, or hybrid class (S=Spiterstlen; N=Mayodan; F1, F2)
CrossPop: population of origin, or hybrid class (S=Spiterstulen; N=Mayodan; Sx=F1 with Sp cytoplasm, Nx=F1 with Ma cytoplasm; SNx=F2 with Sp cytoplasm; NSx=F2 with Ma cytoplasm)
Cytop: indicates the cytoplasmic origin for each plant (S=Sp;N=Ma)
FamA: family of the plants (family of F2 is reciprocal: A=Sp cytoplasm; B=Ma-cytoplasm)
Fam: family of the plants (family of F1 and F2 is reciprocal: A=Sp cytoplasm; B=Ma-cytoplasm)
SurvPla: survival status of each plant a few days after planting (0=dead; 1=alive)
SurvSum0: survival status of each plant at the end of the planting summer (0=dead; 1=alive)
SurvWin1: survival status of each plant after the first winter (0=dead; 1=alive)
SurvSum1: survival status of each plant after the first summer (0=dead; 1=alive)
SurvWin2: survival status of each plant after the second winter  (0=dead; 1=alive)
SurvSum2: survival status of each plant after the second summer  (0=dead; 1=alive)
SurvWin3: survival status of each plant after the third winter  (0=dead; 1=alive)
SurvSum3: survival status of each plant after the third summer  (0=dead; 1=alive)
SurvWin4: survival status of each plant after the fourth winter  (0=dead; 1=alive)
SurvSum4: survival status of each plant after the fourth summer  (0=dead; 1=alive)
Flow1: flowering status of each plant during the first summer (0=vegetative; 1=flowered)
Flow2: flowering status of each plant during the second summer (0=vegetative; 1=flowered)
Flow3: flowering status of each plant during the third summer (0=vegetative; 1=flowered)
Flow4: flowering status of each plant during the fourth summer (0=vegetative; 1=flowered)
TotSh1: total number of inflorescence shoots on each plant at the end of first summer
TotSh2: total number of inflorescence shoots on each plant at the end of second summer
TotSh3 total number of inflorescence shoots on each plant at the end of third summer
FrSh1: average number of fruits per (reproductively mature) inflorescence shoot on each plant at the end of the first summer
FrSh2: average number of fruits per (reproductively mature) inflorescence shoot on each plant at the end of the second summer
FrSh3: average number of fruits per (reproductively mature) inflorescence shoot on each plant at the end of the third summer 
SeFrG1: average number of good-looking (not flat) seeds per fruit on each plant at the end of the first summer
SeFrG2: average number of good-looking (not flat) seeds per fruit on each plant at the end of the second summer
SeFrG3: average number of good-looking (not flat) seeds per fruit on each plant at the end of the third summer
FlStart1: flowering start date in the first summer (Julian days from the first o January each year calculated in Excel)
FlStart2: flowering start date in the second summer (Julian days from the first o January each year calculated in Excel)
FlStart3: flowering start date in the third summer (Julian days from the first o January each year calculated in Excel)
H1Sh1: inflorescence length (cm) of the first flowering inflorescence at the flowering start date
TotFl1: total number of flowers produced in the first summer
PsM12: average petal size (mm2) calculated from width and length of one to three petals from separate flowers


##################################
AlyrNorway_SpMa_Tables1345_lmer.R
##################################

#R scripts for lmer analyses with notes

