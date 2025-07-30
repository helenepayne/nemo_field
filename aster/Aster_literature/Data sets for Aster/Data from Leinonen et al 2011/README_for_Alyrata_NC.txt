
#AlyrataNC.zip

#Phenotypic data and statistical analyses on R on local adaptation and hybrid fitness of Arabidopsis lyrata grown in
#a transplant experiment in North Carolina USA (Leinonen, Remington, Savolainen 2011 Evolution) 
#if questions, please contact David L. Reminton (e-mail: dlreming@uncg.edu)

##################################
Alyrata1105Data090808CombR.txt
##################################

#Phenotypic field data
#missing data are coded as "NA" 
#Data file for linear/mixed model and aster analyses for North Carolina field site
#Data collected by David L. Remington from UNCG and many students 

#Field names:
Pop			Population:  I=Ithaca, N=North Carolina (Mayodan), P=Plech, S=Spiterstulen, F1, F2.
FamFixed		Same as population, but F1 and F2 are separated by reciprocal.  For F1, SN=Spiterstulen cytoplasm, NS=Mayodan cytoplasm.  For F2, A=Spiterstulen cytoplasm, B=Mayodan cytoplasm.
Fam			For Pop=I,N,P,S, this is the full-sib family from within the population.  For Pop=F1,F2, this is the cytoplasm, same as FamFixed.
Seed			Consecutive seedling number within Fam.
Rep			Replication or Block in which the plant was planted in the NC field study.
Row			Row coordinate for plant in NC field site.
Col			Column coordinate for plant in NC field site.  These are A-Z,AA-AT in order.  Each Rep had four columns, then two columns were skipped before starting the adjacent Rep.  Thus, there are no plants in Col E-F, K-L, etc.
Surv306			Recorded as 1 if plant was alive for March 2006 measurement, 0 if dead.
BoltDate		Julian date at which bolting (visible elongation of assumed reproductive shoot) was first noted.  (NOTE:  The apparently bolting shoots never did actually flower on some plants.)
FlowerDate		Julian date at which first open flower was noted.
Diam1_306		Pre-reproductive rosette diameter along largest axis at March 2006 measurement, in mm.
Diam2_306		Pre-reproductive rosette diameter along second largest axis at March 2006 measurement, in mm.
Area_306		Rosette “area” = Diam1_306*Diam2_306; NOT the true elliptical area, which would be calculated as Area_306 * pi/4.
ShootHt306		The height above ground of the first flower to open.
S06TotalShoots		The total number of reproductive shoots (flowering shoots, or inflorescences) produced over the main 2006 reproductive season (March to late June).  This was left blank if no reproductive shoots were produced.
S06TotalShootsZ		Same as S06TotalShoots, except that 0 was recorded for live plants at the end of the reproductive season that produced no reproductive shoots (i.e. plants for which S06Flowered = 0).
S06Flowered		Recorded as 1 if plant flowered, and as 0 if it did not.  Recorded only for plants still alive in June 2006 unless they had recently died.
S06SampledShoots	The number of representative reproductive shoots that were sampled to count siliques.
AfterColl(1/0)		Recorded as 1 if siliques were counted after the sample was collected to determine seed mass and seeds/silique, and otherwise a 0.
S06AdjSiliques		If AfterColl=1, this adjusts S06TotalSiliques to account for the expected number of siliques that were collected from the sampled shoots, and converts to an integer.  Specifically, a correction S06CollSiliques*S06SampledShoots/S06TotalShoots is added to the value of S06TotalSiliques.
S06TotalSiliques	The total number of siliques counted from the sampled reproductive shoots.
S06CollSiliques		The total number of siliques collected from the plant to estimate seeds per silique and seed mass.
S06SilPerShoot		The estimated mean number of siliques per shoot, calculated as S06AdjSiliques/S06SampledShoots.
S06Seeds		The total number of seeds counted from the collected siliques.
S06SeedPerSil		The estimated mean number of seeds per silique, calculated as S06Seeds/S06CollSiliques.
S06SeedWt		The total mass of the seeds obtained from the collected siliques, in mg.
S06WtPer100		The estimated mean mass of 100 seeds, calculated as 100*S06SeedWt/S06Seeds.
ReprodOutput		The estimated total reproductive output of the plant, calculated as S06TotalShoots*S06SilPerShoot*S06SeedPerSil.  This was left blank if any of the terms were missing.
ReprodOutputZ		Same as ReprodOutput, except that 0 was recorded for live plants at the end of the reproductive season that produced no reproductive shoots (i.e. plants for which S06Flowered = 0).
CollDate		The Julian date on which the data on the number of reproductive shoots, the number of siliques per shoot, and June rosette diameter were collected.
Surv606			Recorded as 1 if the plant was alive on CollDate, and 0 otherwise.
Shoots606		Recorded as 1 if the plant had newly-developing reproductive shoots (not included in S06TotalShoots) on CollDate, and 0 otherwise.
Flowering606		Recorded as 1 if the plant was flowering as of CollDate, and 0 otherwise.
Diam1_606		Post-reproductive rosette diameter along largest axis at June 2006 measurement, in mm (CollDate).
Diam2_606		Post-reproductive rosette diameter along second largest axis at June 2006 measurement, in mm (CollDate).
Area_606		Rosette “area” = Diam1_606*Diam2_606; NOT the true elliptical area, which would be calculated as Area_606 * pi/4.
dDiam1			Net reproductive season change (in mm) in rosette diameter along largest axis, calculated as Diam1_606-Diam1_306.
dDiam2			Net reproductive season change in rosette diameter along second largest axis, calculated as Diam2_606-Diam2_306.
dArea			Net reproductive season change in rosette area, calculated as Area_606-Area_306; NOT the change in true elliptical area, which would be calculated as dArea * pi/4 
Surv1106		Recorded as 1 if the plant could be identified and was alive as of late November/early December 2006, and 0 otherwise.
Diam1_1206		Late November/early December2006 rosette diameter along largest axis on live plants, in mm.
Diam2_1206		Late November/early December2006 rosette diameter along second largest axis on live plants, in mm.
Final			A unique consecutive number to identify each plant.


##################################
AlyScriptsEvolPaper.R
##################################

#R scripts for lmer analyses with notes

##################################
AsterScriptsEvolPaper.R
##################################

#R scripts for aster life-history analyses with notes

