

/* Read or import the file "Fitness_Lfremontii.xls"*/

data one; set PHS;
fitness=.;
if IfPlantGerm=0 then delete;
if inflorwt="" then fitness = 0; else fitness = inflorwt;
log_inflorwt=log(inflorwt+.1);
log_fitness=log(fitness+.1);
run;
quit ;

**Create a new data file that includes relative fitness;
*** Calculate the grand means by treatment;
proc sort data=one;
by Treatment;
proc means data=one;
var fitness;
output out=fitbytreat mean=xfitbytreat;
by Treatment;
run;
quit;
data relative; set one;
relfit=.;
if Treatment="Nina" then relfit=(fitness)/3.1627846;
if Treatment="Normal" then relfit=(fitness)/8.7562729;
if Treatment="Nino" then relfit=(fitness)/8.7264879;
***Confirm that relative fitness was calculated correctly;
proc sort data=relative;
by Population;
proc means data=relative;
var relfit;
output out=checkrelfit mean=xrelfit stderr=se_relfit;
by Population Treatment;
run;
quit;


************************************************************************************************;
*EFFECTS OF HYDROPERIOD ON RELATIVE FITNESS;
************************************************************************************************;

ods pdf file="ANALYSES_FULLDATASET.pdf";

proc mixed covtest data=relative asycov;
class Population Treatment Sire Dam;
model relfit=Population/ddfm=satterth;
random intercept dam /subject=sire(Population);
random sire(Population);title "RELATIVE FITNESS Model A: mixed model w/PHS structure -variance constrained";
run;
		
**Post-hoc comparisons (results presented in Table S5);
proc glm data=relative;
class Population Treatment;
model relfit = Population|Treatment;
output out=fitness_resids predicted=p_fitness residual=r_fitness;
lsmeans Population Treatment Population*Treatment/pdiff adjust=tukey;
	title "GLM testing for differences among Populations and Treatments in RELATIVE FITNESS";
	run;

**Calculate mean relative fitness for each Population x Hydroperiod combination for Fig. 3B
proc sort data=one;
by Population Treatment;
proc means data=one;
var inflorwt;
output out=xfitness mean=xinflorwt stderr=se_inflorwt;
by Population Treatment;
run;  


************************************************************************************************;
*EFFECTS OF HYDROPERIOD ON ABSOLUTE FITNESS;
************************************************************************************************;

/*NOTE: ALL ANALYSES BELOW WERE WRITTEN TO INCLUDE AN OUTLIER FROM THE NORTHERN POPULATION. To run the models with the outlier removed, first delete “Sire=9” from the dataset, and then modify log-likelihood values and the degrees of freedom as needed for the chi-squared tests used in model comparisons*/

*ABSOLUTE FITNESS = inflor wt of each experimental plant - assigned a value of zero to plants that did not flower;

**MODEL A: Standard variance component model - constrains residual error and variance of random effects to be equal;	
proc mixed covtest data=one asycov;
class Population Treatment sire dam;
model log_fitness=Population|Treatment/ddfm=satterth;
random dam Treatment Population /subject=sire(Population);
title " Model A: mixed model w/PHS structure - variance constrained";
run;

**Post-hoc comparisons – results presented in Table S5;
proc glm data=one;
class Population Treatment;
model log_fitness = Population|Treatment;
output out=abfitness_resids predicted=p_abfitness residual=r_abfitness;
lsmeans Population Treatment Population*Treatment/pdiff adjust=tukey;
title "GLM testing for differences among Populations and Treatments in ABSOLUTE FITNESS";
run;

**Calculate mean absolute fitness for each Population x Hydroperiod combination for Fig. 3A
proc sort data=one;
by Population Treatment;
proc means data=one;
var log_fitness;
output out=xabfitness mean=xabinflorwt stderr=se_abinflorwt;
by Population Treatment;
run; 



************************************************************************************************;
* POPULATION AND ENVIRONMENT-DEPENDENT SHORT-TERM ADAPTIVE POTENTIAL
************************************************************************************************;
**********
*I. Does additive genetic variation in fitness vary among Hydroperiod treatments?

**MODEL B: Allow the residual error to vary among Treatments, but constraining among-sire variance to be equal across Treatments;
proc mixed covtest data=one asycov;
class Population Treatment sire dam;
model log_fitness=Population|Treatment/ddfm=satterth;
random  dam Population Treatment / subject=sire(Population);
repeated/group=Treatment; *allows residual error to vary among Treatments;
title "Model B: residual error allowed to vary among Treatments, among-sire variance constrained to be equal";
run;

**TEST THE HYPOTHESIS that allowing the among-sire variance to differ among Treatments leads to improved model fit: log-likelihood comparison between model A and model B
**model A log-likelihood = 10558.6, # cov params = 4
**model B log-likelihood = 10242.3, # cov params = 6;
data prob;
chiprob = 1 - probchi((10558.6-10242.3),(6-4));
proc print;
title "Model B vs. Model A: Chi-square test for significant differences in residual error among treatments";
run;
quit;

**MODEL C: Allow the residual error AND among-sire variance to differ among Treatments;
proc mixed covtest data=one asycov;
class Population Treatment sire dam;
model log_fitness=Population|Treatment/ddfm=satterth;
random dam Population / subject=sire(Population);		
random Treatment / subject=sire(Population) type=unr; *allows sire variance to vary among Treatments;
repeated/group=Treatment; *allows residual error to vary among Treatments;
title " Model C: residual error AND AMONG-SIRE VARIANCE allowed to vary among Treatments";
run;
**TEST THE HYPOTHESIS that allowing the among-sire variance to differ among Treatments leads to improved model fit over model that allows only the residual variance differ among Treatments:  log-likelihood comparison between model B and model C;
**model B log-likelihood = 10242.3, # cov params = 6;
**model C log-likelihood = 10212.0, # cov params = 9;
data prob;
chiprob = 1 - probchi((10242.3-10212.0),(10-6));
title "Model C vs. Model B: Chi-square test for significant differences in among-sire variance among treatments";
proc print;
run;

**********
*II. Does additive genetic variation in fitness vary among Populations?

**MODEL D: Allow the residual error to differ among Populations, but constraining among-sire variance to be equal across Populations;
proc mixed covtest data=one asycov;
class Population Treatment sire dam;
model log_fitness=Population|Treatment/ddfm=satterth;
random dam Treatment Population / subject = sire(Population);
repeated/group = Population; * allows residual error to vary among Populations;
title "Model D: residual error allowed to vary among Populations";
run;
quit;
**TEST THE HYPOTHESIS that allowing the among-sire variance to vary among Populations leads to improved model fit: log-likelihood comparison between model D and model A;
**model A log-likelihood = 10558.6, # cov params = 4
**model D log-likelihood = 10557.4, # cov params = 6;
data prob;
chiprob = 1 - probchi((10558.6 - 10557.4),(6-4)); 
title "Model D vs. Model A: Chi-square test for significant differences in residual error among populations";
proc print;
run;

**MODEL E: Allow the residual error AND among-sire variance to vary among Populations;
proc mixed covtest data=one asycov;
class Population Treatment sire dam;
model log_fitness=Population|Treatment/ddfm=satterth;
random dam Treatment / subject=sire(Population);		
random Population / subject=sire(Population) type=unr; *allows sire variance to vary among populations;
repeated/group=Population; *allows residual error to vary among populations;
title " Model E: residual error AND AMONG-SIRE VARIANCE allowed to vary among populations";
run;
**TEST THE HYPOTHESIS that allowing the among-sire variance and residual variance to vary among Populations leads to improved model fit over model that only allows the residual variance vary among Populations: log-likelihood comparison between model E and model D;
**model D log-likelihood = 10557.4, # cov params = 6;
**model E log-likelihood = 10551.8, # cov params = 8;
data prob;
chiprob = 1 - probchi((10557.4-10551.8),(8-6));
title "Model E vs. Model D: Chi-square test for significant differences in among-sire variance among populations";
proc print;
run;


**********
*III. Does additive genetic variation in fitness vary among Population x Hydroperiod treatment combinations?

**MODEL F: Allow residual variance to vary with Population x Treatment;
proc mixed covtest data=one asycov;
class Population Treatment sire dam;
model log_fitness=Population|Treatment/ddfm=satterth;
random dam Treatment Population / subject = sire(Population);
repeated/group=Population*Treatment; *allows residual error to vary with Population x Treatment;
title " Model F: RESIDUAL variance allowed to vary with Population x Treatment";
run;
quit;
**TEST THE HYPOTHESIS that allowing the among-sire variance to differ with Population x Treatment leads to improved model fit: log-likelihood comparison between model F and model B;
**model B log-likelihood = 10242.3, # cov params = 6
**model F log-likelihood = 10167, # cov params = 12;
data prob;
chiprob = 1 - probchi((10242.3-10167),(12-6));
title "Model F vs. Model B: Chi-square test for significant differences in residual error with Population x Treatment";
proc print;
run;
**TEST THE HYPOTHESIS that allowing the among-sire variance to differ with Population x Treatment leads to improved model fit: log-likelihood comparison between model F and model D;
**model D log-likelihood = 10557.4, # cov params = 6
**model F log-likelihood = 9917.0, # cov params = 12;
data prob;
chiprob = 1 - probchi((10285.8-9917.0),(12-6));
title "Model F vs. Model D: Chi-square test for significant differences in residual error with Population x Treatment";
proc print;
run;

**MODEL G: Allow among-sire variance AND residual variance to vary with Population x Treatment;
proc mixed covtest data=one asycov;
class Population Treatment sire dam;
model log_fitness=Population|Treatment/ddfm=satterth;
random dam  / subject=sire(Population);		
random Population*Treatment / subject=sire(Population) type=unr; * allows sire variance to vary among Populations AND Treatments;
repeated/group=Population*Treatment; * allows residual error to vary with Population x Treatment;
title "Model G: residual error AND among-sire variance allowed to vary with Population x Treatment";
run;
**TEST THE HYPOTHESIS that allowing the among-sire variance AND residual variance to vary with Population x Treatment leads to improved model fit: log-likelihood comparison between model F and model B;
**model G log-likelihood = 10125.9, # cov params = 28
**model F log-likelihood = 10167, # cov params = 12;
data prob;
chiprob = 1 - probchi((10167-10125.9,),(28-12));
title "Model F vs. Model G: Chi-square test for the among-sire variance AND residual variance to vary with Population x Treatment";
proc print;
run;

ods pdf close; *This closes the pdf file to be saved;


************************************************************************************************;
*ESTIMATING BLUPs - FOR FIG. 2 & Fig. S4;
************************************************************************************************;

ods pdf file="BLUPSmixedmodelG.pdf"; * This allows it to save the output in a pdf;

*Generating BLUPS from model G above by including “solution” in “random” statement;
proc mixed covtest data=one asycov;
class Population Treatment sire dam;
model log_fitness=Population|Treatment/ddfm=satterth;
random dam  / subject=sire(Population);		
random Population*Treatment / subject=sire(Population) solution type=unr; */the "solution" statement generates BLUPS*/;
repeated/group= Population*Treatment; *this statement allows the residual error to differ with Population x Treatment*/;
title "BLUPS derived from Model G: residual error AND among-sire variance allowed to vary with Population x population";
run;

ods pdf close; *This closes the pdf file to be saved;


*****************************************************************************
ADDITIONAL ANALYSES FOUND IN SUPPLEMENTAL INFORMATION;
*****************************************************************************;

*ANCOVA testing if INFLORESCENSE WEIGHT predicts the NUMBER OF VIABLE SEEDS (TABLE S2);
**Import datafile “NumberViableSeeds.InforWeight.xls" and save as =work.SeedVia;


ods graphics on;
ods pdf file="NumberViableSeedsCorrelation.pdf";
proc glm data=SeedVia plot=meanplot(cl);
class Population;
model  Number_of_Viable_Seeds = Population Inflorwt Population*Inflorwt;
lsmeans Population / pdiff;
title "ANCOVA testing if INFLORESCENSE WEIGHT predicts the NUMBER OF VIABLE SEEDS";
run;
ods graphics off;
ods pdf close;














    
