These files produce the Aster analyses from Wadgymar et al., “Integrating viability and fecundity to illuminate the adaptive nature of genetic clines”, Evolution Letters.

The “Aster_analyses_2011_cohort” and “Aster_analyses_2012_cohort” R scripts include the code necessary to run analyses (from the Aster package in R) for estimating cumulative, fecundity, and viability selection for both the 2011 and 2012 cohorts.  The data files associated with these scripts have the same name. Traits and fitness components represent family-level average values (LSMEANS) derived from initial individual level analyses, as described in the methods section.

Description of columns:

elevation - elevation of origin (m) for each maternal family
      
garden - in 2012 cohort file, referring to the low-elevation (Gothic) or high-elevation (Sco) common garden. This column is missing from the 2011 cohort file because the experiment occurred only in the low elevation garden for that cohort.
      
season - growing season for phenotypes and fitness components (2012-2014 for the 2011 cohort and 2013-2014 for the 2012 cohort)
      
geno - unique ID for each individual maternal family 
      
deltac13 - stable Carbon isotopes, a measure of water use efficiency 
      
SLA - specific leaf area, the ratio of leaf area to dry mass (cm2/g)
      
FDsnow - flowering phenology, the number of days from snowmelt to date of first flowering
      
height - height at first flowering, bolt height (cm) on the date of first flowering 
      
root - the number of individuals per maternal family that survived transplant shock and were included in the Aster models 
      
varb - fitness component for a specific maternal family in a given growing season (e.g. the Aster node type signifying either flowering success, fecundity, end of season survival, or overwintering survival for each maternal family in each season, see Supporting Table S7)
      
resp - family-level average fitness data associated with the specific fitness component identified by varb
      
id - unique ID number given to each maternal family 

As explained in Wadgymar et al. and supplemental information, Aster models can not accommodate NAs and require the data for any individual or family where NAs are found to be removed entirely.  For reference, we also provide the full data sets for both the 2011 and 2012 cohort. 

We also provide the WorldClim data from which we calculated the aridity indeces associated with each maternal family’s home site.  These values were included in figures 1, S1, and S2.  

