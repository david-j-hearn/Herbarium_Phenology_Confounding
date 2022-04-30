Start with the occurrence.csv file from IDigBio
	run parse_iDigBio_Occurrences.pl on the occurrence.csv file
	If there are many species, put the info into parseAllSpecies.sh, and run that script file
Run the R function on the parsed occurrence.csv file, e.g.: 
	runAnalysis("c:/Users/David/OneDrive - Towson University/Projects/HerbariumConfounding/Spring21Analysis/SpecimenFiles/AnemonoidesQuinquefolia/AnemoneQuinquifolia_All_Geo/occurrence.Parsed.csv")
On the output of the runAnalysis function, run the extract_pValue.pl script:
	extract_pValue.pl ../InfoFiles/extractPValuesInfo.txt > summary
	Move the summary file into the folder that runs the analysis on the p-values:
		mv summaryResults.txt /cygdrive/c/Users/David/OneDrive\ -\ Towson\ University/Projects/HerbariumConfounding/Spring21Analysis/Paper/Figures/Figure_ResultsSummary/	

