./extract_pValue.pl ../InfoFiles/extractPValuesInfo.Empirical.txt > summaryResults.Empirical.txt
./extract_significantPercent.pl ../InfoFiles/extractPValuesInfo.simulatedUncorrelated.txt > summaryResults.simulatedUncorrelated.txt
./extract_significantPercent.pl ../InfoFiles/extractPValuesInfo.simulatedCorrelated.txt > summaryResults.simulatedCorrelated.txt
mv summaryResults*.txt ..
