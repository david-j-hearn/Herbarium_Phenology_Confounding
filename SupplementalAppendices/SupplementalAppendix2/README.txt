The two R scripts, NormalizeData.txt.R and removeOutliers.txt.R, provide two functions, normalizeData and 
removeOutliers, respectively. 

Both functions take as input a dataframe object with (minimally) the fields 'dayOfYear' and 'latitude'. 

Such a dataframe could be created by reading the table in the output file provided by parse_iDigBio_Occurrences.pl (See Supplemental Appendix 1). 

The function normalizeData returns a dataframe with dayOfYear normalized using the Box-Cox transform. 
The function removeOutliers removes doyOfYear and latitude outliers using the interquartile range (IQR) criterion.

To use these scripts, 'source' them in R. 
