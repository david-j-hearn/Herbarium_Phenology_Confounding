


source("MosaicGraph.txt.R")

#boxes.col = c("black", "magenta", "orange", "olivedrab1", "khaki1")
boxes.col = c(rep("black", 5), "yellow")
boxes = c(1,2,3,3,2,3)

files.empirical = c("summaryResults.Empirical.AA.txt", "summaryResults.Empirical.DMC.txt", "summaryResults.Empirical.WD.txt")
files.uncorrelated = c("summaryResults.simulatedUncorrelated.AA.txt", "summaryResults.simulatedUncorrelated.DMC.txt", "summaryResults.simulatedUncorrelated.WD.txt")
files.correlated = c("summaryResults.simulatedCorrelated.AA.txt", "summaryResults.simulatedCorrelated.DMC.txt", "summaryResults.simulatedCorrelated.WD.txt")

makeMosaicGraph.txt(files.empirical, headerH=115, nameW=80, itemW=10, out="resultsSummary.Empirical.ave.pdf", boxes = boxes, boxes.col = boxes.col, percentage=FALSE)
makeMosaicGraph.txt(files.uncorrelated, headerH=115, nameW=80, itemW=10, out="resultsSummary.simulatedUncorrelated.ave.pdf", boxes = boxes , boxes.col = boxes.col,percentage=TRUE)
makeMosaicGraph.txt(files.correlated, headerH=115, nameW=80, itemW=10, out="resultsSummary.simulatedCorrelated.ave.pdf", boxes = boxes , boxes.col = boxes.col ,percentage=TRUE)

