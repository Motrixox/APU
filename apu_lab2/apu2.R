library(AHPtools)
library(data.tree)
library(ahp)
setwd("D:/R")
aparaty <- Load("D:/R/aparaty.yml")

print(aparaty, filterFun = isNotLeaf)

Calculate(aparaty)

print(aparaty, priority = function(x) x$parent$priority["Total", x$name])

Visualize(aparaty)

Analyze(aparaty)

AnalyzeTable(aparaty)
