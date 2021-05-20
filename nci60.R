n = data.frame(read_excel("./RNA__Affy_HG_U133_Plus_2.0_GCRMA.xls",col_names = TRUE))

n = n[,-3:-7]

n = n[,-grep("ME.MDA.N",colnames(n))]

rem = which(apply(n,1,function(x) any(x == NaN | x == -Inf | x == "-") == TRUE))

n = n[-rem,]

n1 = n[,-1]
rownames(n1) = n[,1]

gseMatNum = as.data.frame(apply(n[ ,-1:-2],2,as.numeric))  
rownames(gseMatNum) = n[,1]

gseMatNum[is.null(gseMatNum)]  = 0

## Log2 transformation
if(na.omit(any(gseMatNum >= 100))) {
	cat("log2 transformation---\n") 
	gseMatNum = log2(gseMatNum + 1)
}

splitMat = split(gseMatNum,n1$Gene.name.d) 

expMat = matrix(0,nrow =length(splitMat),ncol = ncol(gseMatNum))
probeNames=geneNames = list()

for(i in 1:length(splitMat)){
	if(nrow(splitMat[[i]]) > 1 ){
		meanExp = apply(splitMat[[i]],2,mean)
		#maxIdx = which(rowSum == max(rowSum))
		expMat[i, ] = as.matrix(meanExp)
		probeNames[[i]] = paste(rownames(splitMat[[i]]),collapse= ",")
		geneNames[[i]] = names(splitMat)[i]
	} else {
		expMat[i, ] = as.matrix(splitMat[[i]][1,])
		probeNames[[i]] = paste(rownames(splitMat[[i]]),collapse= ",")
		geneNames[[i]] = names(splitMat)[i]
	}
}

## Substitute NaNs with 0
expMat[is.nan(expMat)]  = 0
rowZero1 = which(apply(expMat,1,function(x) sum(x)) == 0)

if(length(rowZero1) >= 1 ) expMat = expMat[-rowZero1, ]


expMat[is.na(expMat)]  = 0

expMatNew = cbind(toupper(unlist(geneNames)),unlist(probeNames),expMat)
colnames(expMatNew) = c("Gene","Probe_ID",colnames(gseMatNum))
expMatNew = expMatNew[-1, ]

setwd("./Gene_signatures/76GS")
n2 = EMT76GS(expMatNew,"NCI60",".")
n2 = KSScore(expMatNew,"NCI60",".")
setwd("../..")