fig2 = function() {
	gs = read.table("NCI60_EMT_76GS.txt",sep = "\t",header = TRUE)
	ks = read.table("NCI60_EMT_KS.txt", sep = "\t", header = TRUE)
	x = data.frame(gs,ks$V2)
	x = plyr::rename(x,c("ks.V2"="ks"))
	x = plyr::rename(x,c("EMTScoreStd"="gs"))
	ggplot(x,aes(x=gs,y=ks)) + geom_point() + 
		stat_smooth(method = "lm", color="red", se=FALSE) + 
		annotate("text",x=80,y=0.6,label=paste("R =",round(cor(x$gs,x$ks),2))) + 
		annotate("text",x=80,y=0.5,label="p<0.05") + 
		xlab("EMT Score (76GS)") + 
		ylab("EMT Score (KS)")	
}
