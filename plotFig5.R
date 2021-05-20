library(plotly)
library(plyr)

fig5 = function() {
	
	c3_76gs = read.table("./Output/GSE49644/GSE49644_EMT_76GS.txt",sep="\t",header = TRUE)
	c3_76gs$X = sub("(.*)_(.*)_Parental$","\\2",c3_76gs$X)
	c3_76gs$X = sub("(.*)_(.*)_EMT$","\\2 EMT",c3_76gs$X)
	c3_76gs$X = sub("H358","NCI-H358",c3_76gs$X)
	
	dsd1 = ddply(c3_76gs,c("X"),summarise,sd=sd(EMTScoreStd))
	dmean1 = ddply(c3_76gs,c("X"),summarise,mean=mean(EMTScoreStd))
	data1 = data.frame(dmean1,dsd1$sd)
	data1 = plyr::rename(data1,c("dsd1.sd" = "sd"))
	
	
	c3_ks = read.table("./Output/GSE49644/GSE49644_EMT_KS.txt",sep="\t",header = TRUE)
	c3_ks$V1 = sub("(.*)_(.*)_Parental$","\\2",c3_ks$V1)
	c3_ks$V1 = sub("(.*)_(.*)_EMT$","\\2 EMT",c3_ks$V1)
	c3_ks$V1 = sub("H358","NCI-H358",c3_ks$V1)
	
	
	dsd2 = ddply(c3_ks,c("V1"),summarise,sd=sd(V2))
	dmean2 = ddply(c3_ks,c("V1"),summarise,mean=mean(V2))
	data2 = data.frame(dmean2,dsd2$sd)
	data2 = plyr::rename(data2,c("dsd2.sd" = "sd"))
	
	
	mcf7_76gs = read.table("./Output/GSE58252/GSE58252_EMT_76GS.txt",sep="\t",header = TRUE)
	mcf7_76gs[grep("Snail",mcf7_76gs[,"X"]),"X"] = "SNAI1"
	mcf7_76gs[grep("control",mcf7_76gs[,"X"]),"X"] = "Untreated"
	
	dsd3 = ddply(mcf7_76gs,c("X"),summarise,sd=sd(EMTScoreStd))
	dmean3 = ddply(mcf7_76gs,c("X"),summarise,mean=mean(EMTScoreStd))
	data3 = data.frame(dmean3,dsd3$sd)
	data3 = plyr::rename(data3,c("dsd3.sd" = "sd"))
	
	
	mcf7_ks = read.table("./Output/GSE58252/GSE58252_EMT_KS.txt",sep="\t",header = TRUE)
	mcf7_ks[grep("Snail",mcf7_ks[,"V1"]),"V1"] = "SNAI1"
	mcf7_ks[grep("control",mcf7_ks[,"V1"]),"V1"] = "Untreated"
	
	dsd4 = ddply(mcf7_ks,c("V1"),summarise,sd=sd(V2))
	dmean4 = ddply(mcf7_ks,c("V1"),summarise,mean=mean(V2))
	data4 = data.frame(dmean4,dsd4$sd)
	data4 = plyr::rename(data4,c("dsd4.sd" = "sd"))
	
	ep_76gs = read.table("./Output/GSE59922/GSE59922_EMT_76GS.txt",sep="\t",header = TRUE)
	ep_76gs[grep("EMT",ep_76gs[,"X"]),"X"] = "EMT"
	ep_76gs[grep("Control",ep_76gs[,"X"]),"X"] = "Untreated"
	
	dsd5 = ddply(ep_76gs,c("X"),summarise,sd=sd(EMTScoreStd))
	dmean5 = ddply(ep_76gs,c("X"),summarise,mean=mean(EMTScoreStd))
	data5 = data.frame(dmean5,dsd5$sd)
	data5 = plyr::rename(data5,c("dsd5.sd" = "sd"))
	
	
	ep_ks = read.table("./Output/GSE59922/GSE59922_EMT_KS.txt",sep="\t",header = TRUE)
	ep_ks[grep("EMT",ep_ks[,"V1"]),"V1"] = "EMT"
	ep_ks[grep("Control",ep_ks[,"V1"]),"V1"] = "Untreated"
	
	
	dsd6 = ddply(ep_ks,c("V1"),summarise,sd=sd(V2))
	dmean6 = ddply(ep_ks,c("V1"),summarise,mean=mean(V2))
	data6 = data.frame(dmean6,dsd6$sd)
	data6 = plyr::rename(data6,c("dsd6.sd" = "sd"))
	
	
	diff_76gs = read.table("./Output/GSE43495/GSE43495_EMT_76GS.txt",sep="\t",header = TRUE)
	diff_76gs[grep("untreated",diff_76gs[,"X"]),"X"] = "Untreated"
	diff_76gs[grep("empty",diff_76gs[,"X"]),"X"] = "EV"
	diff_76gs[grep("Twist",diff_76gs[,"X"]),"X"] = "TWIST"
	diff_76gs[grep("Snail",diff_76gs[,"X"]),"X"] = "SNAI1"
	diff_76gs[grep("Slug",diff_76gs[,"X"]),"X"] = "SNAI2"
	
	
	dsd7 = ddply(diff_76gs,c("X"),summarise,sd=sd(EMTScoreStd))
	dmean7 = ddply(diff_76gs,c("X"),summarise,mean=mean(EMTScoreStd))
	data7 = data.frame(dmean7,dsd7$sd)
	data7 = plyr::rename(data7,c("dsd7.sd" = "sd"))
	
	
	diff_ks = read.table("./Output/GSE43495/GSE43495_EMT_KS.txt",sep="\t",header = TRUE)
	diff_ks[grep("untreated",diff_ks[,"V1"]),"V1"] = "Untreated"
	diff_ks[grep("empty",diff_ks[,"V1"]),"V1"] = "EV"
	diff_ks[grep("Twist",diff_ks[,"V1"]),"V1"] = "TWIST"
	diff_ks[grep("Snail",diff_ks[,"V1"]),"V1"] = "SNAI1"
	diff_ks[grep("Slug",diff_ks[,"V1"]),"V1"] = "SNAI2"
	
	
	dsd8 = ddply(diff_ks,c("V1"),summarise,sd=sd(V2))
	dmean8 = ddply(diff_ks,c("V1"),summarise,mean=mean(V2))
	data8 = data.frame(dmean8,dsd8$sd)
	data8 = plyr::rename(data8,c("dsd8.sd" = "sd"))
	
	gr_76gs = read.table("./Output/GSE36081/GSE36081_EMT_76GS.txt",sep="\t",header = TRUE)
	gr_76gs[grep("x7",gr_76gs[,"X"]),"X"] = "Untreated"
	gr_76gs[grep("x8",gr_76gs[,"X"]),"X"] = "GRHL2"
	
	dsd9 = ddply(gr_76gs,c("X"),summarise,sd=sd(EMTScoreStd))
	dmean9 = ddply(gr_76gs,c("X"),summarise,mean=mean(EMTScoreStd))
	data9 = data.frame(dmean9,dsd9$sd)
	data9 = plyr::rename(data9,c("dsd9.sd" = "sd"))
	
	
	gr_ks = read.table("./Output/GSE36081/GSE36081_EMT_KS.txt",sep="\t",header = TRUE)
	gr_ks[grep("x7",gr_ks[,"V1"]),"V1"] = "Untreated"
	gr_ks[grep("x8",gr_ks[,"V1"]),"V1"] = "GRHL2"
	
	
	dsd10 = ddply(gr_ks,c("V1"),summarise,sd=sd(V2))
	dmean10 = ddply(gr_ks,c("V1"),summarise,mean=mean(V2))
	data10 = data.frame(dmean10,dsd10$sd)
	data10 = plyr::rename(data10,c("dsd10.sd" = "sd"))
	
	hc_76gs = read.table("./Output/GSE26391/GSE26391_EMT_76GS.txt",sep="\t",header = TRUE)
	hc_76gs[grep("3p",hc_76gs[,"X"]),"X"] = "3p"
	hc_76gs[grep("3sp",hc_76gs[,"X"]),"X"] = "3sp"
	
	dsd11 = ddply(hc_76gs,c("X"),summarise,sd=sd(EMTScoreStd))
	dmean11 = ddply(hc_76gs,c("X"),summarise,mean=mean(EMTScoreStd))
	data11 = data.frame(dmean11,dsd11$sd)
	data11 = plyr::rename(data11,c("dsd11.sd" = "sd"))
	
	
	hc_ks = read.table("./Output/GSE26391/GSE26391_EMT_KS.txt",sep="\t",header = TRUE)
	hc_ks[grep("3p",hc_ks[,"V1"]),"V1"] = "3p"
	hc_ks[grep("3sp",hc_ks[,"V1"]),"V1"] = "3sp"
	
	
	dsd12 = ddply(hc_ks,c("V1"),summarise,sd=sd(V2))
	dmean12 = ddply(hc_ks,c("V1"),summarise,mean=mean(V2))
	data12 = data.frame(dmean12,dsd12$sd)
	data12 = plyr::rename(data12,c("dsd12.sd" = "sd"))
	
	# fig1 = plot_ly(data=data1,x=~X, y=~mean, type = "bar", error_y = ~list(array=sd, color="#000000"), color=~X,  showlegend=FALSE)
	# fig1 = fig1 %>% layout(title="76GS", xaxis= list(title="") , yaxis= list(title=""))
	# 
	# fig2 = plot_ly(data=data2,x=~V1, y=~mean, type = "bar", error_y = ~list(array=sd, color="#000000"), color=~V1,  showlegend=FALSE)
	# fig2 = fig2 %>% layout(title="KS", xaxis= list(title="") , yaxis= list(title="",side="right"))
	# 
	# fig = subplot(fig1,fig2)
	# fig = fig %>% layout( width=400, margin=list(t=100), title="MCF7", annotations = list(
	# 										list(x = 0.2 , y = 1.05, text = "76GS", showarrow = F, xref='paper', yref='paper'),
	# 										list(x = 0.8 , y = 1.05, text = "KS", showarrow = F, xref='paper', yref='paper')
	# 										))
	# fig
	
	p1 = ggplot(data1) + geom_bar(aes(x=X,y=mean,fill=X), show.legend = FALSE, stat = "identity") + geom_errorbar(aes(x=X,ymin=mean-sd, ymax = mean+sd, width=.25)) + labs(title = "76GS",x="",y="EMT Score") + theme(axis.text.x = element_text(angle = 90, hjust=1))
	p2 = ggplot(data2) + geom_bar(aes(x=V1,y=mean,fill=V1), show.legend = FALSE, stat = "identity") + geom_errorbar(aes(x=V1,ymin=mean-sd, ymax = mean+sd, width=.25)) + labs(title = "KS",x="",y="") + theme(axis.text.x = element_text(angle = 90, hjust=1))
	p3 = ggplot(data3) + geom_bar(aes(x=X,y=mean,fill=X), show.legend = FALSE, stat = "identity") + geom_errorbar(aes(x=X,ymin=mean-sd, ymax = mean+sd, width=.25)) + labs(title = "76GS",x="",y="EMT Score") + theme(axis.text.x = element_text(angle = 90, hjust=1))
	p4 = ggplot(data4) + geom_bar(aes(x=V1,y=mean,fill=V1), show.legend = FALSE, stat = "identity") + geom_errorbar(aes(x=V1,ymin=mean-sd, ymax = mean+sd, width=.25)) + labs(title = "KS",x="",y="") + theme(axis.text.x = element_text(angle = 90, hjust=1))
	p5 = ggplot(data5) + geom_bar(aes(x=X,y=mean,fill=X), show.legend = FALSE, stat = "identity") + geom_errorbar(aes(x=X,ymin=mean-sd, ymax = mean+sd, width=.25)) + labs(title = "76GS",x="",y="EMT Score") + theme(axis.text.x = element_text(angle = 90, hjust=1))
	p6 = ggplot(data6) + geom_bar(aes(x=V1,y=mean,fill=V1), show.legend = FALSE, stat = "identity") + geom_errorbar(aes(x=V1,ymin=mean-sd, ymax = mean+sd, width=.25)) + labs(title = "KS",x="",y="") + theme(axis.text.x = element_text(angle = 90, hjust=1))
	p7 = ggplot(data7) + geom_bar(aes(x=X,y=mean,fill=X), show.legend = FALSE, stat = "identity") + geom_errorbar(aes(x=X,ymin=mean-sd, ymax = mean+sd, width=.25)) + labs(title = "76GS",x="",y="EMT Score") + theme(axis.text.x = element_text(angle = 90, hjust=1))
	p8 = ggplot(data8) + geom_bar(aes(x=V1,y=mean,fill=V1), show.legend = FALSE, stat = "identity") + geom_errorbar(aes(x=V1,ymin=mean-sd, ymax = mean+sd, width=.25)) + labs(title = "KS",x="",y="") + theme(axis.text.x = element_text(angle = 90, hjust=1))
	p9 = ggplot(data9) + geom_bar(aes(x=X,y=mean,fill=X), show.legend = FALSE, stat = "identity") + geom_errorbar(aes(x=X,ymin=mean-sd, ymax = mean+sd, width=.25)) + labs(title = "76GS",x="",y="EMT Score") + theme(axis.text.x = element_text(angle = 90, hjust=1))
	p10 = ggplot(data10) + geom_bar(aes(x=V1,y=mean,fill=V1), show.legend = FALSE, stat = "identity") + geom_errorbar(aes(x=V1,ymin=mean-sd, ymax = mean+sd, width=.25)) + labs(title = "KS",x="",y="") + theme(axis.text.x = element_text(angle = 90, hjust=1))
	p11 = ggplot(data11) + geom_bar(aes(x=X,y=mean,fill=X), show.legend = FALSE, stat = "identity") + geom_errorbar(aes(x=X,ymin=mean-sd, ymax = mean+sd, width=.25)) + labs(title = "76GS",x="",y="EMT Score") + theme(axis.text.x = element_text(angle = 90, hjust=1))
	p12 = ggplot(data12) + geom_bar(aes(x=V1,y=mean,fill=V1), show.legend = FALSE, stat = "identity") + geom_errorbar(aes(x=V1,ymin=mean-sd, ymax = mean+sd, width=.25)) + labs(title = "KS",x="",y="") + theme(axis.text.x = element_text(angle = 90, hjust=1))
	
	
	for (i in c("SNAI1","SNAI2","TWIST")) {
		s = ""
		t = t.test(subset(diff_76gs,X=="Untreated")$EMTScoreStd,subset(diff_76gs,X==i)$EMTScoreStd, paired = TRUE)
		if(t$p.value < 0.05) {
			s = paste(s,"a",sep = "")
		}
		t = t.test(subset(diff_76gs,X=="EV")$EMTScoreStd,subset(diff_76gs,X==i)$EMTScoreStd, paired = TRUE)	
		if(t$p.value < 0.05) {
			s = paste(s,"b",sep = "")	
		}
		if(s != "") {
			p7 = p7 + annotate("text",x=i,y=20,size=3,label=s,parse=TRUE)
			p8 = p8 + annotate("text",x=i,y=0.7,size=3,label=s,parse=TRUE)
		}
	}
	
		
	p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + plot_layout(nrow = 2)
}

