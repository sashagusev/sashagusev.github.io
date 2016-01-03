library('vioplot')
tbl = read.table("run_rel_typed.out",as.is=T)
svg("SKK_twins_typed.svg",width=9,height=4)
plot(0,0,type="n",xlab="# twins (out of 1,000 individuals)",ylab="h2 estimate",xlim=c(0,50),ylim=c(0,1),main="Estimated heritability with relatedness (no untyped SNPs)")
abline(h=0.5)

for ( i  in 0:50 ) { 
keep = tbl[,2] == i
vioplot( tbl[keep,3] , at=i , add=T , col="#CCCCCC", border="#AAAAAA",drawRect=F)
mu = mean(tbl[keep,3])
se = mean(tbl[keep,4])
sd = sd(tbl[keep,3])
points( i , mu , pch=19, , col="black")
arrows( i , mu - sd , i , mu + sd , angle=0)
points( c(i,i) , c(mu - se,mu+se) , cex=0.5 )

}
#legend("bottomright",legend=c("Total heritability","SNP-heritability"),lty=c(2,1),box.lty=0)
dev.off()
