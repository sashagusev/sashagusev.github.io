library('vioplot')
tbl = read.table("run_rel.out",as.is=T)
svg("SKK_twins_untyped.svg",width=9,height=4)
plot(0,0,type="n",xlab="# twins (out of 1,000 individuals)",ylab="h2 estimate",xlim=c(0,50),ylim=c(0,1),main="Estimated heritability with relatedness")
abline(h=c(0.75/2,0.75),lty=1:2)

for ( i  in seq(0,50,by=1) ) { 
keep = tbl[,2] == i
vioplot( tbl[keep,3] , at=i , add=T , col="#CCCCCC", border="#AAAAAA",drawRect=F)
mu = mean(tbl[keep,3])
se = mean(tbl[keep,4])
sd = sd(tbl[keep,3])
##cat( se^2 / sd^2 , '\n' )
points( i , mu , pch=19, , col="black")
arrows( i , mu - sd , i , mu + sd , angle=0)
points( c(i,i) , c(mu - se,mu+se) , cex=0.5 )
}
legend("bottomright",legend=c("Total heritability","SNP-heritability"),lty=c(2,1),box.lty=0,cex=0.75)
box()
dev.off()
