truth = read.table("sim.out",as.is=T)
truth.mu = unlist(truth[2])
truth.se = unlist(truth[3])
h2 = 0.75

rand = read.table("run_random.out",as.is=T)
P = 50000
P.samp = 5000

svg("plot_krishna_kumar_pnas.svg")

## plot down-sampled results
keep = rand[,2] == "down-sampled"
vals = sort(rand[keep,3] / P.samp)
xlim = range(vals)
n.samp = sum(keep)

rand.mu = mean(rand[keep,3]) / P.samp
rand.se = mean(rand[keep,4]) / P.samp

dens = density(vals)
# get the middle 95%
q = c(vals[ as.integer(n.samp * 0.025) ] , vals[ as.integer(n.samp * 0.975) ])

plot( dens , xlab="per-SNP heritability" , ylab="density" , main="500 random h2 estimates from 5k SNPs" , xlim=xlim )
# shade the middle 95%
x1 = min(which(dens$x >= q[1]))
x2 = max(which(dens$x <  q[2]))
with(dens, polygon(x=c(x[c(x1,x1:x2,x2)]), y= c(0, y[x1:x2], 0), col="gray", border=NA))

# true results
abline( v = truth.mu / P )
abline( v = c(truth.mu - 1.96*truth.se,truth.mu + 1.96*truth.se) / P , lty=2 )
# down-sampled results
abline( v = c(rand.mu - 1.96*rand.se,rand.mu + 1.96*rand.se) , lty=3 )

legend("topleft",legend=c("Estimated h2 from 50k SNPs","Estimated 95CI from 50k SNPs","Estimated 95CI from 5k SNPs") , lty=1:3 , cex=0.75 , bg="white" )
box()

dev.off()
