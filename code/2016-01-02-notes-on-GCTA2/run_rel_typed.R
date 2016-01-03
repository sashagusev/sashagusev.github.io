source('../func_reml.R')
load("../sim.rbin")

h2 = 0.5

seed = as.numeric(commandArgs(trailingOnly=T))[1]
set.seed(seed)

## replace individuals with twins
for ( i in 0:50 ) {
if ( i > 0 ) {
twin.source = i
twin.dest = N + 1 - twin.source
Z[twin.dest,] = Z[twin.source,]
K[[1]][,twin.dest] = K[[1]][,twin.source]
K[[1]][twin.dest,] = K[[1]][twin.source,]
}

## generate a heritable phenotype
# 1. SNP effects
u = rnorm(P.kin,0,1)
# 2. genetic value
g = Z[,1:P.kin] %*% u
g = (g - mean(g))/sd(g)
# 3. add environmental noise
y = sqrt(h2) * g + rnorm(N,0,sqrt(1-h2))
y = (y - mean(y))/sd(y)

## estimate heritability
reml = aiML(K,y,c(0.5,0.5),verbose=F)
cat( seed , i , reml$h2 , reml$se[1] , '\n' )

}
