source('func_reml.R')

# reproducibility!
set.seed(1234)

N = 1000
P = as.integer(50000/3)
# half the SNPs will go into the kinship, the others are "untyped"
P.kin = as.integer(P*0.5)
h2 = 0.75

## generate genotype matrix with MAF = 0.5
Z = matrix(rbinom(N*P,2,0.5),nrow=N,ncol=P)

# standardize
for ( i in 1:P ) {
Z[,i] = (Z[,i] - mean(Z[,i]))/sd(Z[,i])
if ( i %% 1000 == 0 ) cat(i,'\n',file=stderr())
}

## generate kinship
K = list()
K[[1]] = Z[,1:P.kin] %*% t(Z[,1:P.kin]) / P.kin
save(N,P,P.kin,h2,K,Z,file="sim.rbin")
