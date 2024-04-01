library(funSpaTimeLBM)


data(Velib)
set.seed(12345)

# Co-clustering with funLBM

pos = Velib$positions
#algo sans la dépendance
out = funSpaTimeLBM::funSpaTimeLBM(Velib$data,K=4,L=3,basis.name="fourier",nbasis=5)
# Visualization of results
plot(out,type='blocks')
plot(out,type='proportions')
plot(out,type='means')

#algo avec la dependance temporelle
out = funSpaTimeLBM::funSpaTimeLBM(Velib$data,K=4,L=3,basis.name="fourier",nbasis=5,timevar = c(1:7))
# Visualization of results
plot(out,type='blocks')
plot(out,type='proportions')
plot(out,type='means')

#algo avec la dependance spatiale
out = funSpaTimeLBM::funSpaTimeLBM(Velib$data,K=4,L=3,basis.name="fourier",nbasis=5, spatialvar = pos )
# Visualization of results
plot(out,type='blocks')
plot(out,type='proportions')
plot(out,type='means')

#algo avec la dependance spatio-temporelle
out = funSpaTimeLBM::funSpaTimeLBM(Velib$data,K=4,L=3,basis.name="fourier",nbasis=5,timevar = c(1:7), spatialvar = pos)
# Visualization of results
plot(out,type='blocks')
plot(out,type='proportions')
plot(out,type='means')



################################################################################
################################################################################
################################################################################

data.univariate<-simulateData(n=100,p=90,t=48)
data.multivar<-simulateData2(n=100,p=90,t=48)
res.uni<-funLBM(data.univariate$data,K=4,L=3,
                basis.name = "fourier",nbasis=15,init="funFEM")
plot(res.uni,type='blocks')
plot(res.uni,type='proportions')
plot(res.uni,type='means')
