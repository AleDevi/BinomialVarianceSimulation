##################################################################################
##################################################################################
#Creating a fake dataset to use
xx<-matrix(NA,100,3)
xx[,1]<-rep(c(1:50),2)
xx[,2]<-sample(70:3000, 100, replace=T)
xx[,3]<-runif(100,0,1)
xx<-as.data.frame(xx)

##CREATE A FUNCTION TO GENERATE A "SAMPLING DISTRIBUTION" OF LIVE/DEAD
#SPERM BASED ON OBSERVED VIABILITY (WHITHIN MALE) and a given size of sampled 
#samples (i.e. sampled cells):
#
#
testrand1<-function(data,size,repl,z,w){##data: the dataset, size:sample size (i.e. number of sperm), repl: iterazioni per maschio (numero di sottocampioni)
        samplingmeans<-matrix(NA,nrow(data),repl)#these are the proportions of the sampled sperm 
        samplingvariance<-NA
        yy<-matrix(NA,nrow=nrow(data),max(data,2))## max(data,2) to generate a matrix with a number of coums equal to the biggest number of perm collected
        celsampled<-NA
        for(j in 1:repl){
                for (i in 1:nrow(data)){
                        yy[i,]<-c(rbinom(data[i,2], 1, data[i,3]),rep(NA,(max(data,2)-data[i,2])))#yy[i,]<-rbinom(data[i,2], 1, data[i,3])### data[i,2] here is to generate a binomial variable based on the number of sperm observed. it was "size" to generate one based on a fixed sample
                        samplingmeans[,j]<-rowMeans(yy,na.rm =T) #in samplingmeans mi genera una serie di proporzioni (n=repl) basate OGNI VOLTA su un numero di "spermi" uguale a quelli testati nel maschio
                }
                celsampled<-rowSums(!is.na(yy))}
        for (h in 1:nrow(samplingmeans)){
                samplingvariance[h]<-var(samplingmeans[h,])
        }
        samplingmeans<-cbind(data[,c(1:3)],samplingvariance,celsampled,samplingmeans[,c(1:5)])
        samplingmeans
}


########################
##data: the dataset, 
#repl: iterazioni per maschio (numero di ricampionamenti), 
#prop=colonna in cui c'è la viability,
#cells, colonna in cui c'è il numero di spermi,
#size:sample size (i.e. number of sperm),
#K1,K2... are columns to keep

testrand<-function(data,repl,prop,cells=1,size=max(data[,cells]),K1=1,K2=0,K3=0,K4=0){##data: the dataset, repl: iterazioni per maschio (numero di ricampionamenti, prop=colonna in cui c'è la viability,cells, colonna in cui c'è il numero di spermi,size:sample size (i.e. number of sperm),K1,K2... are columns to keep
        if(size==max(data[,cells])){
                samplingmeans<-matrix(NA,nrow(data),repl)#these are the proportions of the sampled sperm 
                samplingvariance<-NA
                yy<-matrix(NA,nrow=nrow(data),max(data[,cells]))## max(data,cells) to generate a matrix with a number of coums equal to the biggest number of perm collected
                celsampled<-NA
                for(j in 1:repl){
                        for (i in 1:nrow(data)){
                                yy[i,]<-c(rbinom(data[i,cells], 1, data[i,prop]),rep(NA,max(data[,cells])-data[i,cells]))#yy[i,]<-rbinom(data[i,2], 1, data[i,3])### data[i,2] here is to generate a binomial variable based on the number of sperm observed. it was "size" to generate one based on a fixed sample
                                samplingmeans[,j]<-rowMeans(yy,na.rm =T) #in samplingmeans mi genera una serie di PROPORZIONI (n=repl) basate OGNI VOLTA su un numero di "spermi" uguale a quelli testati nel maschio. Sono le proporzioni simulate
                        }
                        celsampled<-rowSums(!is.na(yy))}
                for (h in 1:nrow(samplingmeans)){
                        samplingvariance[h]<-var(samplingmeans[h,])
                }
                samplingmeans<-cbind(data[,c(K1,K2,K3,K4)],repl,celsampled,samplingvariance)#add ",samplingmeans[,c(1:5)]" after celsampled to also add the firs 5 sampled proportions
                samplingmeans
        }
        else{
                samplingmeans<-matrix(NA,nrow(data),repl)#these are the proportions of the sampled sperm 
                samplingvariance<-NA
                yy<-matrix(NA,nrow=nrow(data),size)## max(data,cells) to generate a matrix with a number of coums equal to the biggest number of perm collected
                celsampled<-NA
                for(j in 1:repl){
                        for (i in 1:nrow(data)){
                                yy[i,]<-rbinom(size, 1, data[i,prop])#yy[i,]<-rbinom(data[i,2], 1, data[i,3])### data[i,2] here is to generate a binomial variable based on the number of sperm observed. it was "size" to generate one based on a fixed sample
                                samplingmeans[,j]<-rowMeans(yy,na.rm =T) #in samplingmeans mi genera una serie di proporzioni (n=repl) basate OGNI VOLTA su un numero di "spermi" uguale a quelli testati nel maschio. Sono le proporzioni simulate
                        }
                        celsampled<-rowSums(!is.na(yy))}
                for (h in 1:nrow(samplingmeans)){
                        samplingvariance[h]<-var(samplingmeans[h,])
                }
                samplingmeans<-cbind(data[,c(K1,K2,K3,K4)],repl,celsampled,samplingvariance)#add ",samplingmeans[,c(1:5)]" after celsampled to also add the firs 5 sampled proportions
                samplingmeans       
        }
}


#test the function
testrand(xx,repl=50,3)
as.data.frame(testrand(xx,repl=100,prop=3,size=30))

names(results)<-c("male","obssperm","obsprop","permutations","cells","variance")
plot(results$obsprop,results$variance)



#test the function with number of sperm
results<-matrix(NA,nrow(xx),3)
for(i in c(200,500,1000)){for(h in 1:3){
        results[,h]<-testrand(xx,size=i,repl=500)[,6]
        results
}
}

#test the function with different numner of simulations
results2<-matrix(NA,nrow(xx),3)
for(i in c(200,500,1000)){for(h in 1:3){
        results2[,h]<-testrand(xx,size=50,repl=i)[,6]
        results2
}
}

hist(results[,1])
hist(results[,2])
hist(results[,3])

hist(results2[,1])
hist(results2[,2])
hist(results2[,3])



dataset<-read.csv("C:/Users/Color&Sound/OneDrive/Desktop/sperm_phenotype.csv",
                    header = T)
dataset

##data: the dataset, 
##repl: iterazioni per maschio (numero di ricampionamenti, 
##prop=colonna in cui c'è la viability,
##cells, colonna in cui c'è il numero di spermi,
##size:sample size (i.e. number of sperm)


dataset2<-dataset[,c(1,10,11)]
testrand(dataset,repl=100,prop=10,cells=11,K1=2,K2=3)
str(dataset2)

testrand(dataset,repl=100,prop=10,size=50,K1=2,K2=3)


testrand(xx,repl=100,prop=3,cells=2,K1=1,K2=2)


