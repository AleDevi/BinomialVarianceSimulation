##################################################################################
##################################################################################
#Creating a fake dataset to use
xx<-matrix(NA,100,3)
xx[,1]<-rep(c(1:50),2)
xx[,2]<-sample(70:3000, 100, replace=T)
xx[,3]<-runif(100,0,1)
xx<-as.data.frame(xx)
names(xx)<-c("male","sperm","proportion")

##CREATE A FUNCTION TO GENERATE A "SAMPLING DISTRIBUTION" OF LIVE/DEAD
#SPERM BASED ON OBSERVED VIABILITY (WHITHIN MALE) and a given size of sampled 
#samples (i.e. sampled cells):
#
#Simple function with few things... ignore it
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
#THE FINAL FUNCTION
###data: the dataset, 
#"repl": number of iterations (how many times the variance is calculated for each male), 
#"prop": THE COLUMN NUMBER where there is the observed viability
#"cells": THE COLUMN NUMBER where the observed number of sperm is present (it has to be specified IF you want to use the 
#observed number of sperm for each male to calculate the variance)
#"size": sample size (i.e. number of sperm to use), to use if you want to use the same number of sperm for all the males
#K1,K2... THE COLUMN NUMBER to keep from the original dataset. Up to 5
#
#NB: IF "cells" IS NOT SPECIFIED, size has to be specified or a value of 100 is given
#NB: IF "cells" IS SPECIFIED, size is used accordingly to the dataset (number of cells observed) and if another value is used, it is ignored
#NB: IF "repl" is not specified 100 is used

testrand<-function(data,repl=100,prop,cells=NA,size=100,K1=1,K2=0,K3=0,K4=0,K5=0){##data: the dataset, repl: iterazioni per maschio (numero di ricampionamenti, prop=colonna in cui c'è la viability,cells, colonna in cui c'è il numero di spermi,size:sample size (i.e. number of sperm),K1,K2... are columns to keep
        if(is.na(cells)){
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
                samplingmeans<-cbind(data[,c(K1,K2,K3,K4,K5)],repl,celsampled,samplingvariance)#add ",samplingmeans[,c(1:5)]" after celsampled to also add the firs 5 sampled proportions
                samplingmeans
                
                
        }
        else{
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
                samplingmeans<-cbind(data[,c(K1,K2,K3,K4,K5)],repl,celsampled,samplingvariance)#add ",samplingmeans[,c(1:5)]" after celsampled to also add the firs 5 sampled proportions
                samplingmeans      
        }
}



testrand(xx,prop=3,cells=2,size=40,K1=1,K2=3,K3=1,K4=1)

#######testing time with different sizes

results<-NULL
longness<-(NULL)

for (repl in c(10,20,50,100,500,1000,2000)){for (size in c (10,20,50,100,500,1000,2000)){
        #y<-testrand(xx, repl=repl,size=size,prop=3)
        #results<-cbind(results,y)
        #results
        y<-c(repl,size,system.time(testrand(xx, repl=repl,size=size,prop=3)))#change size or repl value to "repl" to have a single line in the pot below
        longness<-rbind(longness,y)
        longness
}}
library(ggplot2)
plot(longness[,1],longness[,2])
ggplot()+geom_point(aes(x=longness[,1],y=longness[,2],size=longness[,5],col=longness[,5]))

#############################################################################


dataset<-read.csv("C:/Users/Color&Sound/OneDrive/Desktop/sperm_phenotype.csv",
                    header = T)
dataset

##data: the dataset, 
##repl: iterazioni per maschio (numero di ricampionamenti, 
##prop=colonna in cui c'è la viability,
##cells, colonna in cui c'è il numero di spermi,
##size:sample size (i.e. number of sperm)

testrand(dataset,repl=100,prop=10,size=50,K1=2,K2=3)



