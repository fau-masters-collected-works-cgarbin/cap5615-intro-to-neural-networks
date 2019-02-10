rm(list = ls())

class1<-read.table("Class1.txt",header=TRUE,sep=",") 
class1.label<-rep(1,100)
class1<-cbind(class1,class1.label)
names(class1)<-c("weight","height","label")

class2<-read.table("Class2.txt",header=TRUE,sep=",")
class2.label<-rep(-1,100)
class2<-cbind(class2,class2.label)
names(class2)<-c("weight","height","label")

class1.2<-rbind(class1,class2)

d.set<-data.frame(cbind(rep(1,200),class1.2))
names(d.set)<-c("bias","weight","height","label")

plot(d.set[1:100,]$weight,d.set[1:100,]$height,xlim=c(0:1),ylim=c(0:1),col="red")
points(d.set[101:200,]$weight,d.set[101:200,]$height,col="blue")

samples<-sample(nrow(d.set))
randomized.set<-d.set[samples,]

perceptron <- function(x, eta, niter) {
        
        # initialize weight vector
        weight <- rep(0.1, dim(x)[2]-1)
        errors <- rep(0, niter)
        label.index<-length(x[1,])
        features<-x[,-label.index]
        labels<-x[,label.index]
        
        # loop over number of epochs niter
        for (jj in 1:niter) {
                
                # loop through training data set
                for (ii in 1:nrow(x)) 
                {
                        
                        # Predict binary label using activation function
                        z <- sum(weight[1:length(weight)] * as.numeric(features[ii,])) 
                        if(z < 0) 
                        {
                                ypred <- -1
                        } else {
                                ypred <- 1
                        }
                        
                        # Change weight - the formula doesn't do anything 
                        # if the predicted value is correct
                        weightdiff <- eta * (as.numeric(labels[ii]) - ypred) * as.numeric(features[ii,])
                        weight <- weight + weightdiff
                        
                        # update error rate
                        if ((as.numeric(labels[ii]) - ypred) != 0.0) 
                        {
                                errors[jj] <- errors[jj] + 1
                        }
                        
                }
        }
        
        # weight to decide between the two species 
        print(weight)
        print(errors)
        return(list(v1=weight,v2=errors))
}

iterations<-1000
weight.err <- perceptron(randomized.set, 0.2, iterations)
plot(1:iterations,weight.err$v2)

plot(d.set[1:100,]$weight,d.set[1:100,]$height,xlim=c(0:1),ylim=c(0:1),col="red")
points(d.set[101:200,]$weight,d.set[101:200,]$height,col="blue")
slope<-weight.err$v1[2]/weight.err$v1[3]*(-1)
intercept<-weight.err$v1[1]/weight.err$v1[3]*(-1)
abline(intercept,slope,col="green",lty=2)

print(weight.err$v1)
print(slope)
print(intercept)
