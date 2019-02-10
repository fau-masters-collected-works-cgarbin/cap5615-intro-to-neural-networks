# Original file used in class with adaptions to work with my
# directory structure and file names

rm(list = ls()) # start fresh to avoid subtle bugs
image_path <- "~/fau/cap5615/homework4"

library(pixmap)  
library(gdata)

left1<-read.pnm(file=paste(image_path, "an2i_left_happy_open_4.pgm", sep = "/"), cellres=1)
plot(left1)
left1.matrix<-left1@grey
left1.vector<-unmatrix(left1.matrix,byrow=T)
left1.frame<-data.frame(left1.vector)

loadImages <- function(pathName,fileNames, clasLabel) {
files<-list.files(path=pathName,pattern=fileNames,all.files=T,full.name=F,no..=T)
list_of_images=lapply(files,read.pnm)
plot(list_of_images[[1]])
n.images<-length(list_of_images)
image.matrix<-list_of_images[[1]]@grey
image.vector<-unmatrix(image.matrix,byrow=T)
for(ii in 2:n.images)
{
 i.matrix<-list_of_images[[ii]]@grey
 i.vector<-unmatrix(i.matrix,byrow=T)
 image.vector<-rbind(image.vector,i.vector)
}
image.frame<-data.frame(image.vector)
n.rows<-nrow(image.frame)
class1.label<-rep(clasLabel,n.rows)
image.frame<-cbind(image.frame,class1.label)
return (image.frame)
}

left.frame <- loadImages(image_path,".*left*.*",1)
right.frame <- loadImages(image_path,".*right*.*",-1)
total.frame<-rbind(left.frame,right.frame)
total.IstNum<-nrow(total.frame)

library(neuralnet)

myform <- as.formula(paste('class1.label ~ ',paste(names(total.frame[!names(total.frame) %in% 'class1.label']),
                     collapse = ' + ')))

face.classifier <- neuralnet(myform, total.frame, hidden = 4, rep=100, linear.output = FALSE, threshold = 0.1)

class.index<-length(total.frame)
face.prediction<-compute(face.classifier,total.frame[,-class.index])
face.prediction$net.result

face.4.3.classifier <- neuralnet(myform, total.frame, hidden = c(4,3), rep=1000, linear.output = FALSE, threshold = 0.1)
face.prediction<-compute(face.4.3.classifier,total.frame[,-class.index])
face.prediction$net.result