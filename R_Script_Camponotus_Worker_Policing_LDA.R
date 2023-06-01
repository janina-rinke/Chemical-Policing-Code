
################################################################################################################################
# Evolutionary Ecology, 2023

# Conserved worker policing in African carpenter ants with drastically different egg chemotypes 
# Jan Buellesbach, Janina Rinke, Leonie Reuter, Jurian M. Thomas, Vivien Hartmann, Marius Pohl, Jürgen Gadau and Ulrich R. Ernst


# R-Script to conduct the Linear Discriminant Analysis (Fig. 6)
################################################################################################################################


############ Start completely fresh ####################

rm(list = ls())

############# Load all required libraries ################

install.packages("vegan")
library(vegan)
install.packages("MASS")
library(MASS)

############### Read in your data  ###########################

# dataset HAS to be in the following format: unique column names identify the variables, just the FIRST column as sample identifier with a simple yet unique acronym unambiguously identifying the sample groups, then numbered (_01, _02, ..) within each sample group 

dataset <- as.matrix(read.csv(file.choose(),header=T,row.names=1,dec=".", sep=",",check.names=FALSE))

# in case the dataset did not read in right, switch around dec (decimal separator or sep (column separator) in the read.csv command)

# Get rid of NAs

for (i in 1:length(dataset)) if (is.na(dataset[i])) dataset[i] <- 0.0 

# Order your data frame according to the row.names before you continue from here:

dataset <- as.data.frame(dataset[order(row.names(dataset)),])

# Create a grouping vector based on the length of your individual group identifiers

vec <- vector(length=length(row.names(dataset))) 
for (i in 1:length(vec)) vec[i] = substr(row.names(dataset)[i],1,7)


# Normalize your data to, make them comparable, eliminate size differences etc.:


for (i in 1:nrow(dataset)) {
  dataset[i,]	<- dataset[i,] / sum(dataset[i,]) * 100
}


####### LDA: Linear discriminant analysis ######################

Disc=lda(dataset,vec)

disc2=predict(Disc)$x

prob <- (Disc$svd^2)/sum((Disc$svd^2))

plot(disc2[,1],disc2[,2],col=0,xlab=paste("Discriminant function 1"," (",round(prob[1]*100,2)," %)"),ylab=paste("Discriminant function 2"," (",round(prob[2]*100,2)," %)"))

unique(vec) 

pchseq <- c(15,19,22,21)

colseq <- c("firebrick3","firebrick3","dodgerblue3","dodgerblue3")            
            
for (i in 1:length(unique(vec))) points(disc2[vec==unique(vec)[i],1],disc2[vec==unique(vec)[i],2],pch=pchseq[i],col=colseq[i]) 
for (i in 1:length(unique(vec))) text(mean(disc2[vec==unique(vec)[i],1]),mean(disc2[vec==unique(vec)[i],2]),unique(vec)[i],cex=1)

####### Create table with mean and standard deviations ##########

means <- matrix(nrow=length(colnames(dataset)),ncol=length(unique(vec)))

for (i in 1:length(unique(vec))) for (j in 1:length(dataset[1,])) 
  means[j,i] = paste(round(mean(dataset[vec==unique(vec)[i],j]),2),"±",round(sd(dataset[vec==unique(vec)[i],j]),2))

row.names(means)=colnames(dataset)
colnames(means)=unique(vec)

write.table(means,file="means+sd_percent.csv",sep=",",dec=".")
