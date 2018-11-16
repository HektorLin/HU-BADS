# Data and path
setwd("C:/Users/Hector Lin/Documents/HU-courses-practice/PMP")
FX_data = read.csv("FX.csv",  sep = ";")

# Library


# Declaring global variables
No_of_FX_clusters = 3
Sampling_months = 24

# Clearing, and partitioning data
FX_data$EURHKD = NULL
x = sample(c(1:(nrow(FX_data)-Sampling_months)),1)
FX_data_sample = FX_data[x:(x+Sampling_months),]
rm(x)

# K-mean clustering algorithm
corMx = cor(FX_data_sample[,-1])
corMx = corMx - 1 # ?coding distance in a different way?
set.seed(123)
FX_clustering = kmeans(corMx, centers = No_of_FX_clusters, iter.max = 50)
FX_cluster = FX_clustering$cluster

# Result output
print(paste(FX_data_sample[1,1], " to " , FX_data_sample[nrow(FX_data_sample),1]))
for (i in 1:No_of_FX_clusters) {
  print (FX_cluster[FX_cluster == i])
}
