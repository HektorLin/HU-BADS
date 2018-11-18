# Library
#install.packages("RCurl")
library("RCurl")

# Data and path
#setwd("C:/Users/Hector Lin/Documents/HU-courses-practice/PMP")
FX_data = read.csv(text=getURL("https://raw.githubusercontent.com/HektorLin/HU-courses-practice/master/PMP/FX.csv"),sep = ";")

# Declaring global variables
No_of_FX_clusters = 2
Sampling_months = 24

# Clearing, and partitioning data
FX_data$EURHKD = NULL
set.seed(as.numeric(Sys.time()))
x = sample(c(1:(nrow(FX_data)-Sampling_months)),1)
FX_data_sample = FX_data[x:(x+Sampling_months),]
rm(x)

# K-mean clustering
corMx = cor(FX_data_sample[,-1])
corMx = corMx - 1 # ?coding distance in a different way?
FX_clustering = kmeans(corMx, centers = No_of_FX_clusters, iter.max = 50, nstart = 10)
FX_cluster = FX_clustering$cluster

# Direct result output
print(paste(FX_data_sample[1,1], " to " , FX_data_sample[nrow(FX_data_sample),1]))
for (i in 1:No_of_FX_clusters) {
  print (FX_cluster[FX_cluster == i])
}

# Iterated clustering prob. result
PMP_FX_cluster = function (a,b, m) {
  # a,b = different currency pair
  # i = # of sampling and clustering
  same_cluster_count = 0
  for (i in 1:m) {
    set.seed(as.numeric(Sys.time())+i)
    x = sample(c(1:(nrow(FX_data)-Sampling_months)),1)
    FX_data_sample = FX_data[x:(x+Sampling_months),]
    rm(x)
    corMx = cor(FX_data_sample[,-1])
    corMx = corMx - 1
    FX_clustering = kmeans(corMx, centers = No_of_FX_clusters, iter.max = 50, nstart = 10)
    FX_cluster = FX_clustering$cluster
    if (FX_cluster[a] == FX_cluster[b]) (same_cluster_count = same_cluster_count + 1)
  }
  return(same_cluster_count/m)
}
trial1 = PMP_FX_cluster("EURUSD","EURGBP",20)
print(trial1)
