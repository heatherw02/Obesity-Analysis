
##Clustering with Habits Code

obesity <-  read.csv("C:/Users/heath/OneDrive/Documents/Book Math/151/Project/ObesityDataSet.csv")

#First, cluster analysis
library(klaR)
library(clustMixType)
library(cluster)

#convert categorical variables to factors
obesity$Gender <- as.factor(obesity$Gender)
obesity$CAEC <- as.factor(obesity$CAEC) #Do you eat any food between meals?
obesity$FAVC <- as.factor(obesity$FAVC) #Do you eat high caloric food frequently?
obesity$CALC <- as.factor(obesity$CALC) #How often do you drink alcohol?
obesity$MTRANS <- as.factor(obesity$MTRANS) #Which transportation do you usually use?
obesity$NObeyesdad  <- as.factor(obesity$NObeyesdad) #Obesity Level
obesity$family_history_with_overweight  <- as.factor(obesity$family_history_with_overweight)
obesity$FAVC  <- as.factor(obesity$FAVC) #Do you eat high caloric food frequently?
obesity$SMOKE  <- as.factor(obesity$SMOKE) #Do you smoke?
obesity$SCC  <- as.factor(obesity$SCC) #Do you monitor the calories you eat daily
#obesity$FCVC  <- factor(obesity$FCVC, ordered=TRUE) #Do you usually eat vegetables in your Meal #IGNORE THIS BAD DATA

#separation
obesity_habits <- obesity[, c(8, 15, 10, 11, 6, 13, 16)] #CAEC, CALC, SMOKE, CH20, FAVC, FAF, MTRANS
obesity_habits

#calculate totss ONCE outside the loop
totss <- sum(daisy(obesity_habits, metric = "gower")^2)

kmeans.out <- list()
ratio <- numeric(10)  # safer to pre-allocate

#loop
for(g in 2:10){
  kmeans.out[[g]] <- kproto(obesity_habits, k = g, nstart = 15)
  
  #calculate betweenss manually and ratio
  betweenss <- totss - kmeans.out[[g]]$tot.withinss
  ratio[g] <- betweenss * (nrow(obesity_habits) - g) / (kmeans.out[[g]]$tot.withinss * (g - 1))
}

#plots
plot(2:10, ratio[2:10], type = "b", pch = 19,
     xlab = "Number of Clusters (g)", ylab = "Ratio",
     main = "Choosing Optimal Number of Clusters")

plot(kmeans.out[[2]])
kmeans.out[[2]]$cluster


#SEPERATE CLUSTERS
clusters <- kmeans.out[[2]]$cluster
cluster_1 <- obesity[clusters == 1, ]
cluster_2 <- obesity[clusters == 2, ]

#investigating NCP bc its weird
ncp_cluster1 <- cluster_1$NCP
mean_ncp <- mean(ncp_cluster1)
median_ncp <- median(ncp_cluster1)
q1_ncp <- quantile(ncp_cluster1, 0.25)
q3_ncp <- quantile(ncp_cluster1, 0.75)
min_ncp <- min(ncp_cluster1)
max_ncp <- max(ncp_cluster1)

cat("Mean:", mean_ncp, "\n")
cat("Median:", median_ncp, "\n")
cat("Q1:", q1_ncp, "\n")
cat("Q3:", q3_ncp, "\n")
cat("Min:", min_ncp, "\n")
cat("Max:", max_ncp, "\n")

#and cluster 2
ncp_cluster2 <- cluster_2$NCP
mean_ncp2 <- mean(ncp_cluster2)
median_ncp2 <- median(ncp_cluster2)
q1_ncp2 <- quantile(ncp_cluster2, 0.25)
q3_ncp2 <- quantile(ncp_cluster2, 0.75)
min_ncp2 <- min(ncp_cluster2)
max_ncp2 <- max(ncp_cluster2)

cat("Mean:", mean_ncp2, "\n")
cat("Median:", median_ncp2, "\n")
cat("Q1:", q1_ncp2, "\n")
cat("Q3:", q3_ncp2, "\n")
cat("Min:", min_ncp2, "\n")
cat("Max:", max_ncp2, "\n")


regression_cluster_1 <- lm(Weight ~ NCP + CALC + SMOKE+ CH2O + FAVC + FAF + MTRANS, data=cluster_1)
regression_cluster_2 <- lm(Weight ~ NCP + CALC + SMOKE+ CH2O + FAVC + FAF + MTRANS, data=cluster_2)
#cannot graph :(
#give interpretation of coefficents

summary(regression_cluster_1)
summary(regression_cluster_2)