library(clustMixType)
library(cluster)   # silhouette + daisy (Gower)
library(dplyr)
library(ggplot2)
library(tidyr) 

obesity <- read.csv("C:/Users/heath/OneDrive/Documents/Book Math/151/Project/ObesityDataSet.csv")


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
obesity_habits <- obesity[, c("CAEC","CALC","SMOKE","CH2O","FAVC","FAF","MTRANS","NCP")]

# --- ensure no missing values in the features you cluster on ---
obesity_habits <- obesity_habits %>% tidyr::drop_na()

# Precompute Gower distance ONCE for silhouette (works with mixed data)
gower_dist <- daisy(obesity_habits, metric = "gower")

set.seed(42)
ks <- 2:10
fits <- vector("list", length(ks))
metrics <- data.frame(
  k = ks,
  tot_withinss = NA_real_,   # from k-prototypes (elbow)
  avg_sil      = NA_real_    # silhouette on Gower
)

for (i in seq_along(ks)) {
  k  <- ks[i]
  fit <- kproto(obesity_habits, k = k, nstart = 15)
  fits[[i]] <- fit
  
  # elbow: total within-cluster "distance" from the model itself
  metrics$tot_withinss[i] <- fit$tot.withinss
  
  # silhouette: use Gower distance + cluster labels from k-prototypes
  sil <- silhouette(fit$cluster, gower_dist)
  metrics$avg_sil[i] <- mean(sil[, 3])
}

#print(metrics)

# pick best k by silhouette (you could also inspect elbow bend)
best_idx <- which.max(metrics$avg_sil)
best_k   <- metrics$k[best_idx]
best_fit <- fits[[best_idx]]

cat("Best k by silhouette:", best_k,
    " | avg silhouette =", round(metrics$avg_sil[best_idx], 3), "\n")

# --- plots ---
# Elbow (tot.withinss)
ggplot(metrics, aes(k, tot_withinss)) +
  geom_point() + geom_line() +
  labs(title = "k-prototypes elbow", x = "k", y = "Total withinss")

# Silhouette vs k
ggplot(metrics, aes(k, avg_sil)) +
  geom_point() + geom_line() +
  labs(title = "Average silhouette (Gower) vs k", x = "k", y = "Avg silhouette")

# --- Cluster Plot ----
obesity_habits$cluster <- factor(best_fit$cluster)

# helper: mode (most frequent category)
mode1 <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA)
  names(sort(table(x), decreasing = TRUE))[1]
}

# Categorical features: mode by cluster
profile_cat <- obesity_habits %>%
  group_by(cluster) %>%
  summarise(
    CAEC_mode   = mode1(CAEC),
    CALC_mode   = mode1(CALC),
    SMOKE_mode  = mode1(SMOKE),
    FAVC_mode   = mode1(FAVC),
    MTRANS_mode = mode1(MTRANS),
    .groups = "drop"
  )

# Numeric features: mean by cluster
profile_num <- obesity_habits %>%
  group_by(cluster) %>%
  summarise(
    CH2O_mean = round(mean(CH2O, na.rm = TRUE), 2),
    FAF_mean  = round(mean(FAF, na.rm = TRUE), 2),
    .groups = "drop"
  )

profile_cat
profile_num

# Trying to plot as graph
par(mfrow=c(2,2), mar=c(4,4,2,1))
# (1) elbow
plot(metrics$k, metrics$tot_withinss, type="b", pch=19,
     xlab="Number of Clusters (k)", ylab="Total withinss",
     main="Choosing Optimal Number of Clusters")

# attach clusters
plotdat <- transform(obesity_habits, cluster = factor(best_fit$cluster))

# (2) numeric boxplot (pick one)
boxplot(CH2O ~ cluster, data=plotdat, main="CH2O", xlab="cluster", ylab="CH2O")

# (3) categorical bars
barplot(t(prop.table(table(plotdat$cluster, plotdat$CALC), 1)),
        beside=TRUE, legend=TRUE, main="CALC", xlab="cluster", ylab="proportion")

# (4) categorical bars
barplot(t(prop.table(table(plotdat$cluster, plotdat$SMOKE), 1)),
        beside=TRUE, legend=TRUE, main="SMOKE", xlab="cluster", ylab="proportion")

par(mfrow=c(1,2), mar=c(4,4,2,1))

# (5) FAVC (high-calorie food consumption)
tab_favc <- prop.table(table(plotdat$cluster, plotdat$FAVC), margin = 1)
barplot(t(tab_favc), beside = TRUE, legend = TRUE,
        main = "FAVC", xlab = "cluster", ylab = "proportion")

# (6)  MTRANS (transportation)
tab_mtrans <- prop.table(table(plotdat$cluster, plotdat$MTRANS), margin = 1)
barplot(t(tab_mtrans), beside = TRUE, legend = TRUE,
        main = "MTRANS", xlab = "cluster", ylab = "proportion")
# (7) NCP Number Of Meals
boxplot(NCP ~ cluster, data=obesity_habits,
        main="NCP (Number of Meals)", xlab="Cluster", ylab="Meals per day")

# --- Regression ---
# Habits-only regression
reg_habits <- lm(Weight ~ NCP + CALC + FAVC + SMOKE + CH2O + FAF + MTRANS + CAEC, 
                 data = obesity)

summary(reg_habits)



