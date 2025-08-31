library(dplyr)
library(cluster)
library(ggplot2)

df <- read.csv("C:/Users/heath/OneDrive/Documents/Math151/151Project/ObesityDataSet.csv")

# CREATING CLUSTER STEPS

# Step 1: Prepare data Age, Height, Weight
physical_df <- df |> select(Age, Height, Weight)
physical_scaled <- scale(physical_df)

# Step 2: Initialize lists and variables
kmeans.out <- list()       
ratio <- NULL

# Step 3: Try different numbers of clusters
for (g in 2:6) {
  kmeans.out[[g]] <- kmeans(x = physical_scaled, centers = g, nstart = 50)
  ratio[g] <- kmeans.out[[g]]$betweenss*(nrow(physical_scaled)-g)/(kmeans.out[[g]]$tot.withinss*(g-1))
}


# Step 4: Find best number of clusters
best.k <- which.max(ratio==max(ratio,na.rm=TRUE))
cat("Best number of clusters based on ratio score is:", best.k, "\n")

# Step 5: Find K-means clustering with best k
set.seed(123)
final_kmeans <- kmeans(physical_scaled, centers = best.k, nstart = 50)
df$cluster <- final_kmeans$cluster

# Step 6: Convert cluster label to factor
df$cluster <- as.factor(final_kmeans$cluster)

# Regression Line 1
# Step 1: Fit models by using model selection

lm_df <- lm(Weight ~ ., data = physical_df)
step_df <- step(lm_df, direction = "both")
summary(step_df) #Shows that all physical traits variables are significant

# Step 2: 
# Get unique clusters
clusters <- unique(df$cluster)

# Create a list of models for each cluster
models <- lapply(clusters, function(clust) {
  lm(Weight ~ Height, data = df[df$cluster == clust, ])
})

# Name the models by cluster number
names(models) <- as.character(clusters)

# Step 3:
# Create prediction dataframe for each cluster to put in geomline()

# Create a smooth sequence of height value
height_seq <- seq(min(df$Height), max(df$Height), length.out = 100)

#Looping though each cluster from models list
# then grabs regression model for each cluster
pred_lines <- lapply(names(models), function(clust) 
{
  newdata <- data.frame(Height = height_seq) #smooth height value
  newdata$Weight <- predict(models[[clust]], newdata = newdata) #predict weights
  newdata$cluster <- clust #cluster label
  return(newdata) #returning to mini dataframe
}) |> bind_rows()

#ASSUMPTION CHECK 1

# Check assumptions for each cluster model
par(mfrow = c(2, 2))  # 2x2 plot layout

for (clust in names(models)) {
  cat("Assumption check for Cluster", clust, "\n")
  
  model <- models[[clust]]
  
  # Basic diagnostic plots
  plot(model, which = 1:2)  # Residuals vs Fitted, Normal Q-Q
  
  # Check for homoscedasticity (Residuals vs Fitted plot should show random scatter)
  # Check for normality (Q-Q plot points should lie close to the line)
  
  # Additional test for normality
  shapiro.test(residuals(model))  # p-value > 0.05 suggests normality
  
  # Additional test for constant variance (Breusch-Pagan Test)
  if (!require(lmtest)) install.packages("lmtest")
  library(lmtest)
  print(bptest(model))  # p-value > 0.05 suggests homoscedasticity
  
  cat("\n")
}



# READING DATA


cluster_colors <- c(
  "1" = "red",
  "2" = "blue",
  "3" = "green",
  "4" = "orange",
  "5" = "purple"
)



# 1. 3D Plot
# Basic scatterplot matrix
pairs(physical_df,
      main = "Scatterplot Matrix: Height, Weight, Age",
      pch = 21,
      bg = cluster_colors[as.character(df$cluster)])



# 2. PLOT the Clusters + Regression Line for Weight Vs Height
ggplot(df, aes(x = Height, y = Weight, color = cluster)) +
  geom_point(alpha = 0.5) +
  geom_line(data = pred_lines, aes(x = Height, y = Weight, color = cluster), size = 1.2) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "Cluster-Specific Regression Lines: Weight ~ Height",
       x = "Height (m)", y = "Weight (kg)") +
  theme_minimal()

# 3. Reading Tables

# Now check majority Obesity level per cluster
table(df$cluster, df$NObeyesdad)
prop.table(table(df$cluster, df$NObeyesdad), margin = 1)

# proportions for gender
prop.table(table(df$cluster, df$Gender), margin = 1)


# 4. Plot for Regression Line 2 Weight vs Age

# Step 2: 
# Create a list of models for each cluster
models <- lapply(clusters, function(clust) {
  lm(Weight ~ Age, data = df[df$cluster == clust, ])
})

# Name the models by cluster number
names(models) <- as.character(clusters)

# Step 3:
# Create prediction dataframe for each cluster to put in geomline()

# Create a smooth sequence of Age value
Age_seq <- seq(min(df$Age), max(df$Age), length.out = 100)

#Looping though each cluster from models list
# then grabs regression model for each cluster
pred_lines <- lapply(names(models), function(clust) 
{
  newdata <- data.frame(Age = Age_seq) #smooth Age value
  newdata$Weight <- predict(models[[clust]], newdata = newdata) #predict weights
  newdata$cluster <- clust #cluster label
  return(newdata) #returning to mini dataframe
}) |> bind_rows()

# PLOT the Clusters + Regression Line for Weight Vs Age
ggplot(df, aes(x = Age, y = Weight, color = cluster)) +
  geom_point(alpha = 0.5) +
  geom_line(data = pred_lines, aes(x = Age, y = Weight, color = cluster), size = 1.2) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "Cluster-Specific Regression Lines: Age ~ Weight",
       x = "Age (yr)", y = "Weight (kg)") +
  theme_minimal()

