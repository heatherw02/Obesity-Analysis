# Estimation of Obesity Levels Based on Eating Habits and Physical Condition

**Value Statement:**  
This project analyzes how eating habits and physical traits affect obesity levels, using clustering and regression techniques. The insights can guide both personal health decisions and data-driven public health strategies.

---

## Goal
To identify patterns and risk factors associated with obesity by analyzing lifestyle habits (e.g., meal frequency, high-calorie food, alcohol intake) and physical characteristics (e.g., height, weight, age).  

Specifically:  
- Which lifestyle habits are most correlated with obesity?  
- How do physical traits cluster by obesity category?  
- Can obesity levels be predicted reliably from available features?  

---

## Data
- **Source:** [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Estimation+of+obesity+levels+based+on+eating+habits+and+physical+condition)  
- **Size:** 2,111 individuals, 17 attributes  
- **Features:**  
  - Demographics: Gender, Age, Height, Weight  
  - Habits: High-calorie food (FAVC), number of meals (NCP), snacks, smoking, water intake, physical activity (FAF), transportation (MTRANS), etc.  
  - Target: Obesity category (7 classes: Insufficient Weight → Obesity Type III)  
- **Privacy:** Public dataset; anonymized, no personal identifiers  

---

## 🔧 Methods
- **Cleaning:** Checked for missing values (none), normalized height/weight to compute BMI, encoded categorical variables.  
- **EDA:** Visual summaries of habits (e.g., FAVC, NCP, MTRANS, FAF) vs. obesity levels.  
- **Clustering:**  
  - K-means clustering on physical traits (height, weight, age) and on habit traits.  
  - PCA for dimensionality reduction and visualization.  
- **Regression/Classification:**  
  - Logistic regression for obesity category prediction.  
  - Regression models to test contribution of physical vs. habit traits.  

---

## Results (Highlights)
- **Physical traits dominate**: height, weight, and age strongly drive obesity categories (expected due to BMI).  
- **Habit traits matter too**:  
  - Higher NCP (number of meals) + FAVC (frequent high-calorie foods) = higher obesity risk.  
  - Active lifestyle (FAF) and healthier transport choices (walking, biking) correlate with lower obesity levels.  
- **Clustering:**  
  - K-means separated individuals cleanly by physical traits (BMI-driven).  
  - Habits-based clusters were more mixed, but showed patterns (e.g., snacking + low activity grouped together).  
- **PCA:** first 2 components explained most variance in physical traits; visual separation between obesity classes.  

*(Insert plots here: e.g., PCA plot, clustering visual, regression coefficients, habit vs. physical comparison.)*  

---

## Reproduce
```bash
# Clone repo
git clone https://github.com/<your-username>/obesity-clustering-r.git
cd obesity-clustering-r

# R environment
install.packages("renv")
renv::restore()

# Run analysis
Rscript src/analysis.R
