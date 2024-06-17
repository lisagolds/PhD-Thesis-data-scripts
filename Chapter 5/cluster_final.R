
library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)




  
#Step 1 - inspecting data
summary(df_whole)


#Checking missing data

library(naniar)

any_na(df_whole)
n_miss(df_whole)

selected_vars <- c("ASQ_1", "ASQ_1.1","ASQ_2","ASQ_2.1","ASQ_3","ASQ_3.1",
                   "ASQ_4","ASQ_4.1","ASQ_5","ASQ_5.1","ASQ_6","ASQ_6.1",
                   "ASQ_7","ASQ_7.1","ASQ_8", "ASQ_8.1","ASQ_9","ASQ_9.1",
                   "ASQ_10","ASQ_10.1", "ASQ_11","ASQ_11.1","ASQ_12","ASQ_12.1",
                   "ASQ_13","ASQ_13.1","ASQ_14","ASQ_14.1", "ASQ_15","ASQ_15.1", 
                   "ASQ_16","ASQ_16.1","ASQ_17","ASQ_17.1","ASQ_18","ASQ_18.1", 
                   "ASQ_19","ASQ_19.1","ASQ_20","ASQ_20.1","ASQ_21","ASQ_21.1",
                   "ASQ_22","ASQ_22.1","ASQ_23","ASQ_23.1")
               



total_missing <- sum(is.na(df_whole[, selected_vars]))

total_potential_observations <- length(selected_vars) * nrow(df_whole)

percentage_missing <- (total_missing / total_potential_observations) * 100


print(percentage_missing)


# Where are NAs located?
vis_miss(df_whole)

prop_miss(df_whole)


#Step 2 - imputing knn data
library(VIM)

df_total <- kNN(df_whole, k = 10)

summary(df_total)

    

#Step 3 - total scores

df_scores <- df_total %>% mutate(
  dass_s = DASS_1 + DASS_6 + DASS_8 + DASS_11 + DASS_12 + DASS_14 + DASS_18,
  dass_a = DASS_2 + DASS_4 + DASS_7 + DASS_9 + DASS_15 + DASS_19 + DASS_20,
  dass_d = DASS_3 + DASS_5 + DASS_10 + DASS_13 + DASS_16 + DASS_17 + DASS_21,
  dass_total = dass_s + dass_a + dass_d,
  asq = ASQ_1 + ASQ_1.1 + ASQ_2 + ASQ_3 + ASQ_4 + ASQ_4.1 + ASQ_5 + ASQ_6 + ASQ_6.1 + ASQ_7 + 
    ASQ_7.1 + ASQ_8 + ASQ_9 + ASQ_10 + ASQ_11 + ASQ_11.1 + ASQ_12 + ASQ_12.1 + ASQ_13 + ASQ_13.1 + ASQ_14 + 
    ASQ_14.1 + ASQ_15 + ASQ_16 + ASQ_16.1 + ASQ_17 + ASQ_18 + ASQ_18.1 + ASQ_19 + ASQ_20 + ASQ_20.1 + ASQ_21 + 
    ASQ_22 + ASQ_23 + ASQ_23.1,
  who = WHO_1 + WHO_2 + WHO_3 + WHO_4 + WHO_5,  
  isel_a = ISEL_2 + ISEL_4 + ISEL_6 + ISEL_11,
  isel_b = ISEL_1 + ISEL_5 + ISEL_7 + ISEL_9,
  isel_t = ISEL_3 + ISEL_8 + ISEL_10 + ISEL_12,
  isel_total = isel_a + isel_b + isel_t,
  tips = TIPS_1 + TIPS_2 + TIPS_3 + TIPS_4 + TIPS_5 + TIPS_6 + TIPS_7 + TIPS_8 + TIPS_9 + TIPS_10 + TIPS_11 + TIPS_12
  + TIPS_13 + TIPS_14,
  miri = MIRI_1 + MIRI_2 + MIRI_3 + MIRI_4 + MIRI_5 + MIRI_6 + MIRI_7 + MIRI_8 + MIRI_9 + MIRI_10 + 
    MIRI_11 + MIRI_12 + MIRI_13 + MIRI_14 + MIRI_15 + MIRI_16 + MIRI_17 + MIRI_18 + MIRI_19 + MIRI_20 + 
    MIRI_21 + MIRI_22
)



#Step 4 - Standardised scores
df_scores$zasq <- (df_scores$asq - mean(df_scores$asq)) / sd(df_scores$asq)
df_scores$zdass_d <- (df_scores$dass_d - mean(df_scores$dass_d)) / sd(df_scores$dass_d)
df_scores$zdass_a <- (df_scores$dass_a - mean(df_scores$dass_a)) / sd(df_scores$dass_a)
df_scores$zdass_s <- (df_scores$dass_s - mean(df_scores$dass_s)) / sd(df_scores$dass_s)
df_scores$zwho <- (df_scores$who - mean(df_scores$who)) / sd(df_scores$who)
df_scores$zisel_a <- (df_scores$isel_a - mean(df_scores$isel_a)) / sd(df_scores$isel_a)
df_scores$zisel_b <- (df_scores$isel_b - mean(df_scores$isel_b)) / sd(df_scores$isel_b)
df_scores$zisel_t <- (df_scores$isel_t - mean(df_scores$isel_t)) / sd(df_scores$isel_t)
df_scores$ztips <- (df_scores$tips - mean(df_scores$tips)) / sd(df_scores$tips)
df_scores$zmiri <- (df_scores$miri - mean(df_scores$miri)) / sd(df_scores$miri)


#Stratifying by age
#df_scores$age_category <- ifelse(df_scores$infant_age < 6, "1", "2")
#category_counts <- table(df_scores$age_category)
#print(category_counts)

#(1) 3-6_months (2) 6-9_months
#    224            226 

# Include infant_age_category in the dataframe used for clustering
#df_scores$age_category <- df_scores$age_category


#df_3_6_pop <- df_scores %>% filter(age_category == "1")

#df_6_9_pop <- df_scores %>% filter(age_category == "2")

# Clustering df

variables <- c("zasq", "zdass_d", "zdass_a", "zdass_s", "zwho", "zisel_a", "zisel_b", "zisel_t", "ztips", "zmiri")
df_clust <- df_scores[variables]

#df_clust1 <- df_3_6_pop[variables]
#df_clust2 <- df_6_9_pop[variables]




#Step 5 - Alphas
library(ltm)

# Subset scoring (alphas)
##***Fix these to be the right codes
DASS_full <- df_scores %>% dplyr::select(DASS_1:DASS_21)
DASS_stress <- df_scores %>% dplyr::select(DASS_1, DASS_6, DASS_8, DASS_11, DASS_12, DASS_14, DASS_18)
DASS_anxiety <- df_scores %>% dplyr::select(DASS_2, DASS_4, DASS_7, DASS_9, DASS_15, DASS_19, DASS_20)
DASS_depression <- df_scores %>% dplyr::select(DASS_3, DASS_5, DASS_10, DASS_13, DASS_16, DASS_17, DASS_21)

ISEL_full <- df_scores %>% dplyr::select(ISEL_1:ISEL_12)
ISEL_appraisal <- df_scores %>% dplyr::select(ISEL_2, ISEL_4, ISEL_6, ISEL_11)
ISEL_belonging <- df_scores %>% dplyr::select(ISEL_1, ISEL_5, ISEL_7, ISEL_9)
ISEL_tangible <- df_scores %>% dplyr::select(ISEL_3, ISEL_8, ISEL_10, ISEL_12)

TIPS <- df_scores %>% dplyr::select(TIPS_1:TIPS_14)

#Cronbach's alpha
cronbach.alpha(DASS_full)
cronbach.alpha(DASS_stress)
cronbach.alpha(DASS_anxiety)
cronbach.alpha(DASS_depression)
cronbach.alpha(asq)
cronbach.alpha(who)
cronbach.alpha(ISEL_full)
cronbach.alpha(ISEL_appraisal)
cronbach.alpha(ISEL_belonging)
cronbach.alpha(ISEL_tangible)
cronbach.alpha(TIPS)
cronbach.alpha(miri)



#Step 6 - hierarchical cluster analysis

library(ggplot2)
library(factoextra)
library(cluster)

distance_matrix <- dist(df_clust, method = 'euclidean')

hcluster <- hclust(distance_matrix, method = "ward.D2")

fviz_dend(hcluster, rect = TRUE, cex = 0.7, k = 3)

dend_plot <- fviz_dend(
  hcluster, rect = TRUE, cex = 0.7, k = 3, show_labels = FALSE, color_labels_by_k = TRUE
)


#Step 7 - K Means cluster analysis

set.seed(1984)

kmeans <- kmeans(df_clust, 3, nstart = 100)

#table includes centres & size of each cluster
kmeans_table <- data.frame(kmeans$size, kmeans$centers)

kmeans_df <- data.frame(Cluster = kmeans$cluster,
                        df_clust)

head(kmeans_df)


#Descriptives for clusters
df_inf <- kmeans_df %>% filter(Cluster == "3")
summary(df_inf)

df_mother <- kmeans_df %>% filter(Cluster == "1")
summary(df_mother)

df_low <- kmeans_df %>% filter(Cluster == "2")
summary(df_low)


#Step 8 - ANOVA table

df_clust$cluster <- as.factor(kmeans$cluster)

anova_results_miri <- aov(zmiri ~ cluster, data = df_clust)
summary(anova_results_miri)

anova_results_tips <- aov(ztips ~ cluster, data = df_clust)
summary(anova_results_tips)

anova_results_asq <- aov(zasq ~ cluster, data = df_clust)
summary(anova_results_asq)

anova_results_dass_d <- aov(zdass_d ~ cluster, data = df_clust)
summary(anova_results_dass_d)

anova_results_dass_a <- aov(zdass_a ~ cluster, data = df_clust)
summary(anova_results_dass_a)

anova_results_dass_s <- aov(zdass_s ~ cluster, data = df_clust)
summary(anova_results_dass_s)

anova_results_who <- aov(zwho ~ cluster, data = df_clust)
summary(anova_results_who)

anova_results_isel_a <- aov(zisel_a ~ cluster, data = df_clust)
summary(anova_results_isel_a)

anova_results_isel_b <- aov(zisel_b ~ cluster, data = df_clust)
summary(anova_results_isel_b)

anova_results_isel_t <- aov(zisel_t ~ cluster, data = df_clust)
summary(anova_results_isel_t)




#Step 9 - Post Hoc Tukeys test

tukey_result_miri <- TukeyHSD(anova_results_miri)
print(tukey_result_miri)

tukey_result_tips <- TukeyHSD(anova_results_tips)
print(tukey_result_tips)

tukey_result_asq <- TukeyHSD(anova_results_asq)
print(tukey_result_asq)

tukey_result_dass_d <- TukeyHSD(anova_results_dass_d)
print(tukey_result_dass_d)

tukey_result_dass_a <- TukeyHSD(anova_results_dass_a)
print(tukey_result_dass_a)

tukey_result_dass_s <- TukeyHSD(anova_results_dass_s)
print(tukey_result_dass_s)

tukey_result_who <- TukeyHSD(anova_results_who)
print(tukey_result_who)

tukey_result_isel_a <- TukeyHSD(anova_results_isel_a)
print(tukey_result_isel_a)

tukey_result_isel_b <- TukeyHSD(anova_results_isel_b)
print(tukey_result_isel_b)

tukey_result_isel_t <- TukeyHSD(anova_results_isel_t)
print(tukey_result_isel_t)



#Step 10 - Visualising k-means clusters
library(ggplot2)
library(tidyr)
library(dplyr)

#Final cluster centers (means) for variables in cluster profiles
kmeans_table$Cluster <- 1:nrow(kmeans_table)

subset_df <- kmeans_table[, 2:12]

#creating scale
reshaped_data <- gather(subset_df, key = "Variable", value = "Value", -Cluster)

reshaped_data$Variable <- factor(reshaped_data$Variable, levels = c("zmiri", "ztips", "zasq", "zdass_d", "zdass_a", "zdass_s", "zwho", "zisel_a", "zisel_b", "zisel_t"))

variable_labels <- c("MIRI", "TIPS", "ASQ:SE-2", "DASS-21 (D)", "DASS-21 (A)", "DASS-21 (S)", "WHO-5", "ISEL-12 (A)", 
                     "ISEL-12 (B)", "ISEL-12 (T)")

reshaped_data$Cluster <- factor(reshaped_data$Cluster, levels = c(3, 1, 2))
cluster_labels <- c("Mother at risk", "Low risk", "Infant at risk")
reshaped_data$Cluster <- recode(reshaped_data$Cluster, !!!setNames(cluster_labels, 1:3))

max_value <- max(reshaped_data$Value)
max_display <- max(2, ceiling(max_value))
min_value <- min(reshaped_data$Value)
min_display <- min(-2, ceiling(min_value))

ggplot(reshaped_data, aes(x = Variable, y = Value, color = as.factor(Cluster), group = Cluster)) +
  geom_line() +
  geom_point() +
  labs(x = "Variables", y = "Values", color = "Cluster", title = "Final Cluster Centres") +
  scale_x_discrete(labels = c("MIRI", "TIPS", "ASQ:SE-2", "DASS-21 (D)", "DASS-21 (A)", "DASS-21 (S)", "WHO-5", "ISEL-12 (A)", 
                              "ISEL-12 (B)", "ISEL-12 (T)")) +
  scale_y_continuous(breaks = seq(min_display, max_display, by = 1),
                     labels = seq(min_display, max_display, by = 1)) +
  theme_minimal()



#Clusters with ellipses (cluster centers)
fviz_cluster(kmeans, data = df_clust, geom = c("point"), ellipse.type = "euclid")


#Cluster with variables distributions
data_for_plot <- cbind(kmeans$cluster, df_clust[variables])

colnames(data_for_plot)[1] <- "Cluster"

data_for_plot_long <- gather(data_for_plot, Variable, Value, -Cluster)

data_for_plot_long$Variable <- factor(data_for_plot_long$Variable, levels = c("zmiri", "ztips", "zasq", "zdass_d", "zdass_a", "zdass_s", "zwho", "zisel_a", "zisel_b", "zisel_t"))
data_for_plot_long$Cluster <- factor(data_for_plot_long$Cluster, levels = c(3, 1, 2))
cluster_labels <- c("Mother at risk", "Low risk", "Infant at risk")
data_for_plot_long$Cluster <- recode(data_for_plot_long$Cluster, !!!setNames(cluster_labels, 1:3))
variable_labels <- c("MIRI", "TIPS", "ASQ:SE-2", "DASS-21 (D)", "DASS-21 (A)", "DASS-21 (S)", "WHO-5", "ISEL-12 (A)", 
                     "ISEL-12 (B)", "ISEL-12 (T)")


ggplot(data_for_plot_long, aes(x = factor(Cluster), y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Variable Distribution within Clusters", x = "Cluster", y = "z-score",) +
  scale_fill_discrete(labels = variable_labels) +
  theme_minimal()



#Principle component analysis

pca_result <- prcomp(kmeans_df)

kmeans$cluster <- factor(kmeans$cluster, levels = c(3, 1, 2))
custom_colors <- c("3" = "red", "1" = "#2ECC71", "2" = "#4DA6FF")
custom_labels <- c("Infant at risk", "Mother at risk", "Low risk")
  
ggplot(data.frame(pca_result$x), aes(x = PC1, y = PC2, color = as.factor(kmeans$cluster))) +
  geom_point() +
  labs(title = "PCA transformed data", color = "Cluster") +
  scale_color_manual(values = custom_colors, labels = custom_labels)
 



#Calculate silhouette score

dist_matrix <- dist(kmeans_df)

silhouette_scores <- silhouette(kmeans$cluster, dist_matrix)

average_silhouette <- mean(silhouette_scores[, "sil_width"])

cat("Average Silhouette Score:", average_silhouette, "\n")



#MISC
##library(writexl)
##output_path <- "C:\\Users\\lisag\\OneDrive - University of Edinburgh\\Survey\\Data Analysis\\New analysis May_June 2023\\df_2.xlsx"
##write_xlsx(df_scores, path = output_path)