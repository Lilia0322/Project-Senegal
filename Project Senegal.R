# Install necessary packages
install.packages(c(
  "dplyr", "tidyr", "table1", "flextable", "magrittr", "stargazer", "lmtest", 
  "plm", "car", "leaps", "fixest", "modelsummary", "kableExtra", "margins", 
  "ggplot2", "NbClust", "cluster", "factoextra", "fpc", "clValid", "ggpubr", 
  "epiDisplay"
))

# Load required libraries
library(dplyr)
library(tidyr)
library(table1)
library(flextable)
library(magrittr)
library(stargazer)
library(lmtest)
library(plm)
library(car)
library(leaps)
library(fixest)
library(modelsummary)
library(kableExtra)
library(margins)
library(ggplot2)
library(NbClust)
library(cluster)
library(factoextra)
library(fpc)
library(clValid)
library(ggpubr)
library(epiDisplay)
library(readxl)

# Load data
shocks_final <- read_excel("Documents/Bewerbungen/AA/shocks_final_data1.xlsx")

# Hierarchical Cluster Analysis (HCA)
# Note: Fixed mix-ups from sm_q3 and sm_q4 in session 3 and session 26
cluster.data <- shocks_final %>%
  dplyr::select(Session_ID, Participant_ID, sm_q2:sm_q8) %>%
  unique()

# Handle missing data: Removing participant 8 from session 22
cluster.data <- cluster.data[-278,]

# Determine the optimal number of clusters
clusternum <- NbClust(cluster.data, distance = "euclidean", method = "kmeans")
pam.res2 <- pam(cluster.data, 2, metric = "euclidean", stand = FALSE)

# Visualize clusters
fviz_cluster(pam.res2, data = cluster.data, palette = c("blue", "yellow", "green", "red", "brown", "orange"),
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE, ggtheme = theme_minimal())
width <- fviz_silhouette(pam.res2, palette = "jco", ggtheme = theme_classic())
print(width)

# Silhouette Analysis for Optimal Number of Clusters
silh <- fviz_nbclust(cluster.data[, 3:9], kmeans, method = 'silhouette')
print(silh)

# Apply k-means clustering with 2 clusters
k2 <- kmeans(cluster.data[, 3:9], centers = 2, nstart = 20)
fviz_cluster(k2, data = cluster.data[, 3:9])

# Assign clusters to data
cluster.data$cluster <- k2$cluster

# Frequency and percentage of clusters
tab1(cluster.data$cluster, sort.group = "decreasing", cum.percent = TRUE)

# Calculate and plot average contributions by cluster
plot <- cluster.data %>%
  group_by(cluster) %>%
  summarise(across(sm_q2:sm_q8, mean))

plot2 <- plot %>%
  pivot_longer(cols = sm_q2:sm_q8, names_to = "type", values_to = "contributions") %>%
  mutate(contributions = round(contributions, 0), cluster = as.factor(cluster))

SM <- c("0", "1000", "1500", "2500", "3000", "4000", "5000")

# Plot average contributions
ggplot(plot2, aes(x = type, y = contributions, colour = cluster, group = cluster)) +
  geom_point() +
  geom_line() +
  ylab("Average contributions to the public fund") +
  xlab("Contribution of the other player to the public fund") +
  scale_x_discrete(labels = SM) +
  scale_y_continuous(limits = c(0, 5000)) +
  scale_colour_grey(name = "Cluster", labels = c("1 (60.5%)", "2 (39.5%)")) +
  geom_text(aes(label = contributions, vjust = "inward", hjust = "inward", show.legend = FALSE), colour = "black", size = 3.5) +
  labs(fill = "Cluster", caption = "Note: 1 = Conditional cooperator , 2 = Weak conditional cooperator") +
  theme_minimal()

# Basic Data Exploration and Outlier Handling
boxplot(shocks_final$cooperation)

# Identify and filter outliers
outliers <- shocks_final %>%
  dplyr::select(Environment, Session_ID, Participant_ID, Round, cooperation, invalid) %>%
  filter(Round <= 5) %>%
  na.omit() %>%
  mutate(fail = ifelse(cooperation %in% c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000), "exclude", "include"))

# Create datasets for analysis
main <- shocks_final %>%
  dplyr::filter(invalid == 0 & Round <= 5) %>%
  rename(Village = villageID) %>%
  dplyr::select(Environment, Village, Location, Time, Session_ID, Participant_ID, Round, Shock_Treatment, cooperation_sh, 
                cooperation, ids_treat, cov_treat, age, old, young, woman, hhsize, hhhead, illiterate, education_french, 
                education_arabic, education, literacy, agro_pasteur, water_impr, past_impr, exp_ids, dum_ids, exp_cov, 
                cov_econ, cov_clim, cov_pol, shock_cov_epid, trust_card, social_cap, cluster, cluster_high, sm_q1, 
                risk_seeking, risk_averse, Control_mistake, attrition_survey, groupsize, Time, shock_prior, round1, 
                coop_shocks, sc10_collab)

# Transform and generate new variables
main <- main %>%
  mutate(belief_sh = sm_q1 / 5000) %>%
  group_by(Session_ID, Participant_ID) %>%
  mutate(Round1 = cooperation[Round == 1][1]) %>%
  ungroup()

# Average contributions over specific rounds
main_treat <- main %>%
  filter(Round > 1 & Round <= 5) %>%
  group_by(Participant_ID, Session_ID) %>%
  mutate(avg_cooperation_sh = mean(cooperation_sh),
         avg_coop_shocks = mean(coop_shocks)) %>%
  dplyr::select(Environment, Village, Location, Time, Session_ID, Participant_ID, Shock_Treatment, ids_treat, cov_treat, avg_cooperation_sh, 
         age, old, young, woman, hhsize, hhhead, illiterate, education_french, education_arabic, education, literacy, agro_pasteur, 
         water_impr, past_impr, exp_ids, dum_ids, exp_cov, cov_econ, cov_clim, cov_pol, shock_cov_epid, trust_card, social_cap, 
         cluster, cluster_high, sm_q1, belief_sh, risk_seeking, risk_averse, Control_mistake, attrition_survey, groupsize, Time, 
         Round1, avg_coop_shocks, sc10_collab) %>%
  unique() %>%
  mutate(avg_cooperation_sh = round(avg_cooperation_sh, 2),
         avg_coop_shocks = round(avg_coop_shocks, 2))

# Create labels for the output table
cm <- c(
  '(Intercept)' = 'Constant',
  'ids_treat' = 'Idiosyncratic shock treatment',
  'cov_treat' = 'Covariate shock treatment',
  'age' = 'Age',
  'old' = 'Age > 65',
  'young' = 'Age < 25',
  'woman' = 'Female',
  'hhsize' = 'Number of household members',
  'hhhead' = 'Household head',
  'illiterate' = 'Illiterate',
  'literacy' = 'Literate',
  'education_french' = 'Years of schooling (French)',
  'education_arabic' = 'Years of schooling (Arabic)',
  'education' = 'Years of education (French or Arabic)',
  'agro_pasteur' = 'Agro-pastoralist',
  'water_impr' = 'Improved access to water',
  'past_impr' = 'Improved access to pastures',
  'exp_ids' = 'Idiosyncratic shocks index (0-1)',
  'exp_cov' = 'Covariate shocks index (0-1)',
  'cov_clim' = 'Experienced climate shock',
  'cov_econ' = 'Experienced economic shock',
  'cov_pol' = 'Experienced political shock',
  'cluster' = 'Weak unconditional cooperator',
  'cluster_high' = 'Strong unconditional cooperator',
  'belief_sh' = "Beliefs about others' contribution",
  'risk_seeking' = 'Risk-seeking',
  'risk_averse' = 'Risk aversion',
  'cov_treat:cluster' = 'Interaction cluster-cov',
  'cov_treat:belief_sh' = 'Interaction belief-cov',
  'cov_treat:risk_averse' = 'Interaction cov-risk',
  'Control_mistake1' = 'Confusion',
  'trust_card' = 'Trust',
  'social_cap' = 'Expected help of neighbors in contributing to community task',
  'Round1' = 'Cooperation baseline round',
  'groupsize' = 'Session size',
  'Round' = 'Round',
  'shock_prior' = 'Shock previous round',
  'EnvironmentThiel' = 'Thiel',
  'cov_treat:EnvironmentThiel' = 'Covariate shock treatment*Thiel'
)

# Regression analysis
model_5_fec <- feols(avg_cooperation_sh ~ cov_treat + Round1 + age + woman + hhsize + education + cov_econ + cov_clim + cluster_high + belief_sh + risk_averse + trust_card + social_cap + groupsize + Environment | Village, vcov = ~ Village, data = main_treat)
model_5_feh <- feols(avg_cooperation_sh ~ cov_treat + Round1 + age + woman + hhsize + education + cov_econ + cov_clim + cluster_high + belief_sh + risk_averse + trust_card + social_cap + groupsize + Environment, vcov = ~ Village, data = main_treat)
model_5_ols <- feols(avg_cooperation_sh ~ cov_treat + Round1 + age + woman + hhsize + education + cov_econ + cov_clim + cluster_high + belief_sh + risk_averse + trust_card + social_cap + groupsize + Environment, data = main_treat)

# Display models with custom formatting
Model <- modelsummary(list(model_5_ols, model_5_feh, model_5_fec), coef_map = cm, stars = TRUE, gof_omit = "R2 Within|RMSE|IC")
Model %>%
  pack_rows("Treatment", 3, 4) %>%
  pack_rows("Socio-demographics", 5, 12) %>%
  pack_rows("Shocks", 13, 16) %>%
  pack_rows("Individual preferences", 17, 22) %>%
  pack_rows("Social capital", 23, 26) %>%
  pack_rows("Session", 27, 31) %>%
  add_header_above(c("", "OLS" = 2, "FE" = 1)) %>%
  add_header_above(c("", "Cooperation" = 3))

# Sensitivity analysis across different rounds
rfec <- feols(cooperation_sh ~ cov_treat + Round1 + age + woman + hhsize + education + cov_econ + cov_clim + cluster_high + belief_sh + risk_averse + trust_card + social_cap + groupsize + Round + Environment | Village, vcov = ~ Village, data = main_treat_rounds)
rolsc <- feols(cooperation_sh ~ cov_treat + Round1 + age + woman + hhsize + education + cov_econ + cov_clim + cluster_high + belief_sh + risk_averse + trust_card + social_cap + groupsize + Round + Environment, vcov = ~ Village, data = main_treat_rounds)
rols <- feols(cooperation_sh ~ cov_treat + Round1 + age + woman + hhsize + education + cov_econ + cov_clim + cluster_high + belief_sh + risk_averse + trust_card + social_cap + groupsize + Round + Environment, data = main_treat_rounds)

# Display sensitivity models with custom formatting
Model <- modelsummary(list(rolsc, rolsc, rfec), coef_map = cm, stars = TRUE, gof_omit = "R2 Within|RMSE|IC")
Model
