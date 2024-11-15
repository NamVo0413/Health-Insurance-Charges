#Loading libraby if needed
install.packages("dplyr")
install.packages("stats")
install.packages("Hmisc")
install.packages("dunn.test")
library(dplyr)
library(stats)
library(Hmisc)
library(dunn.test)
#Question 01: Visualise the distributions of each variable and provide their summary statistics. # nolint
#Get path of current script
current_path <- getwd()
#Set path for current script
setwd(current_path)
#Read file Health-Insurance-Dataset.csv
dataset <- read.csv("Health-Insurance-Dataset.csv")
#Attach dataset for later using
attach(dataset)
#Summary Statistic for dataset
summary(dataset)
sd(age)
#Result: 14.04996
sd(bmi)
#Result: 6.098187
sd(children)
#Result: 1.205493
sd(charges)
#Result: 12110.01
#Result
#       age            sex                 bmi           children
#  Min.   :18.00   Length:1338        Min.   :15.96   Min.   :0.000
#  1st Qu.:27.00   Class :character   1st Qu.:26.30   1st Qu.:0.000
#  Median :39.00   Mode  :character   Median :30.40   Median :1.000   # nolint
#  Mean   :39.21                      Mean   :30.66   Mean   :1.095
#  3rd Qu.:51.00                      3rd Qu.:34.69   3rd Qu.:2.000
#  Max.   :64.00                      Max.   :53.13   Max.   :5.000
#     smoker             region             charges      # nolint
#  Length:1338        Length:1338        Min.   : 1122
#  Class :character   Class :character   1st Qu.: 4740
#  Mode  :character   Mode  :character   Median : 9382
#                                        Mean   :13270
#                                        3rd Qu.:16640   # nolint
#                                        Max.   :63770

#Check if the dataset have any missing data
#Check for missing data
missing_data <- any(is.na(dataset))
if(missing_data) {
  print("There is missing data.")
} else {
  print("There is no missing data.")
}
#Result
#"There is no missing data."

#Visualize the distribution of the dataset
#Create a png file to make it easier insert into report
png("CourseWorkDistribution.png", width = 800, height = 600)
#Set size for the visualization
par(mfrow = c(3, 3))
#Add chart
#For numeric data, the best option to visualize the distribution is hist chart
hist(age, main = "Age Distribution", xlab = "Age")
hist(bmi, main = "BMI Distribution", xlab = "BMI")
hist(children, main = "Children Distribution", xlab = "Number of Children")
hist(charges, main = "Charges Distribution", xlab = "Charges")
#For categorical data, the best to visualize the distribution is bar chart
barplot(table(sex), main = "Sex Distribution", xlab = "Sex")
barplot(table(smoker), main = "Smoker Distribution", xlab = "Smoker")
barplot(table(region), main = "Region", xlab = "Region")
#Disconnect the generating file
dev.off()
#Check if all the numeric data is normally distributed
#Using Shapiro-Wilk test for the tesing
#Null Hypothesis: The data is normally distributed
#Significant Level: 0.05
#Testing: Age
ks.test(age, "pnorm", mean = mean(age), sd = sd(age))
#Result
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  age
# D = 0.078945, p-value = 1.143e-07
# alternative hypothesis: two-sided
#Conclusion -> Reject Null Hyothesis -> Age not normally distributed
ks.test(bmi, "pnorm", mean = mean(bmi), sd = sd(bmi))
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  bmi
# D = 0.0261, p-value = 0.3218
# alternative hypothesis: two-sided
#Conclusion -> Reject Null Hyothesis -> BMI not normally distributed
ks.test(children, "pnorm", mean = mean(children), sd = sd(children))
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  children
# D = 0.24713, p-value < 2.2e-16
# alternative hypothesis: two-sided
#Conclusion -> Reject Null Hyothesis -> Children not normally distributed
ks.test(charges, "pnorm", mean = mean(charges), sd = sd(charges))
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  charges
# D = 0.18846, p-value < 2.2e-16
# alternative hypothesis: two-sided
#Conclusion -> Reject Null Hyothesis -> Charges not normally distributed

#Question 02: test the assumption that all predictor variables are independent using correlation coefficients and # nolint
#Visualise your results using scatter plots and a correlation matrix.
#Create test sample that contain all independent variables.
predictor_variables <- dataset[, c("age", "bmi", "children", "charges")]
#Correlation test to check the hypothesis
cor.test(age, bmi, method = "pearson")
#Result
# Pearson's product-moment correlation
# data:  age and bmi
# t = 4.0181, df = 1336, p-value = 6.194e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.05600895 0.16191463
# sample estimates:
#       cor
# 0.1092719
cor.test(age, children, method = "spearman")
#Result
#         Spearman's rank correlation rho
# data:  age and children
# S = 376471515, p-value = 0.03712
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho
# 0.05699222
# Warning message:
# In cor.test.default(age, children, method = "spearman") :
#   Cannot compute exact p-value with ties
cor.test(age, charges, method = "spearman")
#Result
# Spearman's rank correlation rho
# data:  age and charges
# S = 185881923, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho
# 0.5343921
# Warning message:
# In cor.test.default(age, charges, method = "spearman") :
#   Cannot compute exact p-value with ties
cor.test(bmi, children, method = "pearson")
#Result
# Pearson's product-moment correlation
# data:  bmi and children
# t = 0.46639, df = 1336, p-value = 0.641
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.04085995  0.06630448
# sample estimates:
#       cor
# 0.0127589
cor.test(bmi, charges, method = "pearson")
#Result
# Pearson's product-moment correlation
# data:  bmi and charges
# t = 7.3966, df = 1336, p-value = 2.459e-13
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1463052 0.2492822
# sample estimates:
#      cor
# 0.198341
cor.test(children, charges, method = "spearman")
#Result
# Spearman's rank correlation rho
# data:  children and charges
# S = 345992058, p-value = 9.847e-07
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho
# 0.1333389
# Warning message:
# In cor.test.default(children, charges, method = "spearman") :
#   Cannot compute exact p-value with ties
#Visulize the correlation into scatter plots
png("CourseWorkScatterplot.png", width = 800, height = 600)
par(mfrow = c(2, 3))
plot(age, bmi, xlab = "Age", ylab = "BMI", main = "Age vs BMI")
plot(age, children, xlab = "Age", ylab = "Children", main = "Age vs Children")
plot(age, charges, xlab = "Age", ylab = "Charges", main = "Age vs Charges")
plot(bmi, children, xlab = "BMI", ylab = "Children", main = "BMI vs Children")
plot(bmi, charges, xlab = "BMI", ylab = "Charges", main = "BMI vs Charges")
plot(children, charges, xlab = "Children", ylab = "Charges", main = "Children vs Charges")
#Disconnect the generating file
dev.off()

#Question 3: Using linear regression modelling, describe the variables which you believe  # nolint
#have influence on charge and describe your model with coefficients
#Using linear regression model
lm_model_age <- lm(charges ~ age)
summary(lm_model_age)
#Result
# Residuals:
#    Min     1Q Median     3Q    Max
#  -7617  -6914  -6169   8555  29437
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)   3107.3     7176.1   0.433    0.670
# age            261.8      186.9   1.401    0.178
# Residual standard error: 11820 on 18 degrees of freedom
# Multiple R-squared:  0.09829,   Adjusted R-squared:  0.04819
# F-statistic: 1.962 on 1 and 18 DF,  p-value: 0.1783
lm_model_region <- lm(charges ~ region)
summary(lm_model_region)
#Result
# Residuals:
#    Min     1Q Median     3Q    Max
# -12086  -9536  -2927   5531  25800 

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)
# (Intercept)         5580       6303   0.885    0.389
# regionnorthwest     9934       8914   1.114    0.282
# regionsoutheast     8232       7902   1.042    0.313
# regionsouthwest     8018       8457   0.948    0.357

# Residual standard error: 12610 on 16 degrees of freedom
# Multiple R-squared:  0.08818,   Adjusted R-squared:  -0.08279 
# F-statistic: 0.5158 on 3 and 16 DF,  p-value: 0.6773
lm_model_bmi <- lm(charges ~ bmi)
summary(lm_model_bmi)
#Result
# Residuals:
#    Min     1Q Median     3Q    Max 
# -12516  -7542  -5807   7724  22044

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  -2889.6    15127.1  -0.191    0.851
# bmi            500.9      486.0   1.031    0.316

# Residual standard error: 12100 on 18 degrees of freedom
# Multiple R-squared:  0.05574,   Adjusted R-squared:  0.003283
# F-statistic: 1.063 on 1 and 18 DF,  p-value: 0.3163
lm_model_children <- lm(charges ~ children)
summary(lm_model_children)
#Result
# Residuals:
#    Min     1Q Median     3Q    Max 
# -13141  -9563  -1356   5423  24644

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)    14968       3073   4.871 0.000123 ***
# children       -4193       2695  -1.556 0.137217
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 11690 on 18 degrees of freedom
# Multiple R-squared:  0.1185,    Adjusted R-squared:  0.06953 
# F-statistic:  2.42 on 1 and 18 DF,  p-value: 0.1372
lm_model_smoker <- lm(charges ~ smoker)
summary(lm_model_smoker)
#Result
# Residuals:
#    Min     1Q Median     3Q    Max 
# -13401  -5354  -2032   2877  20929

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)     7994       2040   3.918 0.001007 **  # nolint
# smokeryes      22292       4562   4.887 0.000119 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 8160 on 18 degrees of freedom
# Multiple R-squared:  0.5702,    Adjusted R-squared:  0.5463  # nolint
# F-statistic: 23.88 on 1 and 18 DF,  p-value: 0.0001187
lm_model_sex <- lm(charges ~ sex)
summary(lm_model_sex)
#Result
# Residuals:
#    Min     1Q Median     3Q    Max  # nolint
# -10591  -8549  -5445   4602  28423

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    # nolint
# (Intercept)    14348       4363   3.289  0.00408 **
# sexmale        -3159       5632  -0.561  0.58178
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 12340 on 18 degrees of freedom
# Multiple R-squared:  0.01718,   Adjusted R-squared:  -0.03742  # nolint
# F-statistic: 0.3146 on 1 and 18 DF,  p-value: 0.5818

#Question 04
#Split the data into two group male and female
group1 <- dataset %>% filter(sex == "male")
#Result
#   age  sex    bmi children smoker    region   charges
# 1  18 male 33.770        1     no southeast  1725.552
# 2  28 male 33.000        3     no southeast  4449.462
# 3  33 male 22.705        0     no northwest 21984.471
# 4  32 male 28.880        0     no northwest  3866.855
# 5  37 male 29.830        2     no northeast  6406.411
# 6  25 male 26.220        0     no northeast  2721.321
group2 <- dataset %>% filter(sex == "female")
#Result
#   age    sex   bmi children smoker    region   charges
# 1  19 female 27.90        0    yes southwest 16884.924
# 2  31 female 25.74        0     no southeast  3756.622
# 3  46 female 33.44        1     no southeast  8240.590
# 4  37 female 27.74        3     no northwest  7281.506
# 5  60 female 25.84        0     no northwest 28923.137
# 6  62 female 26.29        0    yes southeast 27808.725
#Doing Kolmogorov-Smirnov test for normality in two group
ks.test(group1$age, "pnorm", mean = mean(group1$age), sd = sd(group1$age))
#Result
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  group1$age
# D = 0.080241, p-value = 0.0003315
# alternative hypothesis: two-sided
ks.test(group1$bmi, "pnorm", mean = mean(group1$bmi), sd = sd(group1$bmi))
#Result
#Asymptotic one-sample Kolmogorov-Smirnov test
# data:  group1$bmi
# D = 0.026616, p-value = 0.7245
# alternative hypothesis: two-sided
ks.test(group2$age, "pnorm", mean = mean(group2$age), sd = sd(group2$age))
#Result
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  group2$age
# D = 0.077445, p-value = 0.0007117
# alternative hypothesis: two-sided
ks.test(group2$bmi, "pnorm", mean = mean(group2$bmi), sd = sd(group2$bmi))
#Result
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  group2$bmi
# D = 0.034127, p-value = 0.4237
# alternative hypothesis: two-sided
#Visualize the qq plot for both group
par(mfrow = c(2, 2))
qqnorm(group1$age, main = "QQ plot for age of group male")
qqline(group1$age)
qqnorm(group1$bmi, main = "QQ plot for bmi of group male")
qqline(group1$bmi)
qqnorm(group2$age, main = "QQ plot for age of group female")
qqline(group2$age)
qqnorm(group2$bmi, main = "QQ plot for bmi of group female")
qqline(group2$bmi)
#Create a new categorical variable CHARGE_split
group1$CHARGE_split <- "Group Male"
group2$CHARGE_split <- "Group Female"
#Create a new variable to store the Combined the two group back
combined_data <- rbind(group1,group2)
#Result
# age  sex    bmi children smoker    region   charges CHARGE_split
# 1  18 male 33.770        1     no southeast  1725.552   Group Male
# 2  28 male 33.000        3     no southeast  4449.462   Group Male
# 3  33 male 22.705        0     no northwest 21984.471   Group Male
# 4  32 male 28.880        0     no northwest  3866.855   Group Male
# 5  37 male 29.830        2     no northeast  6406.411   Group Male
# 6  25 male 26.220        0     no northeast  2721.321   Group Male
#Create a list of predictive numerical variable for t-test and Mann-Whitney U test. # nolint
numerical_vars <- c("age", "bmi")
#Mann-Whitney U test for age
wilcox.test(combined_data$age ~ combined_data$CHARGE_split)
#Result
# Wilcoxon rank sum test with continuity correction
# data:  combined_data$age by combined_data$CHARGE_split
# W = 229131, p-value = 0.4468
# alternative hypothesis: true location shift is not equal to 0
# T-test
t.test(combined_data$bmi ~ combined_data$CHARGE_split)
#Result
# Welch Two Sample t-test
# data:  combined_data$bmi by combined_data$CHARGE_split
# t = -1.697, df = 1336, p-value = 0.08992
# alternative hypothesis: true difference in means between group Group Female and group Group Male is not equal to 0 # nolint
# 95 percent confidence interval:
#  -1.21895043  0.08819153
# sample estimates:
# mean in group Group Female   mean in group Group Male
#                   30.37775                   30.94313
#Create a list categorical variable for Chi-square test
categorical_vars <- c("smoker", "region")
chi_square_results <- lapply(categorical_vars, function(var) {
  contingency_table <- table(combined_data[[var]], combined_data$CHARGE_split)
  chi_square_test <- chisq.test(contingency_table)
  return(chi_square_test)
})
names(chi_square_results) <- categorical_vars
#Result
# $smoker
# Pearson's Chi-squared test with Yates' continuity correction
# data:  contingency_table
# X-squared = 7.3929, df = 1, p-value = 0.006548
# $region
# Pearson's Chi-squared test
# data:  contingency_table
# X-squared = 0.43514, df = 3, p-value = 0.9329
#Visualize the result
png("CourseWorkStatistic.png", width = 800, height = 600)
#Numerical variable
par(mfrow = c(2, 2))
for (i in 1:length(numerical_vars)) {
  boxplot(combined_data[[numerical_vars[i]]] ~ combined_data$CHARGE_split,
          main = paste("Boxplot of", numerical_vars[i], "by Group"),
          xlab = "Group", ylab = numerical_vars[i], col = c("lightblue", "lightgreen"))
}
barplot(table(combined_data[[categorical_vars[1]]], combined_data$CHARGE_split), 
          beside = TRUE, 
          main = paste("Barplot of", categorical_vars[1], "by Group"), legend.text=TRUE,
          xlab = categorical_vars[2], ylab = "Frequency", col = c("lightblue", "lightgreen"))
barplot(table(combined_data[[categorical_vars[2]]], combined_data$CHARGE_split), 
          beside = TRUE, 
          main = paste("Barplot of", categorical_vars[2], "by Group"), legend.text=TRUE,
          xlab = categorical_vars[2], ylab = "Frequency", col = c("lightblue", "lightgreen", "red", "black"))
#Disconnect the generating file
dev.off()
par(mfrow = c(1, 1))

#Question 5: assess differences in central tendencies of the interval predictor variables  # nolint
#(eg BMI, Age) with respect to the geography. Perform additional post-hoc tests.
#Anova testing for BMI
anova_model <- aov(bmi ~ region, data = dataset)
anova_results <- summary(anova_model)
#Result
#               Df Sum Sq Mean Sq F value Pr(>F)
# region         3   4056  1352.0   39.49 <2e-16 ***
# Residuals   1334  45664    34.2
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Post-hoc test (Turkey HSD test)
tukey_results <- TukeyHSD(anova_model)
#Result
# Tukey multiple comparisons of means
#     95% family-wise confidence level
# $region
#                            diff        lwr       upr     p adj
# northwest-northeast  0.02628153 -1.1552239  1.207787 0.9999328
# southeast-northeast  4.18248592  3.0330135  5.331958 0.0000000
# southwest-northeast  1.42311230  0.2416069  2.604618 0.0106965
# southeast-northwest  4.15620440  3.0076679  5.304741 0.0000000
# southwest-northwest  1.39683077  0.2162360  2.577426 0.0127393
# southwest-southeast -2.75937363 -3.9079101 -1.610837 0.0000000
# Kruskal-Wallis test for Age
kruskal_test <- kruskal.test(age ~ region, data = dataset)
#Result
# Kruskal-Wallis rank sum test
# data:  age by region
# Kruskal-Wallis chi-squared = 0.41382, df = 3, p-value = 0.9374
#Post-hoc test (Dunn test)
dunn_results <- dunn.test(age, g = region, method = "bonferroni")
#Result
# $chi2
# [1] 0.4138157
# $Z
# [1] -0.06367983  0.32353394  0.38930537 -0.30268141 -0.23918593 -0.63516753
# $P
# [1] 0.4746126 0.3731455 0.3485251 0.3810663 0.4054807 0.2626596
# $P.adjusted
# [1] 1 1 1 1 1 1
# $comparisons
# [1] "northeast - northwest" "northeast - southeast" "northwest - southeast"
# [4] "northeast - southwest" "northwest - southwest" "southeast - southwest"
#Detach
detach(dataset)