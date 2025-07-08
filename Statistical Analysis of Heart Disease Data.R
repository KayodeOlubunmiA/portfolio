#load library tidyverse
library(tidyverse)


#load dataset
df1 <- read_csv("C:/Users/hassa/Downloads/Portfolio datasets/heartdx_data.csv")

#create a copy of the dataset
df <-df1

#lets have a look at our dataset
head(df)
dim(df)
glimpse(df)
str(df)
summary(df)


#are there any missing values
sum(is.na(df))


#lets check for duplicate
sum(duplicated(df))


#convert categorical variables from character to factor
df$Sex <- as.factor(df$Sex)
df$ChestPainType <- as.factor(df$ChestPainType)
df$RestingECG <- as.factor(df$RestingECG)
df$ExerciseAngina <- as.factor(df$ExerciseAngina)
df$ST_Slope <- as.factor(df$ST_Slope)
df$FastingBS  <- as.factor(df$FastingBS)
df$HeartDisease  <- as.factor(df$HeartDisease)

#rename restingpb as SystolicBP
df <- df %>% rename(SystolicBP = RestingBP)


#a glimpse at our data types
glimpse(df)

#numerical variables
num_var <- df %>% select(where(is.numeric))

#categorical variables
cat_var <- df %>% select(where(is.factor))


#lets check for outliers in our numerical variable
outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 -Q1
  lowerbound <-  Q1 - 1.5 * IQR
  upperbound <- Q3 + 1.5 * IQR
  which(x < lowerbound | x > upperbound)
}

outlier_all <- lapply(num_var, outliers)

sapply(outlier_all, length)

#create a boxplot showing the outliers
num_var %>% gather(key = "variable", value ="value") %>% ggplot(aes(x = variable, y = value)) +
  geom_boxplot(fill = "skyblue", outlier.color = "purple", outlier.shape = 8) +
  theme_minimal() + 
  labs(title = "Boxplot of Numeric Variables", x = "Variable", y = "Value") +
  coord_flip()

#We need to do remove some outliers. With my experience as a physician, Cholesterol and systolic BP can not be '0'. Also old peak cannot be negative


#Remove the outliers
cleaned_df <- df %>% filter(SystolicBP != 0, Cholesterol != 0, Oldpeak > 0) 
head(cleaned_df)
dim(cleaned_df) 

#save clean data
write.csv(cleaned_df,"cleaned_hrt.csv",row.names = FALSE)


#Numerical and categorical variables in cleaned_df
cleaned_num <- cleaned_df %>% select(where(is.numeric))

#categorical variables
cleaned_cat <- cleaned_df %>% select(where(is.factor))

#Statistical Analysis

#Age distribution of participants
ggplot(cleaned_df, aes(Age)) +
  geom_histogram(binwidth = 5, fill = 'purple', colour = 'white') + 
  theme_minimal() +
  labs(title = 'Age distribution of participants', x = 'Age', y = 'Frequency')


#Average Age in people with heart disease and those without heart disease
cleaned_df %>% group_by(HeartDisease) %>%
  summarise(avg_age = mean(Age, na.rm = TRUE))


#Average systolicBP by sex
cleaned_df %>% group_by(Sex) %>%
  summarise(avg_BP = mean(SystolicBP, na.rm = TRUE))


#Distribution of target variable
ggplot(cleaned_df, aes(HeartDisease)) +
  geom_bar(fill = c("skyblue", "purple")) +
  labs(title = "Heart Disease Distribution",
       x = "Heart Disease (0 = No, 1 = Yes)",
       y = "Count") +
  theme_minimal()

#Distribution of numerical variables
num_pivot = cleaned_num %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Plot histogram of numerical variables
ggplot(num_pivot, aes(x = value)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "pink", bins = 30, alpha = 0.6) +
  geom_density(color = "purple", size = 1) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = 'Histograms showing distribution of numerical variables',
       x = "Value", y = "Density")


#Distribution of categorical variables
cat_pivot <- cleaned_cat %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ggplot(cat_pivot, aes(x = factor(value))) +
  geom_bar(fill = "purple", color = "pink") +
  facet_wrap(~ variable, scales = "free", ncol = 3)  +
  theme_minimal() +
  labs(title = "Bar Charts of Categorical Variables",, x = "Categorical Variables", y = "Count")

 
#Is there any statistical difference in systolicBP by sex

t.test(SystolicBP ~ Sex, data = cleaned_df)
#the p_value is greater than 0.05, therefore there is no difference statistically significant difference in average systolic blood pressure between males and females.

#is there any statistical  difference in cholesterol by heart disease
t.test(Cholesterol ~ HeartDisease,data=cleaned_df)

#the p_value is greater than 0.05, therefore there is no difference statistically significant difference in average cholesterol between those with heart disease and those without heart disease


#Is there any statistical difference in mean cholesterol levels among different chest pain types

anova_model <- aov(Cholesterol ~ ChestPainType, data = cleaned_df)
summary(anova_model)
#There is no statistically significant difference in mean cholesterol levels across the different chest pain types 

#Tukey HSD post-hoc test
tukey_test <- TukeyHSD(anova_model)
print(tukey_test)
#There are no statistically significant differences in mean cholesterol between any pair of chest pain types.

#is there any statistical difference between chest pain types and heart disease

chi_hx = table(cleaned_df$ChestPainType, cleaned_df$HeartDisease)
chi_hx
chisq.test(chi_hx)
#the p_value < 0.05. Therefore the association between chest pain type and heart disease are statistically significant.


#Is there any corerelation between age and cholesterl
cor.test(cleaned_df$Cholesterol, cleaned_df$Age)
#correlation coefficient is 0.014 and p_value > 0.05, therefore there is no linear relationship between cholesterol and age


ggplot(cleaned_df, aes(x = Age, y = Cholesterol)) +
  geom_point(alpha = 0.8, color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "pink") +
  labs(
    title = 'Scatter Plot of Age vs. Cholesterol',
    subtitle = 'Correlation is not statistically significant (r ≈ 0.01, p = 0.779)',
    x = 'Age',
    y = 'Cholesterol (mg/dL)'
  ) +
  theme_minimal()


#predict heart disease with all factors
model <- glm(HeartDisease ~ ., data = cleaned_df, family = 'binomial')
summary(model)
#male sex, all chest pain types, exercise angina, old peak and flat ST slope all shows statistically significant relationship with Heart Disease as their p_values are less than 0.05

#adjusted odd ratio of the model
exp(cbind(OR = coef(model), confint(model)))
#Male sex(OR = 6.54, CI(3.16 - 14.06)), Exercise angina(OR = 3.29, CI(1.72 - 6.32)), old peak(OR = 2.72, CI(1.84 - 4.18)), flat ST(OR = 3.71, CI(1.13 - 11.74)) leads to increase risk of developing heart disease.

#Type ATA chest pain(OR = 0.35, CI(0.13 - 0.92)), Type NAP chest pain(OR = 0.21, CI(0.10 - 0.46)), Type TA chest pain(OR = 0.17, CI(0.05 - 0.49)) are protective against heart disease

#Conclusion
#The model suggests that being male, Exercise-Induced Angina, having high Oldpeak, and having flat ST significantly increases the risk of heart disease.