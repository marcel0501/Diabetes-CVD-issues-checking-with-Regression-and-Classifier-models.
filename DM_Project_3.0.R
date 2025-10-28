##### Load Data #####
d1 <- read.csv("https://raw.githubusercontent.com/marcel0501/Obesity-classifier-and-BMI-regression/refs/heads/main/Final_data.csv", 
               sep = ",")
head(d1)
sapply(d1, function(x)(sum(is.na(x)))) # No MISSING VALUES



##### Exploratory Analysis #####
summary(d1)

# correlation matrix
d1_numeric <- d1[, sapply(d1, is.numeric)]
cor_matrix <- cor(d1_numeric)
cor_matrix
# visualizziamo la matrice di correlazione
library(corrplot)
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.7)

# lista con i nomi delle colonne di d1
col_names <- names(d1)
col_names

# rimuoviamo le colonne indesiderate
d2 <- d1[,-c(22,44,46,52,53,54)]
# "BMI_calc" (44), "pct_carbs" (46), "expected_burn" (52), 
# "Burns.Calories..per.30.min._bc" (53), "Burns_CAlories_Bin" (54)

# Notiamo che alcune variabili hanno valori float essendo stati creati artificialmente, 
# arrotondiamoli all'int. più vicino
round_numeric_df <- function(df, include = NULL) {
  df[] <- lapply(names(df), function(col_name) {
    # Se la colonna è numerica ed è tra quelle incluse
    if (is.numeric(df[[col_name]]) && (col_name %in% include)) {
      return(round(df[[col_name]]))
    } else {
      return(df[[col_name]])
    }
  })
  # Mantieni i nomi originali delle colonne
  names(df) <- names(df)
  return(df)
}

# arrotondiamo le colonne selezionate
d2 <- round_numeric_df(d2, include = c("Age", "Workout_Frequency..days.week.",
                                       "Experience_Level","Daily.meals.frequency",
                                       "Physical.exercise","Sets","Reps",
                                       "Avg_BPM","Max_BPM","Resting_BPM"))
head(d2)
summary(d2)

# Pairs Panels on numerical variables
# library(psych)
# numeric_vars <- d2[, sapply(d2, is.numeric)]
# pairs.panels(numeric_vars, 
#             method = "pearson", # correlation method
#             hist.col = "#00AFBB",
#             density = TRUE,  # show density plots
#             ellipses = TRUE # show correlation ellipses
# )



##### Regression Analysis #####
# fattorizziamo le variabili categoriche
d3 <- d2
d3[] <- sapply(d2, function(x) {
  if (is.character(x)) as.factor(x) else x
})
head(d3)
summary(d3)



##### Check colinearity #####
fit <- lm(Calories_Burned ~ ., data = d3)
summary(fit)

# Vettore delle covariate
cov <- attr(terms(fit), "term.labels")

# Vettore delle covariate da escludere
exclude <- c()
exclude <- c("Carbs","Proteins","Fats","cal_from_macros","pct_maxHR","Weight..kg.","pct_HRR",
             "lean_mass_kg","Calories","BMI")

# d3_numeric
library(dplyr)
d3_numeric <- d3 |>
  dplyr::select(dplyr::all_of(cov)) |>
  dplyr::select(!dplyr::any_of(exclude)) |>
  dplyr::select(dplyr::where(is.numeric))

### Tol and VIF ###
y = as.numeric(d3$Calories_Burned)
x <- d3_numeric
x <- as.matrix(x)

library(mctest)
m=lm(y ~ x)
imcdiag(m)


# collinearity problems detected, 
# we'll proceed by taking out the variables with VIF > 5
fit2 = lm(Calories_Burned ~ . -Carbs -Proteins -Fats -cal_from_macros -pct_maxHR
          -Weight..kg. -pct_HRR -lean_mass_kg -Calories -BMI, data = d3)
summary(fit2)

##### ANOVA test #####
fit_reduced <- update(fit2, . ~ . -Experience_Level -cal_balance)

anova(fit2, fit_reduced)

summary(fit_reduced) 
# abbiamo tolto cal_balance e Experience_Level variabili non significative



##### Box-Cox Transformation #####
library(MASS)
boxcoxreg1 <- boxcox(fit_reduced)
lambda1 <- boxcoxreg1$x[which(boxcoxreg1$y==max(boxcoxreg1$y))]
lambda1

# Lambda vicino allo 0, serve trasformare la variabile risposta
hist(d3$Calories_Burned)
hist(log(d3$Calories_Burned))
hist(d3$Calories_Burned^0.2) # migliore

fitBox <- update(fit_reduced, .^0.2 ~ .)

# confronto
summary(fit_reduced)
summary(fitBox)

# diagnostic plots
plot(fitBox)
qqnorm(fitBox$residuals)
qqline(fitBox$residuals)



##### reset test #####
library(lmtest)
resettest(fitBox, power = 2, type = "fitted",  data = d3)



##### GAM model #####
library(gam)
gam1 <- gam((Calories_Burned^0.2) ~ . -Carbs -Proteins -Fats -cal_from_macros -pct_maxHR
            -Weight..kg. -pct_HRR -lean_mass_kg -Calories -BMI -Experience_Level -cal_balance, 
            data = d3)
summary(gam1)
# Residual Deviance: 492.4375
# AIC: -17250.86


gam2 <- gam((Calories_Burned^0.2) ~ . -Carbs -Proteins -Fats -cal_from_macros -pct_maxHR
            -Weight..kg. -pct_HRR -lean_mass_kg -Calories -BMI -Experience_Level -cal_balance
            -Age +s(Age) 
            -Avg_BPM +s(Avg_BPM) 
            -Session_Duration..hours. +s(Session_Duration..hours.)
            -Workout_Frequency..days.week. +s(Workout_Frequency..days.week.),
            data = d3)
summary(gam2)
# Residual Deviance: 475.5642
# AIC: -17926.17


gam3 <- gam((Calories_Burned^0.2) ~ . -Carbs -Proteins -Fats -cal_from_macros -pct_maxHR
            -Weight..kg. -pct_HRR -lean_mass_kg -Calories -BMI -Experience_Level -cal_balance
            -Session_Duration..hours. +s(Session_Duration..hours.)
            -Workout_Frequency..days.week. +s(Workout_Frequency..days.week.),
            data = d3)
summary(gam3)
# Residual Deviance: 475.7741
# AIC: -17929.35


# diagnostic plots
par(mfrow=c(3,3))
plot(gam3, resid=T, pch=16)
par(mfrow=c(1,1))
# Residual Deviance: 476.0915
# AIC: -17920.01


fit3 <- update(fitBox, . ~ . -Session_Duration..hours.+I(sqrt(Session_Duration..hours.))
               -Workout_Frequency..days.week. +I(-(Workout_Frequency..days.week.)^2))
summary(fit3)
# Multiple R-squared:  0.7848,	Adjusted R-squared:  0.7844


fit4 <- update(fit3, . ~ . -Gender -Max_BPM -meal_type -diet_type -cooking_method 
               -Name.of.Exercise -Benefit -Difficulty.Level)
summary(fit4)
# Multiple R-squared:  0.7846,	Adjusted R-squared:  0.7843


# AIC and BIC comparison
AIC(fit3, fit4)
BIC(fit3, fit4)

# diagnostic plots
par(mfrow=c(2,2))
plot(fit4)

# reset test
resettest(fit4)

# VIF
library(car)
vif(fit4)



##### Weighted Least Squares #####
w <- exp(-fitted(fit))
fitBox_wls <- lm(((Calories_Burned^0.58)-1/0.58) ~ . -Carbs -Proteins -Fats 
                 -cal_from_macros -pct_maxHR -Weight..kg. -pct_HRR -lean_mass_kg 
                 -Calories -BMI, data=d3, weights = w)
summary(fitBox_wls)

plot(fitBox_wls, which=1)
plot(fitBox_wls)

library(lmtest)
bptest(fitBox_wls)
qqnorm(fitBox_wls$residuals)
qqline(fitBox_wls$residuals)