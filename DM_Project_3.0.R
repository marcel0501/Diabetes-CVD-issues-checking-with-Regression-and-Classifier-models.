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
par(mfrow=c(2,2))
plot(fitBox)
qqnorm(fitBox$residuals)
qqline(fitBox$residuals)



##### reset test #####
library(lmtest)
resettest(fitBox, power = 2, type = "fitted",  data = d3)

##### TUTTE LE VARIABILI ALLA 2a ######

model_formula <- Calories_Burned^0.2 ~ 
  Age +
  Gender +
  Height..m. +
  Max_BPM +
  Avg_BPM +
  Resting_BPM +
  Session_Duration..hours. + I(Session_Duration..hours.^2) +
  Workout_Type +
  Fat_Percentage +
  Water_Intake..liters. +
  Workout_Frequency..days.week. + I(Workout_Frequency..days.week.^2) +
  Daily.meals.frequency +
  Physical.exercise +
  meal_type +
  diet_type +
  sugar_g +
  sodium_mg +
  cholesterol_mg +
  serving_size_g +
  cooking_method +
  prep_time_min +
  cook_time_min +
  rating +
  Name.of.Exercise +
  Sets +
  Reps +
  Benefit +
  Burns.Calories..per.30.min. +
  Target.Muscle.Group +
  Equipment.Needed +
  Difficulty.Level +
  Body.Part +
  Type.of.Muscle +
  Workout +
  protein_per_kg


mod <- lm(model_formula, data = d3)
summary(mod)
resettest(mod, power = 2, type = "fitted",  data = d3)
plot(mod)



##### GAM model #####
library(gam)
gam1 <- gam(Calories_Burned ~ . -Carbs -Proteins -Fats -cal_from_macros -pct_maxHR
            -Weight..kg. -pct_HRR -lean_mass_kg -Calories -BMI -Experience_Level -cal_balance, 
            data = d3)
summary(gam1)


gam2 <- gam(Calories_Burned ~ . -Carbs -Proteins -Fats -cal_from_macros -pct_maxHR
            -Weight..kg. -pct_HRR -lean_mass_kg -Calories -BMI -Experience_Level -cal_balance
            -Age +s(Age) 
            -Avg_BPM +s(Avg_BPM) 
            -Session_Duration..hours. +s(Session_Duration..hours.)
            -Water_Intake..liters. +s(Water_Intake..liters.)
            -Workout_Frequency..days.week. +s(Workout_Frequency..days.week.)
            -Reps +s(Reps),
            data = d3)
summary(gam2)
plot(gam2, resid=T, pch=16)


gam3 <- gam((Calories_Burned^0.2) ~ . -Carbs -Proteins -Fats -cal_from_macros -pct_maxHR
            -Weight..kg. -pct_HRR -lean_mass_kg -Calories -BMI -Experience_Level -cal_balance
            -Session_Duration..hours. +s(Session_Duration..hours.)
            -Workout_Frequency..days.week. +s(Workout_Frequency..days.week.),
            data = d3)
summary(gam3)


# diagnostic plots
par(mfrow=c(3,3))
plot(gam3, resid=T, pch=16)
par(mfrow=c(1,1))


fit3 <- update(fitBox, . ~ . +I(sqrt(Session_Duration..hours.))
              +I(-(Workout_Frequency..days.week.)^2))
summary(fit3)
# Multiple R-squared:  0.7848,	Adjusted R-squared:  0.7844


fit4 <- update(fit3, . ~ . -Gender -Max_BPM -meal_type -diet_type -cooking_method 
               -Name.of.Exercise -Benefit -Difficulty.Level)
summary(fit4)
# Multiple R-squared:  0.7846,	Adjusted R-squared:  0.7843


# AIC and BIC comparison
AIC(fit3, fit4)
BIC(fit3, fit4) # fit4 is better !!!

# diagnostic plots
par(mfrow=c(2,2))
plot(fit4)

# reset test
resettest(fit4, power = 4, type = "fitted",  data = d3)

# VIF


gam4 <- gam(Calories_Burned ~ . -Carbs -Proteins -Fats -cal_from_macros -pct_maxHR
            -Weight..kg. -pct_HRR -lean_mass_kg -Calories -BMI -Experience_Level -cal_balance
            -Gender -Max_BPM -meal_type -diet_type -cooking_method 
            -Name.of.Exercise -Benefit -Difficulty.Level
            -Session_Duration..hours. +I(sqrt(Session_Duration..hours.))
            -Workout_Frequency..days.week. +I(-(Workout_Frequency..days.week.)^2)
            -sugar_g +s(sugar_g) -sodium_mg +s(sodium_mg) -cholesterol_mg +s(cholesterol_mg)
            -serving_size_g +s(serving_size_g) -prep_time_min +s(prep_time_min)
            -cook_time_min +s(cook_time_min),
            data = d3)
summary(gam4)
plot(gam4)

gam5 <- gam(Calories_Burned ~ . -Carbs -Proteins -Fats -cal_from_macros -pct_maxHR
                    -Weight..kg. -pct_HRR -lean_mass_kg -Calories -BMI -Experience_Level -cal_balance
                    -Gender -Max_BPM -meal_type -diet_type -cooking_method 
                    -Name.of.Exercise -Benefit -Difficulty.Level
                    -Sets-Reps+SetsxREPS
                    -Burns.Calories..per.30.min.+ s(Burns.Calories..per.30.min.)
                    +I(sqrt(Session_Duration..hours.))
                    +I(-(Workout_Frequency..days.week.)^2)+
            I(-cholesterol_mg^3 + cholesterol_mg^2) +
             + I(-serving_size_g^2 + serving_size_g) +
             + I((prep_time_min)^2) + 
              + I(-log(cook_time_min + 1)+ I(sodium_mg^2) + I(sqrt(sodium_mg))+ I(sugar_g^2) + I(sugar_g^3))
            
                    ,data = d3)
plot(gam5)
summary(gam5)


###LAST TRY BEFORE GIVING UP
fit5 <- lm(Calories_Burned^0.2 ~ . -Carbs -Proteins -Fats -cal_from_macros -pct_maxHR
           -Weight..kg. -pct_HRR -lean_mass_kg -Calories -BMI -Experience_Level -cal_balance
           -Gender -Max_BPM -meal_type -diet_type -cooking_method 
           -Name.of.Exercise -Benefit -Difficulty.Level
           +I(sqrt(Session_Duration..hours.))
           +I(-(Workout_Frequency..days.week.)^2)+
            I(Age^2)+
            I(Avg_BPM^2)+
            I(Resting_BPM^2)
           + I(Fat_Percentage^2)
           + I(Water_Intake..liters.^2)
           +I(sugar_g^2)
           
           +I(Burns.Calories..per.30.min.^2)+I(Burns.Calories..per.30.min.^2)+
             I(-cholesterol_mg^3 + cholesterol_mg^2) +
             + I(-serving_size_g^2 + serving_size_g) +
             + I((prep_time_min)^2) + 
             + I(-log(cook_time_min + 1))+ I(sodium_mg^2) + I(sqrt(sodium_mg))+ I(sugar_g^2) + I(sugar_g^3)
           ,data = d3)
summary(fit5)
resettest(fit5, power =  , type = "fitted",  data = d3)
plot(fit5)
plot(fitted(fit5), resid(fit5))
abline(h=0, col="red")
AIC(fit5, fit4)
##### Weighted Least Squares #####
w1 <- exp(-fitted(fit4))
fitBox_wls <- lm(Calories_Burned^0.2 ~ . -Carbs -Proteins -Fats -cal_from_macros -pct_maxHR
                 -Weight..kg. -pct_HRR -lean_mass_kg -Calories -BMI -Experience_Level -cal_balance
                 -Gender -Max_BPM -meal_type -diet_type -cooking_method 
                 -Name.of.Exercise -Benefit -Difficulty.Level
                 -Session_Duration..hours. +I(sqrt(Session_Duration..hours.))
                 -Workout_Frequency..days.week. +I(-(Workout_Frequency..days.week.)^2)+
                   I(-cholesterol_mg^3 + cholesterol_mg^2) +
                   + I(-serving_size_g^2 + serving_size_g) +
                   + I((prep_time_min)^2) + 
                   + I(-log(cook_time_min + 1))+ I(sodium_mg^2) + I(sqrt(sodium_mg))+ I(sugar_g^2) + I(sugar_g^3)
                 ,data = d3, weights=w1)
summary(fitBox_wls)
resettest(fitBox_wls)

##### Weighted Least Squares #####
w <- exp(-fitted(fit))
fitBox_wls <- lm(((Calories_Burned^0.58)-1/0.58) ~ . -Carbs -Proteins -Fats 
                 -cal_from_macros -pct_maxHR -Weight..kg. -pct_HRR -lean_mass_kg 
                 -Calories -BMI, data=d3, weights = w)
summary(fitBox_wls)

plot(fitBox_wls, which=1)
plot(fitBox_wls)

library(lmtest)
resettest(fitBox_wls, power = 2, type = "fitted",  data = d3)
bptest(fitBox_wls)
qqnorm(fitBox_wls$residuals)
qqline(fitBox_wls$residuals)

## Model Selection 

sel_both <- step(lm(Calories_Burned ~ 1, data = d3), 
                 scope = list(lower = ~1, upper = formula(fit_reduced)),
                 direction = "both")
summary(sel_both)
coefficients(sel_both)

gam_ms <-  gam(Calories_Burned ~ s(Session_Duration..hours.) + +
                 (Workout_Frequency..days.week.) + 
                 s(Water_Intake..liters.) +
                 s(cook_time_min)  +
                 s(rating) + 
                 (serving_size_g) +
                 (Resting_BPM) +
                 (sodium_mg) +
                 (Height..m.) +
                 (Fat_Percentage), data = d3)
summary(gam_ms)
plot(gam_ms)
resettest(gam_ms, power = 2, type = "fitted",  data = d3)
vars <- c("Session_Duration..hours.","Water_Intake..liters.","cook_time_min","serving_size_g","Resting_BPM",
           "sodium_mg","Height..m.","Fat_Percentage")
d3[vars] <- scale(d3[vars], center = TRUE, scale = TRUE)

hist(d3$Calories_Burned^0.2)
lm_ms<-  lm(log(Calories_Burned) ~ (Session_Duration..hours.)+I(Session_Duration..hours.^2)+I(Session_Duration..hours.^3)+
              (Workout_Frequency..days.week.) + 
              (Water_Intake..liters.) + 
              (cook_time_min) + +
              (rating)+
              (serving_size_g) +
              (Resting_BPM) + 
              (sodium_mg) +
              (Height..m.) +  
              (Fat_Percentage) , data = df_clean)
resettest(lm_ms, power = 2, type = "fitted",  data = df_clean)
summary(lm_ms)
bptest(lm_ms)


resettest(fitBox_wls, power=2 , type="fitted", data=d3)
summary(fitBox_wls)
bptest(fitBox_wls)
plot(fitBox_wls)
###OUTLIER AND INFLUENTIAL POINTS#####
hat_values <- hatvalues(lm_ms)

# Cook's distance
cooks_d <- cooks.distance(lm_ms)


influential_points_leverage <- which(hat_values > 2*(length(coef(lm_ms)))/nrow(d3))
# Punti con alto Cook's
influential_points_cooks <- which(cooks_d > 4/(nrow(d3)-length(coef(lm_ms))))
points_to_remove <- unique(c(influential_points_leverage, influential_points_cooks))
# Dataset senza punti influenti
df_clean <- d3[-points_to_remove, ]
# Ricostruisci il modello
model_w = lm(Calories_Burned ~ (Session_Duration..hours.)+
               (Workout_Frequency..days.week.) + 
               (Water_Intake..liters.) + 
               (cook_time_min) + 
               (rating)+
               (serving_size_g) +
               (Resting_BPM) + 
               (sodium_mg) +
               (Height..m.) +  
               (Fat_Percentage) , data = df_clean)
fitBox_wls <- lm(Calories_Burned^0.2 ~ (Session_Duration..hours.)+I(Session_Duration..hours.^2)+I(Session_Duration..hours.^3)+
                   (Workout_Frequency..days.week.) + 
                   (Water_Intake..liters.) + I(Water_Intake..liters.^2)+ 
                   (cook_time_min) + I(cook_time_min^2)+
                   (rating)+
                   (serving_size_g) +
                   (Resting_BPM) + 
                   (sodium_mg) +
                   (Height..m.) +  
                   (Fat_Percentage) , weights = w, data = df_clean)


library(lmtest)
bptest(lm_ms)
summary(lm_ms)
plot(lm_ms)
qqnorm(lm_ms$residuals)
qqline(lm_ms$residuals)
library(car)
vif(lm_ms)


boxcoxreg2 <- boxcox(lm_ms)
lambda2<- boxcoxreg1$x[which(boxcoxreg1$y==max(boxcoxreg1$y))]
lambda2

