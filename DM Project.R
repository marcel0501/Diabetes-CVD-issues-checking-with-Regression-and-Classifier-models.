d1 <- read.csv("https://raw.githubusercontent.com/marcel0501/Obesity-classifier-and-BMI-regression/refs/heads/main/ObesityDataSet_raw_and_data_sinthetic.csv", 
               sep = ",")
head(d1)
sapply(d1, function(x)(sum(is.na(x)))) # No MISSING VALUES

######Exploratory Analysis######

summary(d1)
#Plotting categorical mutables
par(mfrow=c(3,3))
barplot(table(d1$Gender), main="Gender")
barplot(table(d1$family_history_with_overweight), main="Family History with Overweight")
barplot(table(d1$FAVC), main="Frequent consumption of high caloric food FAVC")
barplot(table(d1$CAEC), main="Consumption of food between meals CAEC")
barplot(table(d1$SMOKE), main="Smoking")
barplot(table(d1$SCC), main="Consumption of food while watching TV/using computer SCC")
barplot(table(d1$CALC), main="Consumption of alcohol CALC")
barplot(table(d1$MTRANS), main="Means of transportation MTRANS")
barplot(table(d1$NObeyesdad), main="Obesity Type NObeyesdad")
par(mfrow=c(1,1))

#Pairs Panels on numerical variables
library(psych)
num_vars <- d1[, sapply(d1, is.numeric)]
pairs.panels(num_vars,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#Notiamo che alcune variabili e.g. FCVC hanno valori float essendo stati imputati o creati artificialmente, arrotondiamoli all'int più vicino
round_numeric_df <- function(df, exclude = NULL) {
  df[] <- lapply(names(df), function(col_name) {
    # Se la colonna è numerica e NON è tra quelle escluse
    if (is.numeric(df[[col_name]]) && !(col_name %in% exclude)) {
      return(round(df[[col_name]]))
    } else {
      return(df[[col_name]])
    }
  })
  
  # Mantieni i nomi originali delle colonne
  names(df) <- names(df)
  
  return(df)
}

d1 <- round_numeric_df(d1, exclude = c("Weight", "Height"))


#
#
#
#####Starting with Regression Analysis####
d2<-d1


#Trasformiamo le variabili categoriche in factor
d2[] <- sapply(d1, function(x) {
  if (is.character(x)) as.factor(x) else x
})
#Aggiungiamo BMI su cui fare regressione
d2$BMI <- d2$Weight / (d2$Height)^2

#Inveritamo i valori della variabile CALC, CAEC
d2$CALC <- max(d2$CALC) - d2$CALC
d2$CAEC <- max(d2$CAEC) - d2$CAEC

summary(d2)

#Check colinearity
#vettore delle colonne

d2.list <- d2

fit <- lm(BMI ~ Gender+Age+family_history_with_overweight+FAVC+FCVC+NCP+CAEC+SMOKE+CH2O+SCC+FAF+TUE+CALC+MTRANS+NObeyesdad, data = d2)
summary(fit)

cov=attr(terms(fit), "term.labels") 

library(dplyr)
d2_numeric <- d2[,cov]%>% dplyr::select_if(is.numeric)
colnames(d2_numeric)

require(corrgram)
corrgram(d2_numeric, lower.panel = panel.cor, cex=1, cex.labels = 1)

#Tol e VIF
y = as.numeric(d2$BMI)
X<-d2_numeric
X=as.matrix(X)


library(mctest)
# imcdiag(X,y)

m=lm(y~X)
imcdiag(m)

#No collinearity problems detected
library(MASS)
boxcoxreg1<-boxcox(fit)
lambda1<-boxcoxreg1$x[which(boxcoxreg1$y==max(boxcoxreg1$y))]
lambda1
#Lambda vicino allo 0, serve trasformare la variabile risposta
# use log for YL
hist(d2$BMI)
hist(log(d2$BMI))

fitBox <- lm(log(BMI) ~ Gender+Age+family_history_with_overweight+FAVC+FCVC+NCP+CAEC+SMOKE+CH2O+SCC+FAF+TUE+CALC+MTRANS+NObeyesdad, data = d2)
summary(fit)
summary(fitBox)
