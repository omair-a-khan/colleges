## Title: Which colleges are worth it? Predicting debt-to-earnings ratios using regression analysis
## Author: Omair Khan
## Original Date: 2016-03-09
## Last Date: 2016-04-16

## Working Directory
setwd("~/Dropbox/Documents/School/2016 Spring/STAT 581")

## Load libraries
library(RSQLite) # To connect to database
library(choroplethr) # For US map
library(choroplethrMaps)
library(Hmisc) # For Choropleth cuts
library(Amelia) # For showing NA's in graph
library(dplyr) # For working with data frames
library(glmnet) # For lasso regression
library(pls) # For PCR and PLS regression
library(corrgram) # For visualizing correlations

## Connect to database
db <- dbConnect(dbDriver("SQLite"), "~/Downloads/output/database.sqlite")
dbGetQuery(db, "PRAGMA temp_store=2;") 

## Load state data
data("state.regions")

#____________________________________________________________________________________
# Retrieve data from scorecard

## 2009 data
Loans2009 <- dbGetQuery(db, "
                        SELECT  INSTNM College,
                        PCTFLOAN FSFLoans,
                        CONTROL CollegeType,
                        C150_4 CompletionRate,
                        CDR2 Default2,
                        COSTT4_A Cost,
                        STABBR abb,
                        AVGFACSAL FacultySalary,
                        RPY_1YR_RT Repayment1,
                        RPY_3YR_RT Repayment3,
                        RPY_1YR_N NSRepayment1,
                        GRAD_DEBT_MDN MedianDebtGrad,
                        PCIP16 PLanguage,
                        ADM_RATE AdmRate,
                        WDRAW_DEBT_MDN MedianDebtNGrad,
                        md_earn_wne_p6 Earnings6,
                        md_earn_wne_p8 Earnings8,
                        md_earn_wne_p10 Earnings10,
                        GRAD_DEBT_MDN/md_earn_wne_p6 DebtToEarn,
                        PPTUG_EF PartTime,
                        SATMTMID Math,
                        UGDS_WHITE WEnroll,
                        UGDS_BLACK BEnroll,
                        UGDS_HISP HEnroll,
                        UGDS_ASIAN AEnroll,
                        Year,
                        SATVRMID Verbal,
                        SATWRMID Writing,
                        UGDS UndergradEnrollment
                        FROM Scorecard
                        WHERE Year=2009
                        AND PREDDEG='Predominantly bachelor''s-degree granting'
                        AND md_earn_wne_p6 NOT LIKE 'NA'
                        AND GRAD_DEBT_MDN NOT LIKE 'NA'
                        AND GRAD_DEBT_MDN/md_earn_wne_p6 NOT LIKE 'Inf'
                        AND Cost NOT LIKE 'NA'
                        ORDER BY INSTNM ASC")

## 2011 data
Loans2011 <- dbGetQuery(db, "
                        SELECT  INSTNM College,
                        PCTFLOAN FSFLoans,
                        CONTROL CollegeType,
                        C150_4 CompletionRate,
                        CDR2 Default2,
                        COSTT4_A Cost,
                        STABBR abb,
                        AVGFACSAL FacultySalary,
                        RPY_1YR_RT Repayment1,
                        RPY_3YR_RT Repayment3,
                        RPY_1YR_N NSRepayment1,
                        GRAD_DEBT_MDN MedianDebtGrad,
                        PCIP16 PLanguage,
                        ADM_RATE AdmRate,
                        WDRAW_DEBT_MDN MedianDebtNGrad,
                        md_earn_wne_p6 Earnings6,
                        md_earn_wne_p8 Earnings8,
                        md_earn_wne_p10 Earnings10,
                        GRAD_DEBT_MDN/md_earn_wne_p6 DebtToEarn,
                        PPTUG_EF PartTime,
                        SATMTMID Math,
                        UGDS_WHITE WEnroll,
                        UGDS_BLACK BEnroll,
                        UGDS_HISP HEnroll,
                        UGDS_ASIAN AEnroll,
                        Year,
                        SATVRMID Verbal,
                        SATWRMID Writing,
                        UGDS UndergradEnrollment
                        FROM Scorecard
                        WHERE Year=2011
                        AND PREDDEG='Predominantly bachelor''s-degree granting'
                        AND md_earn_wne_p6 NOT LIKE 'NA'
                        AND GRAD_DEBT_MDN NOT LIKE 'NA'
                        AND GRAD_DEBT_MDN/md_earn_wne_p6 NOT LIKE 'Inf'
                        AND Cost NOT LIKE 'NA'
                        ORDER BY INSTNM ASC")

#____________________________________________________________________________________
# Regression preparation

## Rename CollegeType to prevent errors because of spacing
Loans2009$CollegeType[Loans2009$CollegeType == "Private nonprofit"] <- "Nonprofit"
Loans2009$CollegeType[Loans2009$CollegeType == "Private for-profit"] <- "For-profit"
Loans2011$CollegeType[Loans2011$CollegeType == "Private nonprofit"] <- "Nonprofit"
Loans2011$CollegeType[Loans2011$CollegeType == "Private for-profit"] <- "For-profit"

## Create test data
df.test <- select(Loans2011, College, DebtToEarn, FSFLoans, CollegeType, CompletionRate, 
                  Default2, Cost, abb, FacultySalary, Repayment1, Repayment3,
                  NSRepayment1, PLanguage, AdmRate, PartTime, Math, 
                  WEnroll, BEnroll, HEnroll, AEnroll, Verbal, Writing,
                  UndergradEnrollment)

df.test <- na.omit(df.test)

x.test <- model.matrix(DebtToEarn ~ . -College, df.test)[,-1]
y.test <- df.test$DebtToEarn

## Set up test.avg for R2 calculation
test.avg <- mean(y.test)

## Calculate DTE ratio for each state in 2009
df1 <- group_by(Loans2009, CollegeType, abb) 
DTE <- summarize(df1, value = mean(DebtToEarn))

## Change variables to appropriate class
Loans2009$Cost <- as.numeric(Loans2009$Cost)
Loans2009$abb <- as.factor(Loans2009$abb)
Loans2009$FacultySalary <- as.numeric(Loans2009$FacultySalary)
Loans2009$NSRepayment1 <- as.numeric(Loans2009$NSRepayment1)
Loans2009$UndergradEnrollment <- as.numeric(Loans2009$UndergradEnrollment)

## Select variables for training data set
df.train <- select(Loans2009, College, DebtToEarn, FSFLoans, CollegeType, CompletionRate, 
                   Default2, Cost, abb, FacultySalary, Repayment1, Repayment3,
                   NSRepayment1, PLanguage, AdmRate, PartTime, Math, 
                   WEnroll, BEnroll, HEnroll, AEnroll, Verbal, Writing,
                   UndergradEnrollment)

## Remove observations with missing data              
df.train <- na.omit(df.train)

## Convert data frame to matrix
x.train <- model.matrix(DebtToEarn ~ . -College, df.train)[,-1]
y.train <- df.train$DebtToEarn

## Remove states that are not common between 2009 and 2011: AL, GU, PR, WY
x.train <- x.train[, -c(7, 17, 46, 59)]

## Create data frame for MSE and R2
results <- matrix(data = NA, nrow = 4, ncol = 2)
results <- as.data.frame(results)
colnames(results) <- c("MSE", "R2")
rownames(results) <- c("Ridge", "Lasso", "PCR", "PLS")

#____________________________________________________________________________________
# Exploratory data analysis

## Visualizing missing data
missmap(Loans2009, main = "US Department of Education Data (2011) Missing Data", 
        col = c("yellow", "black"), legend = FALSE)

## Visualizing correlation
corrgram(x.test, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie,
         text.panel = panel.txt)

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat){
  ut <- upper.tri(cormat)
  data.frame(
    row <- rownames(cormat)[row(cormat)[ut]],
    column <- rownames(cormat)[col(cormat)[ut]],
    cor <- (cormat)[ut],
    p <- pmat[ut]
  )
}

res <- rcorr(x.test)
correlations <- flattenCorrMatrix(res$r, res$P)

#____________________________________________________________________________________
# Ridge regression

## By default the glmnet() function performs ridge regression for an automatically selected range of lambda values. However, here we have chosen to implement the function over a grid of values ranging from lambda = 10^10 to lambda = 10^-2, essentially covering the full range of scenarios from the null model containing only the intercept, to the least squares fit.
grid <- 10^seq(10, -2, length = 100) 

set.seed(1234)

## Cross validation to find optimal lambda
cv.out <- cv.glmnet(x.train, y.train, alpha = 0)
plot(cv.out, main = "CV for ridge regression")
bestLambdaRidge <- cv.out$lambda.min

## Model with all observations
ridge.fit <- glmnet(x.train, y.train, alpha = 0, lambda = grid, thresh = 1e-12)

## Predict for 2011 data
ridge.pred = predict(ridge.fit, s = bestLambdaRidge, newx = x.test)

## Calculate and assign MSE and R2 for all school types
results[1, 1] <- mean((y.test - ridge.pred)^2)
results[1, 2] = 1 - mean((y.test - ridge.pred)^2) / mean((y.test - test.avg)^2)

## Find coefficients
ridge.coef <- predict(ridge.fit, type = "coefficients", s = bestLambdaRidge, newx = x.test)

#____________________________________________________________________________________
# Lasso regression

set.seed(1234)

## Cross validation to find optimal lambda
cv.out <- cv.glmnet(x.train, y.train, alpha = 1, family = "gaussian")
plot(cv.out, main = "CV for lasso regression")
bestLambdaLasso = cv.out$lambda.min

## Model with all observations
lasso.fit <- glmnet(x.train, y.train, alpha = 1, family = "gaussian")
#lasso.coef <- predict(lasso.fit, type = "coefficients", s = bestLambdaLasso)[1:70, ]
#lasso.coef[lasso.coef != 0]

## Predict for 2011 data
lasso.pred <- predict(lasso.fit, newx = x.test, type = "response", s = bestLambdaLasso, exact = TRUE)

## Calculate and assign MSE and R2 for all school types
results[2, 1] <- mean((y.test - lasso.pred)^2)
results[2, 2] <- 1 - mean((y.test - lasso.pred)^2) / mean((y.test - test.avg)^2)

## Find coefficients
lasso.coef <- predict(lasso.fit, newx = x.test, type = "coefficients", s = bestLambdaLasso, exact = TRUE)

#____________________________________________________________________________________
# PCR regression

set.seed(1234)
pcr.fit <- pcr(y.train ~ x.train, validation = "CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")
MSEP(pcr.fit)

pcr.pred <- predict(pcr.fit, newdata = x.test, ncomp = 47)
pcr.pred <- pcr.pred[1:683]

results[3, 1] <- mean((y.test - pcr.pred)^2)
results[3, 2] <- 1 - mean((y.test - pcr.pred)^2) / mean((y.test - test.avg)^2)

#____________________________________________________________________________________
# PLS regression

set.seed(1234)
pls.fit <- plsr(y.train ~ x.train, validation = "CV")
summary(pls.fit)

validationplot(pls.fit, val.type = "MSEP")
MSEP(pls.fit)

pls.pred <- predict(pls.fit, newdata = x.test, ncomp = 27)
pls.pred <- pls.pred[1:683]

results[4, 1]	<- mean((y.test - pls.pred)^2)
results[4, 2] <- 1 - mean((y.test - pls.pred)^2) / mean((y.test - test.avg)^2)

#____________________________________________________________________________________
# Calculations for maps

## True debt-to-earnings ratio for 2009
DTEpublic2009 <- filter(DTE, CollegeType == "Public")
DTEpublic2009 <- inner_join(x = DTEpublic2009, y = state.regions, by = "abb")
DTEpublic2009 <- select(DTEpublic2009, CollegeType, value, region)

## True debt-to-earnings ratio for 2011
df2 <- group_by(Loans2011, CollegeType, abb) # True 2011 DebtToEarn by college
DTE2011 <- summarize(df2, value = mean(DebtToEarn)) # True 2011 DebtToEarn by state and college type

DTEpublic2011 = filter(DTE2011, CollegeType == "Public")
DTEpublic2011 = inner_join(x = DTEpublic2011, y = state.regions, by = "abb")
DTEpublic2011 = select(DTEpublic2011, CollegeType, value, region)

## Lasso (best model) predicted debt-to-earnings ratio for 2011
pred2011 <- cbind(df.test$College, df.test$CollegeType, df.test$abb, lasso.pred)                           
colnames(pred2011) <- c("College", "CollegeType", "abb", "DebtToEarn")
pred2011 <- as.data.frame(pred2011, stringsAsFactors = FALSE)
pred2011$DebtToEarn <- as.numeric(pred2011$DebtToEarn)

df3 <- group_by(pred2011, CollegeType, abb) 
DTEpred <- summarize(df3, value = mean(DebtToEarn))

DTEpred <- left_join(x = DTEpred, y = state.regions, by = "abb") 
DTEpred <- select(DTEpred, CollegeType, value, region)

DTEpredpublic <- filter(DTEpred, CollegeType == "Public")

#____________________________________________________________________________________
# Map visualizations

## True debt-to-earnings ratio for 2009
DTEpublic2009$value=cut2(DTEpublic2009$value, cuts = c(0, 0.2, 0.4, 0.6, 0.8, 1))

print(state_choropleth(DTEpublic2009,
                       title="Average true DTE ratio for public schools in 2009", 
                       num_colors = 5, 
                       legend="DTE ratio"))

## True debt-to-earnings ratio for 2011
DTEpublic2011$value=cut2(DTEpublic2011$value,cuts=c(0, 0.2, 0.4, 0.6, 0.8, 1))

print(state_choropleth(DTEpublic2011,
                       title="Average true DTE ratio for public schools in 2011", 
                       num_colors = 5,
                       legend="DTE ratio"))

## Lasso (best model) predicted debt-to-earnings ratio for 2011
DTEpredpublic$value=cut2(DTEpredpublic$value,cuts=c(0, 0.2, 0.4, 0.6, 0.8, 1))

print(state_choropleth(DTEpredpublic,
                       title="Average predicted DTE ratio for public schools in 2011", 
                       num_colors = 5, 
                       legend="DTE ratio"))
