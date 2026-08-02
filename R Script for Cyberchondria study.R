# Normality Testing using shapiro-wilk normality test
shapiro.test(Raw_scores$CSS12_Total)
shapiro.test(Raw_scores$MCQ30_Total)
shapiro.test(Raw_scores$GAD_Total)


# Creating Histograms
hist(Raw_scores$CSS12_Total)
hist(Raw_scores$MCQ30_Total)
hist(Raw_scores$GAD_Total)


# creating Q-Q Plots
library(ggplot2)
ggplot(data = Raw_scores, aes(sample = CSS12_Total)) + stat_qq() + 
stat_qq_line(color = "blue", linetype = "dashed") +labs(title = "Q-Q Plot for CSS_Total", y = "Sample Quantiles", x = "Theoretical Quantiles") +
theme_minimal()


ggplot(data = Raw_scores, aes(sample= MCQ30_Total))+ stat_qq()+ stat_qq_line(color= "pink", linetype = "dashed")+ labs(title = "Q_Q Plot for MCQ30_Total",y= "Sample Quantiles", x= "Theoretical Quantiles")+ theme_minimal()


ggplot(data = Raw_scores, aes(sample= GAD_Total))+ stat_qq()+ stat_qq_line(color= "green", linetype ="dashed")+ labs(title = "Q_Q Plot for GAD_Total",y= "Sample Quantiles", x= "Theoretical Quantiles")+ theme_minimal()



# Wilcoxon rank sum test for gender differences
wilcox.test(CSS12_Total ~ Gender, data = Raw_scores, mu=0, 
alternative = "greater", conf.int= T, conf.level=0.95, exact=F, correct=T)


# Spearman s rho correlation matrix
## creating a new data frame
df1<-data.frame(Raw_scores)
df2<-df1[,-c(1,2,3,4,6,7,8,9,11,12,13,14,15)]

## Correlation Matrix
cor(df2, method = "spearman")


## Correlation between CSS-12 and MCQ-30
cor.test(df2$CSS12_Total, df2$MCQ30_Total, method = "spearman", exact = NULL, conf.level = 0.95, continuity = F)

## Correlation between MCQ-30 and GAD-7
cor.test(df2$MCQ30_Total, df2$CSS12_Total, method = "spearman", exact = NULL, conf.level = 0.95, continuity = F)
## Correlation between CSS-12 and GAD-7
cor.test(df2$CSS12_Total, df2$GAD_Total, method = "spearman", exact = NULL, conf.level = 0.95, continuity = F)


# Cronbach s alpha
alpha(CSS_item_responses)
alpha(MCQ_Item_responses)
alpha(GAD_item_responses_for_cronbach)


#MULTIPLE LINEAR REGRESSION
## Checking for Linearity
pairs(df2, pch = 18, col = "steelblue")

## Describing the data (EDA)
library("psych")
describe(df2)

## Pairs Panel
pairs.panels(df2, smooth = TRUE, scale = FALSE, density = TRUE, ellipses = TRUE, digits = 2, method = "spearman", pch = 20, lm = FALSE, cor = TRUE, jiggle = FALSE, factor = 2, hist.col = "steelblue", show.points = TRUE, rug = TRUE, breaks = "Sturges", cex.cor = 1, wt = NULL, smoother = FALSE, stars = FALSE, ci = FALSE, alpha = 0.05, hist.border = "black", line.col = "purple", ci.col = "light blue")

## Regression = DV ~ IV1 + IV2`
fit <- lm(CSS12_Total ~ MCQ30_Total + GAD_Total, data = df2)
View(fit)
fit

summary(fit)

## using lm.beta to calculate standardized regression coefficients
library(lm.beta)
lm.beta(fit, complete.standardization = TRUE)

confint(fit, level = .95)


# REGRESSION DIAGNOSTICS
plot(fit)

## Breusch-Pagan Test for Homoscedasticity
library(lmtest)
bptest(CSS12_Total ~ MCQ30_Total + GAD_Total, data = df2, studentize = TRUE)

## Running the heteroscedasticity - Consistent Covariance Matrix Estimation
library(sandwich)
VCM <- vcovHC(fit, type="HC3")
VCM

coeftest(fit, vcov.=VCM)
## Multicollinearity
library(car)
vif(fit)

## Added Value Plots
avPlots(fit)

## Residual Plots
residualPlots(fit, type= "rstudent")

infIndexPlot(fit, vars="cook")




# MEDIATION ANALYSIS
process(data= X3v, y = "CSS 12", x = "GAD 7", m = "MCQ 30", model = 4, stand=1, describe = 1, diagnose = 1)



















