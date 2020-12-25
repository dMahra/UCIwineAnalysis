whitewinequality <- read.csv("~/Desktop/whitewinequality.csv", sep=";")
head(whitewinequality)

# resesarch question 1
avgalc = mean(whitewinequality$alcohol)
avgalc

greateralc = whitewinequality[which(whitewinequality$alcohol >= mean(whitewinequality$alcohol)),]
lesseralac = whitewinequality[which(whitewinequality$alcohol < mean(whitewinequality$alcohol)),]
t.test(x = greateralc$quality, y = lesseralc$quality, alternative = 'greater')

roundedalc = round(whitewinequality$alcohol)
boxplot(whitewinequality$quality~roundedalc)

#boxplot here

# resesarch question 2
avgTSD = mean(whitewinequality$total.sulfur.dioxide)
avgTSD
greaterTSD = whitewinequality[which(whitewinequality$total.sulfur.dioxide >= mean(whitewinequality$total.sulfur.dioxide)),]
lesserTSD = whitewinequality[which(whitewinequality$total.sulfur.dioxide < mean(whitewinequality$total.sulfur.dioxide)),]
t.test(x = lesserTSD$quality, y = greaterTSD$quality, alternative = 'greater')

roundedTSD = round(whitewinequality$total.sulfur.dioxide, -1)
boxplot(whitewinequality$quality~roundedTSD)

# boxplot here

cor(whitewinequality$quality,whitewinequality$total.sulfur.dioxide)

#resarch question 3

summary(aov(formula = pH~quality, data = whitewinequality))
wineanova = aov(formula = pH~quality, data = whitewinequality)
qqnorm(wineanova$residuals)
# qq-plot

roundedpH = round(whitewinequality$pH,1)
boxplot(whitewinequality$quality~roundedpH)

# insert boxplot

plot(wineanova,1,las=1)

# residuals vs. fitted plot

#correlation across ingredients

library(corrplot)

cor.table = cor(whitewinequality)
corrplot(cor.table)
corrplot(cor.table, type = 'upper')

#correlationplot

#multivariable linear regression
# first model with all features
model_linear <- lm(quality ~., data = whitewinequality)
summary(model_linear)

# second model with all significant features only
model_linear_sig <- lm(quality ~ volatile.acidity + residual.sugar + density + pH + sulphates + alcohol, data = whitewinequality)
summary(model_linear_sig)

# new model of all interactions

model_linear_inter <- lm(quality ~ volatile.acidity*residual.sugar + volatile.acidity*density + volatile.acidity*pH + volatile.acidity*sulphates + volatile.acidity*alcohol + residual.sugar*density + residual.sugar*pH + residual.sugar*sulphates + residual.sugar*alcohol + density*pH + density*sulphates + density*alcohol + pH*sulphates + pH*alcohol + sulphates*alcohol, data = whitewinequality)
summary(model_linear_inter)

model_linear_inter2<- lm(quality ~ volatile.acidity + residual.sugar + density + pH + sulphates + alcohol + volatile.acidity:density + volatile.acidity:alcohol + volatile.acidity:sulphates + residual.sugar:alcohol + density:alcohol + pH:sulphates , data = whitewinequality)
summary(model_linear_inter2)

# last model of only statistacally significant interactions between ingredients 









