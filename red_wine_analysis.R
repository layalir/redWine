library(ggplot2)
library(corrplot)
#This report investigates Red Wine dataset
df = read.csv("C:/users/layal/Desktop/R Project/wineQualityReds.csv")
dim(df)
str(df)
summary(df)
#This dataset has 1599 observations and 13 numerical variables.
#Header 2: Univariant Plots Section

# Check if there are missing values
d<-subset(df,is.na(df))
dim(d)
# no missing values

# Check if observations are unique, I will remove the column marking the
# sequence number for each observations, it is not needed in my EDA

df$X <- NULL
dim(unique(df)) == dim(df)

# Yes! All observations are unique.
ggplot(aes(x=quality), data = df) +
  geom_histogram()
  
table(df$quality)

# Following I will add two columns to dataset, total acidity and quality
# bucket. Later in this report I will describe how I reach the conclusion
# that total acidity should include citric acid and fixed acidity.
# Also, quality bucket is going to help in the multivariate analysis.

df$total.acidity = df$fixed.acidity+df$citric.acid
df$quality.bucket <- ifelse(df$quality<=5,"Low", "High")


# The quality of the red wine in this data set ranges between 3 and 8.
# There is very small number of wine with very low quality of 3 or very high
# quality of 8. The observations seem to be right skewed with short tail,
# they center around 5.5.

ggplot(aes(x=fixed.acidity), data = df) +
  geom_density(alpha=0.2, fill="blue")

summary(df$fixed.acidity)

#Most of the observations have fixed.acidity of about 7. The distribution
#is right skewed with a tail expanding until about 16.
# Too much fixed acidity will result in tartar wine, this explains why
# there are small number of observations with high amounts of this acid.

# There are no values of zero or 1 so it should be safe to try the log scale.

ggplot(aes(x=log10(fixed.acidity)), data = df) +
  geom_density(alpha=0.2, fill="red") +
  geom_vline(aes(xintercept=mean(log10(fixed.acidity), na.rm=T)),
             color="red", linetype="dashed", size=1)

# The density plot of the log10 of this variable looks "close" to normal
# the mean is 0.91 and is represented using the vertical red line,
# it is not quite at the center because the density is not normal.
summary(log10(df$fixed.acidity))

ggplot(aes(x=volatile.acidity), data = df) +
  geom_density(alpha=0.2, fill = "yellow")

# The density plot is bimodal. Volatile acidity is found in very small
# amounts in red wine, it appears when wine is steamed. More about this
# variable later in this report.

ggplot(aes(citric.acid), data = df) + 
  geom_histogram(binwidth = 0.005)

# Citric acids usualy appear in citris fruit, they give freshness
# to the wine.
# The above histogram of citrict.acid does not show any particular pattern.
# The majoriy of the observations does not include any amount of this acid.

sort(table(df$citric.acid))

ggplot(aes(residual.sugar), data = df) + 
  geom_density(fill="green")

#The density looks right skewed with long tail. Even when transformed
# to log10 (not interesting enough to include in this document), it also
# looks right skewed. Log10 is not useful because most values are very small.
# The description document mentions that
#"wines with greater than 45 grams/liter are considered sweet".
# There are no sweet wines in this sample. The maximum value is 15.5.
summary(df$residual.sugar)

ggplot(aes(chlorides), data = df) + 
  geom_density(fill="orange")

# Similar observations can be seen when plotting the chlorides. Most
# observations have very small amounts of this compound. It is clearly right
# skewed. Chlorides are present in wine from materials used to sterilize
# wine making equipments, detergents are not commonly used to clean the 
# equipments because they leave residue. Based on this reasoning,
# I don't think chloride is major component of wine.

ggplot(aes(free.sulfur.dioxide), data = df) + 
  geom_density(alpha = 0.2, fill = 'blue')

ggplot(aes(total.sulfur.dioxide - free.sulfur.dioxide), data = df) + 
  geom_density(alpha = 0.2, fill = 'blue') +
  xlab("bound sulfur")

summary(df$free.sulfur.dioxide)

ggplot(aes(total.sulfur.dioxide), data = df) + 
  geom_density(alpha = 0.2, fill = 'blue')

# According to the dataset description, the total.sulfur.dioxide is
# a superset of free.sulfur.dioxide and bound form of SO2.
# Most observations have small concentration of free or bound SO2. Only
# very observations have relatively higher concentrations of these materials
# which result in right skewed density plot.

ggplot(aes(sulphates), data = df) +
  geom_density()

# The majority of observations have relatively small amount of Sulphate.
# Even at small amounts suphur can be antioxidant, anti aging and hence,
# it preserves the freshness of the wine.
# The density plot is right skewed. Sulphate contribute to SO2
# substances so it will be interesting to study these materials together.


ggplot(aes(density), data = df) + 
  geom_density() +
  geom_vline(aes(xintercept=mean(density, na.rm=T)),
           color="red", linetype="dashed", size=1)
# Density is normally distibuted with center around the mean of  0.9967
# and sd of 0.0019
# Density depends on alcohol and sugar percentage so it should interesting
# to study these three variables together later in this report.
mean(df$density)
sd(df$density)

ggplot(aes(pH), data = df) +
  geom_density() +
  geom_vline(aes(xintercept=mean(pH, na.rm=T)),
             color="red", linetype="dashed", size=1)

# pH measures the concentration of fixed acidity and citric acidity.
# Volatile acidity is measured using steam distillation process.
# Higher amounts of acid lead to lower values of pH and more acidic taste.
# pH is log scaled by nature.
# pH is normally distributed  with a center of 3.3, and sd of 0.15
# pH for red wine should ranges between 3 and 4, which is the case for
# this dataset. There are 29 exceptions with a pH value lower than 3.
# The majority of these exceptions are of quality 3 and 4.
mean(df$pH)
sd(df$pH)
d <- subset(df, (df$pH < 3))
table(d$quality)


ggplot(aes(alcohol), data = df) +
  geom_density()

# I don't think this is bimodal and it is probably a stretch to just
# call it right skewed.
# Most wines have about 9.5% alcohol, few observations have higher
# percentages of alcohol with a maximum of 15%.
tail(names(sort(table(df$alcohol))), 1)
summary(df$alcohol)

#*********************** Multivariante plots***********************

# I have 13 variables so I will start by plotting the correlation
# coefficient

corr.matrix = cor(df)
corrplot(m, method = 'circle')

# From the plot above, I think it will be useful to look at the following
# relationships:
# fixed.acidity vs citric.acid
# fixed.acidity vs pH (negative)
# volatile.acidity vs citric.acid (negative)
# citric.acid vs pH (negative)
#
# quality vs alcohol
# quality vs sulphates
# quality vs citric.acid
# quality vs. fixed.acidity
# quality vs volatile.acidity (negative)

# density vs fixed acidity
# density vs alcohol (negative)
# density vs citric.acid
# density vs residual sugar
# density vs pH (negative)

# Start by the acidity (fixed, volatile and citric). Recall pH is
# the measurement of strength of acidity, and acidity is a measurement of
# how much acid you have.
# Usually, the higher the amound of acid the smaller the pH.
# Therefore, it is expected to see linear relationship
# between pH and acidity. Volatile acids are not measured by pH
# though. Although volatile acids seem to have a positive relationship
# with pH in the correlation plot above, it is likely a fake
# relationship that is created by other factors.

ggplot(aes(x=pH, y = citric.acid), data = df) +
  geom_point() +
  geom_smooth(method="lm")

cor.test(df$pH,df$citric.acid)

# There is a good negative relationship between pH and fixed acidity.
# The pearson correlation coefficient is -0.54
# It it also interesting to see zero amount of citric acid and different
# values of pH, implying that pH depends on other substances as we will
# see below.

ggplot(aes(x=pH, y = volatile.acidity), data = df) +
  geom_point(color='red') +
  geom_smooth(method="lm")
cor.test(df$pH,df$volatile.acidity)
# There is a weak relationship of 0.23 between volatile acidity and pH.
# The Pearson correlation coefficient is 0.24.
# Again, I think this relationship is fake as pH is not an indication of
# volatile acidity.

ggplot(aes(x=pH, y = fixed.acidity), data = df) +
  geom_point(color='brown') +
  geom_smooth(method="lm")
cor.test(df$pH,df$fixed.acidity)
# In high concentration the fixed acids can give the wine tartaric taste.
# There is strong negative relationship between pH and fixed acidity.
# The pearson correlation coeficient is -0.68
# Fixed acidity is very important in wine industry, it gives each wine its
# own characteristic. Fixed and citric acids are the two factors that
# contribute to pH in this dataset.

ggplot(aes(x=pH, y = fixed.acidity, color=citric.acid), data = df) +
  geom_point() +
  geom_smooth(method="lm")
cor.test(df$pH,(df$fixed.acidity+df$citric.acid))

# The plot above shows that when you have high fixed and citric acids
# amounts the pH will be low and vice versa.
# The sum of these two substances have a - 0.69 correlation coefficient. 

# density vs fixed acidity
# density vs alcohol (negative)
# density vs citric.acid
# density vs residual sugar
# density vs pH (negative)


# Next I will study density. The dataset description says that the main
# components of density is sugar and alcohol, I will test the accuracy
# of this statement.

ggplot(aes(x=density, y = alcohol), data = df) + 
  geom_point(color = 'pink') +
  ylim(8,13) +
  geom_smooth(method='lm')
cor.test(df$density, df$alcohol)
# There is a negative relationship with an R of -0.50 between density
# and alcohol.

ggplot(aes(x=density, y = (residual.sugar)), data = df) + 
  geom_jitter(alpha = 1/5) +
  ylim(0.5, 5) +
  geom_smooth(method='lm')
cor.test(df$density, df$residual.sugar)

# There is a positive weak relationship with as R of 0.36 between
# density and residual.sugar.

ggplot(aes(x=density, y = total.acidity), data = df) + 
  geom_jitter(alpha = 1/5) +
  geom_smooth(method='lm')
cor.test(df$density, df$total.acidity)

# There is a good positive relationship with an R of 0.66 between denstiy
# and total.acidity.

# I conclude that residual sugar doesn't correlate much with density,
# rather alcohol and total acidity seems to be the major contributers.
# I think more statistical analysis are needed to determine if the
# relationship between density and total acidity is real or an artifact
# of other relationships, but I cannot see that by using my current tools

ggplot(aes(x=density, y = total.acidity, color = alcohol), data = df) + 
  geom_point() +
  geom_smooth(method='lm', color='pink')

# This plot shows, again, the nice positive relationship between density,
# and total acidity and a negative one with alcohol.

# quality vs alcohol
# quality vs volatile.acidity (negative)
# quality vs sulphates
# quality vs citric.acid
# quality vs. fixed.acidity

# Note that the lack of observations with quality of 3, 4, 7 and 8 makes
# it hard to make conclusions out of the plots below.

ggplot(aes(x=as.factor(quality), y = alcohol), data = df) + 
  geom_boxplot()

# Quality increases with alcohol, the boxplot shows this with the exception
# of quality 5 which has lower mean of alcohol with respect to quality of 4.
# Quality 5 also has a large number of outliers. There is a positive
# relationship between alcohol and the wine quality with R of
# 0.48.

ggplot(aes(x=as.factor(quality), y = volatile.acidity), data = df) + 
  geom_boxplot()

# As for the volatile acidity, the alcohol mean of each quality category
# is getting smaller with increasing quality. The negative
# relationship can also be shown with R value of -0.39.

# Quality depends weakly on sulphates (R = 0.25), citric.acid (R = 0.23)
# and total.acidity (R = 0.17), as shown below.

ggplot(aes(y=sulphates, x = as.factor(quality)), data = df) + 
  geom_boxplot()
# This is a weak positive relationship with the quartiles (almost) increasing
# with quality and the mean of sulphates for each category also increases.
# The R value for this relationship is 0.25.

ggplot(aes(y=citric.acid, x = as.factor(quality)), data = df) + 
  geom_boxplot()
# Although we can see the positive relationship between citric acid and
# quality, the small number of observations in category 3 is making it 
# hard to make conclusions because the huge quartile of category 3 is
# probably weakening the relationship between citric acid and quality.
# The R value is 0.23.

ggplot(aes(y=fixed.acidity, x = as.factor(quality)), data = df) + 
  geom_boxplot()
# Fixed acidity also has a weak relationship with quality, qualities of 3
# and 8 and the too few observations is affecting the increasing trend.
# This relationship has an R value of 0.17.

summary(df$quality)

cor.test(df$total.acidity, df$quality)

ggplot(aes(x=alcohol, y = volatile.acidity), data = df) +
  geom_jitter(color = 'red', alpha = 1/5) +
  ylim(0.125, 1.2) +
  facet_wrap(~df$quality.bucket)
ggplot(aes(x=volatile.acidity, fill = quality.bucket), data = df) +
  geom_density(alpha=0.2)
# The plots above show other views of the major two factors contributing
# to quality. The first plot shows the low quality win (<=5) vs high
# quality (> 5). As usualy I solved overplotting by adding a jitter.
# The weak relationships do not make it easy to view the relationship
# but one can notice that high quality has relatively more small values
# of volatile acidity and higher numbers of alcohol percentages.
# Second plot is interesting because it "kind of" breaks down the bimodal
# density plot in volatile acidity, it turns out that the higher mod
# belongs to low quality wine.

# Quality, sulphates and alcohol

ggplot(aes(x=alcohol, y = sulphates,
           color = quality.bucket), data = df) +
  geom_jitter(alpha=1/5) +
  ylim(0.3,1.3)

# Recall, that low quality in this plot refers
# to quality of less than or equal to 5, otherwise the wine is
# in the High category.
# It is interesting to see that low quality wine has smaller amount
# of alcohol and also smaller amounts of sulphates.

# Sulphates and chlorides

ggplot(aes(x=sulphates, y = chlorides), data = df) +
  geom_jitter(alpha=1/10) +
  ylim(0, 0.2) +
  xlim(0.2, 1.25) +
  geom_smooth(method='lm')

# There is a weak relationship between sulphates and chlorides with
# R = 0.37. I'm not a chemist but I looked it up and there is no chemical
# relationship between the two compounds so I think that this relationship
# is fake.
cor.test(df$sulphates, df$chlorides)

# Free, bound and total sulfur dioxide.

ggplot(aes(x=free.sulfur.dioxide, y= total.sulfur.dioxide),
       data = df) +
  ylim(0, 150) +
  xlim(0, 60) +
  geom_jitter(alpha = 1/10, color = "purple")

# Recall that total sulfur dioxide is a superset of free.sulfur.dioxide
# and bound sulfur dioxide (per the dataset description).
# The plot confirms this because we don't see a point where
# free sulfur dioxide is larger than the total.
# Seems like bound sulfur dioxide composed most of the total sulfur dioxide
# because the range of sulphates is small. 

ggplot(aes(x=sulphates, y= total.sulfur.dioxide),
       data = df) +
    ylim(0, 150) +
    xlim(0.25, 1.5) +
  geom_jitter(alpha = 1/10, color = "red") +
  geom_smooth(method="lm")

# The plot also shows that the majority of observations have small
# sulphates value mainly between 0.4 to 0.8. It also shows a number
# of observations with high total.sulfur.dioxide value and still very low
# sulphate value. Therefore, there is no obvious linear relationship between
# the two variables although their description implies a relationship.
# I plot the smooth line and it confirms my expectations.
