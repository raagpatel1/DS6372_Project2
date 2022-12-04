---
title: "MSDS_6372_Project2"
author: "Nicole Assenza, Raag Patel, Michael Shulte"
date: "`r Sys.Date()`"
output:
  #html_document:
    #theme: cerulean
    #highlight: textmate
  github_document:
  toc: FALSE
  toc_depth: 3
  fig_width: 7
  fig_height: 5
  dev: "png"
  df_print: "default"
  includes: NULL
  md_extensions: NULL
  hard_line_breaks: TRUE
  pandoc_args: NULL
  html_preview: FALSE
  keep_html: FALSE
always_allow_html: true
editor_options: 
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#### Loading the necessary R libraries for the analysis
```{r message = FALSE}
library(knitr)
library(rmarkdown)
library(GGally)
library(epitools)
library(MASS)
library(tidyverse)
library(car)
library(caret)
library(glmnet)
library(ROCR)
library(pROC)
library(rpart)
library(rpart.plot)
library(rattle)
library(ResourceSelection)
library(dplyr)
library(ggplot2)
library(gplots)
library(broom)
library(e1071)
library(aplore3)
```

```{r message = FALSE}
# Turn off scientific notation
options(scipen = 100, digits = 4)
```

## Global Longitudinal Study of Osteoporosis in Women
### Objective: Assessing risk factors and predicting if a woman with osteoperosis will have a bone fracture within the first year after joining the study.
#### The study has enrolled over 60,000 women aged 55 and older in ten countries. The major goals of the study are to use the data to provide insights into the management of fracture risk, patient experience with prevention and treatment of fractures and distribution of risk factors among older women on an international scale over the follow up period. The outcome variable is any fracture in the ﬁrst year of follow up. www.outcomes-umassmed.org/glow
#### In order to have a data set of a manageable size, n = 500, the dataset included into the R package ‘aplore3’ the fractures have been over sampled and the non-fractures were under sampled. The description of each variable in the data set is on line 95. 

## Objective 1 methodology:

1. Understand the data
2. EDA
3. Feature Selection (Penalized Logistic Regression, Stepwise/forward/backward, Manual, PCA)
4. Split the data to Training and Test set
5. Fit Logistic Regression models
6. Interpret the models, including hypothesis testing and confidence intervals
7. Conclusion of Obejtive I.


## Objective 2 methodology:

8. Fit a complex model
9. Fit LDA and QDA models
10. Fit Random Forrest and Decision tree models
11. Summary table
12. Final model recommendation



## Read the data
```{r,fig.align='center',out.extra='angle=90', message = FALSE}
bonemed_df = glow_bonemed
attach(bonemed_df)
bonemed_df_sample = sample_n(bonemed_df, 5)
knitr::kable(bonemed_df_sample, "html")
```

## Data Description
* __sub_id__: Identification Code (1 - n)

* __site_id__: Study Site (1 - 6)

* __phy_id__: Physician ID code (128 unique codes)

* __priorfrac__: History of Prior Fracture (1: No, 2: Yes)

* __age__: Age at Enrollment (Years)

* __weight__: Weight at enrollment (Kilograms)

* __height__: Height at enrollment (Centimeters)

* __bmi__: Body Mass Index (Kg/m^2)

* __premeno__: Menopause before age 45 (1: No, 2: Yes)

* __momfrac__: Mother had hip fracture (1: No, 2: Yes)

* __armassist__: Arms are needed to stand from a chair (1: No, 2: Yes)

* __smoke__: Former or current smoker (1: No, 2: Yes)

* __raterisk__: Self-reported risk of fracture (1: Less than others of the same age, 2: Same as others of the same age, 3: Greater than others of the same age)

* __fracscore__: Fracture Risk Score (Composite Risk Score)

* __fracture__: Any fracture in first year (1: No, 2: Yes)

* __bonemed__: Bone medications at enrollment (1: No, 2: Yes)

* __bonemed_fu__: Bone medications at follow-up (1: No, 2: Yes)

* __bonetreat__: Bone medications both at enrollment and follow-up (1: No, 2: Yes)


```{r}
#set random seed
set.seed(329)
```

#### Check for missing values in each column (NA as well as empty strings).
```{r}
missing_df = as.data.frame(sapply(bonemed_df, function(x) sum(is.na(x))))
colnames(missing_df) = c("missing values")
knitr::kable(missing_df, "html")
empty_string_df = as.data.frame(sapply(bonemed_df, function(x) sum(x == "")))
colnames(empty_string_df) = c("empty string")
knitr::kable(empty_string_df, "html")
```

```{r, warning=FALSE}
# Function to Identify different characteristics of the data frame 
# Getting a concise summary of the dataframe: str()
# Listing the column labels of the dataframe: colnames()
# Size of the dataset: dim()
# # Verify if there is any negative values in the dataset
dfinfo = function(df_name)
  {
  df_structure = str(df_name)
  df_colnames = colnames(df_name)
  df_dimensions = print(paste('Number of observations:', dim(bonemed_df)[1], ',  Number of features:', dim(bonemed_df)[2])) 
  df_duplicates = print(paste('Number of duplicated entries in the dataframe: ',sum(duplicated(bonemed_df))))
  num_cols = df_name %>% dplyr::select(where(is.numeric)) %>% colnames()
  df_neg = print(paste("Negative values in the continuous variables:",  sum(df_name[num_cols]<0)))
  
  outparam = list(df_structure, df_colnames, df_dimensions, df_duplicates, df_neg)
  return (outparam)
}
```
```{r}
dfinfo(bonemed_df)
```

### Observations:
* The data set is comprised of 500 observations and 18 variables
* There are numerical and categorical variables in the data set
* There are no missing values or empty strings in the data set
* No negative values in the data set
* No duplicated records
* 'fracture' is the dependent variable


#####################################################################################
#                                Exploratory Data Analysis                          #
#####################################################################################
```{r pair plots, fig.align='center',out.extra='angle=90', message = FALSE}
num_cols = bonemed_df %>% dplyr::select(where(is.numeric)) %>% colnames()
num_cols = num_cols[-c(1,2,3)]
pair_plot = c(num_cols, 'fracture')
pair_plot = pair_plot
ggpairs(bonemed_df[,pair_plot],aes(color=fracture, alpha = 0.5))
```

### Observations:
* Height for fracture 'No' and 'Yes' levels seem to be normally distributed.
* Weight and height are linearly related. The taller the patient is, the higher her weight is.
* __We can observe two extreme observations in the data set__: 

(1) Women with less than 75kg of weight but close to 2m tall. It is not impossible but not usual either.
(2) Women shorter than 140 cm but around 75kg. This woman maybe a "little person".

* Weight and bmi are strongly positively linearly correlated for both factor levels.
* Age and fracscore are strongly positively linearly correlated for both fracture factor levels.
* There are more "No" fractures in the first year observations than "Yes"
* There is no true separation between the factor levels for any of the continuous variables. This means that fracture can happen at any age, weight, height, bmi, or fracscore. None of these variables are influencing fracture.


```{r}
# Summary statistics
t(aggregate(.~ fracture,data=bonemed_df,summary))
```

### Observations:
* The minimum age for "No" fracture in the first year is 55. The minimum age for "Yes" fracture in the first year is 56.
* The maximum age for "No" fracture in the first year is 90. The maximum age for "Yes" fracture in the first year is 89.
* The median age for "No" fracture in the first year is 66. The median age for "Yes" fracture in the first year is 72.
* The mean age for "No" fracture in the first year is 67.49. The mean age for "Yes" fracture in the first year is 71.79.
* Age is a likely is good predictor of fracture in the first year since there is a observable difference in median and mean age of having a fracture. 

* The minimum weight for "No" fracture in the first year is 39.9. The minimum weight for "Yes" fracture in the first year is 45.8.
* The maximum weight for "No" fracture in the first year is 127. The maximum weight for "Yes" fracture in the first year is 124.7.
* The median weight for "No" fracture in the first year is 68. The median weight for "Yes" fracture in the first year is 68.
* The mean weight for "No" fracture in the first year is 72.17. The mean weight for "Yes" fracture in the first year is 70.79.
* Weight does not seem to be a significant variable for predicting fracture in the first year as the median and mean weight values are very similar to the factor levels.

* The minimum height for "No" fracture in the first year is 142. The minimum height for "Yes" fracture in the first year is 134.
* The maximum height for "No" fracture in the first year is 199. The maximum height for "Yes" fracture in the first year is 178.
* The median height for "No" fracture in the first year is 162. The median height for "Yes" fracture in the first year is 160.
* The mean height for "No" fracture in the first year is 161.9. The mean height for "Yes" fracture in the first year is 159.9.
* Height seems to be a good predictor for fracture in the first year.

* The minimum bmi for "No" fracture in the first year is 14.88. The minimum bmi for "Yes" fracture in the first year is 17.04.
* The maximum bmi for "No" fracture in the first year is 49.08 The maximum bmi for "Yes" fracture in the first year is 44.04.
* The median bmi for "No" fracture in the first year is 26.37. The median bmi for "Yes" fracture in the first year is 26.43.
* The mean bmi for "No" fracture in the first year is 27.50. The mean bmi for "Yes" fracture in the first year is 27.71.
* BMI does not seem to be a significant predictor for fracture in the first year.

* The minimum Fracture Risk Score for "No" fracture in the first year is 0. The minimum Fracture Risk Score for "Yes" fracture in the first year is 0.
* The maximum Fracture Risk Score for "No" fracture in the first year is 11. The maximum Fracture Risk Score for "Yes" fracture in the first year is 9.
* The median Fracture Risk Score for "No" fracture in the first year is 3. The median Fracture Risk Score for "Yes" fracture in the first year is 5.
* The mean Fracture Risk Score for "No" fracture in the first year is 3.317. The mean Fracture Risk Score for "Yes" fracture in the first year is 4.840.
* Fracture Risk Score seems to be a significant predictor for fracture in the first year.

#####################################################################################
#                               Categorical data plots                              #
#####################################################################################
```{r,fig.align='center',out.extra='angle=90', fig.dim = c(8, 6)}

cat_cols = bonemed_df %>% dplyr::select(where(is.factor)) %>% colnames()

# Plot all categorical variables
for (c in cat_cols)
{
  cat_plot = bonemed_df %>% ggplot(aes(x= .data[[c]], group = 1)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    labs(y = "Percent") +
    scale_y_continuous(labels = scales::percent) + theme(legend.position = "none") +
    ggtitle(paste(c, "Categorical Analysis")) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) # +
    egg::ggarrange(cat_plot, ncol=2) 
}
```

### Observations:
* 75% of the observations have no prior history of fracture. 25% of the observations have prior history of fracture.
* 81% of the observations have no menopause before age 45. 19% of the observations have menopause before age 45.
* 87% of the subjects' mother had no hip fracture. 13% of the subjects' mother had hip fracture.
* 62% of the subjects do not need arms to stand from a chair. 38% of the subjects need arms to stand from a chair.
* 93% of the subjects don't smoke. 7% of the subjects smoke are former or current smoker.
* 33.4% of the subjects reported less risk of fractures than others of the same age. 37.2% of the subjects reported the same risk of fractures than others of the same age. 29.4% of the subjects reported greater risk of fractures than others of the same age.
* 75% of the patients do not have any fractures in first year. 25% of the patients have any fractures in first year.
* 74% of the subjects have enrolled to Bone medications. 26% of the subjects have not enrolled to Bone medications.
* 72% of the subjects did not require bone medications at follow-up. 28% of the subjects required bone medications at follow-up.
* 76% of the patients did not receive bone medications at either enrollment or follow-up. 24% of the patients received bone medications both at enrollment and follow-up.



#####################################################################################
#                     Bi-variate analysis with Fracture variable                    #
#####################################################################################
```{r,fig.align='center',out.extra='angle=90'}

num_cols = bonemed_df %>% dplyr::select(where(is.numeric)) %>% colnames()
bivar_plot = num_cols[c(-1, -2, -3)]


for (i in bivar_plot)
{
multibox = bonemed_df %>%
  ggplot(aes(x=fracture, y = .data[[i]])) +
  geom_boxplot(fill = "sandybrown", color = "black") + 
  xlab("Fracture") +
  ylab(i) + stat_summary(fun=mean, geom="point", shape=20, size=7, color="red", fill="red") +
  ggtitle(paste(i, "vs Fracture bi-variate analysis")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Oranges")  
egg::ggarrange(multibox, ncol=2)
}
```

### Observations:
* We can see an increase in median and mean age for those who had any fractures in the first year.
* Median and mean weight for both fracture levels are very similar.
* The median and mean height are lower for those who had fracture in the first year. This is likely because the bone density is reduced and the subjects shrunk.
* Median and mean BMI is very similar for both factor levels.
* The mean and median fracture risk score is higher for those who had any fracture in the first year.


#####################################################################################
#                                     Correlation plot                              #
#####################################################################################
```{r correlation plot}
corr_df = bonemed_df[,c('age', 'weight', 'height', 'bmi', 'fracscore')]
cont_var.cor = cor(corr_df)
heatmap.2(cont_var.cor,col=redgreen(75), 
          density.info="none", trace="none", dendrogram=c("row"), 
          symm=F,symkey=T,symbreaks=T, scale="none")
```

### Observations:
* 'weight' and 'bmi' are more similar to each other therefore they form a cluster
* 'height', 'weight' and 'bmi' are also similar but more distant from each other. Still can form a cluster.
* 'age' and 'fracscore' are also similar to each other and can form a cluster


```{r}
# Label encoding Yes=1; No=0
bonemed_df$fracture.num=ifelse(bonemed_df$fracture=="Yes",1,0)
```


#####################################################################################
#                                       Loess plots                                 #
#####################################################################################
```{r, Loess plot}
num_cols = bonemed_df %>% dplyr::select(where(is.numeric)) %>% colnames()
loess_plot = num_cols[c(-1, -2, -3, -9)]

for (i in loess_plot)
{
loess = bonemed_df %>% 
ggplot(aes(x=.data[[i]],y=fracture.num))+
geom_point()+
geom_smooth(formula = y ~ x, method="loess")+
theme(plot.title = element_text(hjust = 0.5)) +
ggtitle(paste(i, "vs fracture loess smoothing"))
egg::ggarrange(loess, ncol=2)
summary(bonemed_df)
}
```

### Observations:
* The Loess plots show that the 'age' and 'fracscore' variables have some 'S' curve like logistic model. The curves are trending down as the observations after age of 85 years is sparse. The  'fracscore' is also trending down as there are less observations in the higher score categories.
* The other continuous predictors don't seem to be important predicting fracture in the first year.


#####################################################################################
#                   Loess plots to investigate interactions                         #
#####################################################################################
```{r, Loess plot interaction}
num_cols = bonemed_df %>% dplyr::select(where(is.numeric)) %>% colnames()
loess_plot = num_cols[c(-1, -2, -3, -9)]

for (j in cat_cols[-7])
{
  for (i in loess_plot)
  {
  plot1 = ggplot(bonemed_df,aes(x=.data[[i]],y=fracture.num,colour=.data[[j]]))+geom_point()+
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle(paste(i, "&", j, " interaction with fracture"))+
  geom_smooth(formula = y ~ x, method="loess",size=1,span=1.5)+facet_wrap(~.data[[j]])
  ylim(-.2,1.2)
  show(plot1)
  }
}
```

## Observations:
* There is a interaction with weight and priorfrac
* There is a interaction with bmi and priorfrac
* There is a interaction with fracscore and priorfrac
* There is a interaction with weight and raterisk
* There is a interaction with bmi and raterisk
* There is a interaction with weight and bonemed
* There is a interaction with bmi and bonemed
* There is a interaction with weight and bonemed_fu
* There is a interaction with bmi and bonemed_fu
* There is a interaction with weight and bonetreat
* There is a interaction with bmi and bonetreat
* This information will be useful building the model in Objective 2.


#####################################################################################
#                    Split the Data to Train and Test sets (85%-15%)               #
#####################################################################################
```{r train test set}
bonmed_df_split = bonemed_df[,-c(1, 2, 3)]
index=sample(1:dim(bonmed_df_split)[1],round(dim(bonmed_df_split)[1]*0.85),replace=F)
train = bonmed_df_split[index,]
test = bonmed_df_split[-index,]
```

#####################################################################################
#                                   Feature Selection                               #
#####################################################################################

## Manual / Intuition selection
```{r, feature selection manual selection}
# Fit each variable separately and check the p-value for significance

# priorfrac
simple.log.priorfrac=glm(fracture~priorfrac,family=binomial(link='logit'),data=train)
simple.log.priorfrac.sum = summary(simple.log.priorfrac)

# age
simple.log.age=glm(fracture~age,family=binomial(link='logit'),data=train)
simple.log.age.sum = summary(simple.log.age)

# weight
simple.log.weight=glm(fracture~weight,family=binomial(link='logit'),data=train)
simple.log.weight.sum = summary(simple.log.weight)

# height
simple.log.height=glm(fracture~height,family=binomial(link='logit'),data=train)
simple.log.height.sum = summary(simple.log.height)

# bmi
simple.log.bmi=glm(fracture~bmi,family=binomial(link='logit'),data=train)
simple.log.bmi.sum = summary(simple.log.bmi)

# premeno
simple.log.premeno=glm(fracture~premeno,family=binomial(link='logit'),data=train)
simple.log.premeno.sum = summary(simple.log.premeno)

# momfrac
simple.log.momfrac=glm(fracture~momfrac,family=binomial(link='logit'),data=train)
simple.log.momfrac.sum = summary(simple.log.momfrac)

# armassist
simple.log.armassist=glm(fracture~armassist,family=binomial(link='logit'),data=train)
simple.log.armassist.sum = summary(simple.log.armassist)

# smoke
simple.log.smoke=glm(fracture~smoke,family=binomial(link='logit'),data=train)
simple.log.smoke.sum = summary(simple.log.smoke)

# raterisk
simple.log.raterisk=glm(fracture~raterisk,family=binomial(link='logit'),data=train)
simple.log.raterisk.sum = summary(simple.log.raterisk)

# fracscore
simple.log.fracscore=glm(fracture~fracscore,family=binomial(link='logit'),data=train)
simple.log.fracscore.sum = summary(simple.log.fracscore)

# bonemed
simple.log.bonemed=glm(fracture~bonemed,family=binomial(link='logit'),data=train)
simple.log.bonemed.sum = summary(simple.log.bonemed)

# bonemed_fu
simple.log.bonemed_fu=glm(fracture~bonemed_fu,family=binomial(link='logit'),data=train)
simple.log.bonemed_fu.sum = summary(simple.log.bonemed_fu)

# bonetreat
simple.log.bonetreat=glm(fracture~bonetreat,family=binomial(link='logit'),data=train)
simple.log.bonetreat.sum = summary(simple.log.bonetreat)
```


```{r, print result of manual selection}
# priorfrac
simple.log.priorfrac.sum$coefficients

# age
simple.log.age.sum$coefficients

# weight
simple.log.weight.sum$coefficients

# height
simple.log.height.sum$coefficients

# bmi
simple.log.bmi.sum$coefficients

# premeno
simple.log.premeno.sum$coefficients

# momfrac
simple.log.momfrac.sum$coefficients

# armassist
simple.log.armassist.sum$coefficients

# smoke
simple.log.smoke.sum$coefficients

# raterisk
simple.log.raterisk.sum$coefficients

# fracscore
simple.log.fracscore.sum$coefficients

# bonemed
simple.log.bonemed.sum$coefficients

# bonemed_fu
simple.log.bonemed_fu.sum$coefficients

# bonetreat
simple.log.bonetreat.sum$coefficients
```

### Observations:
* We can see that the following variables are statistically significant since p-value < 0.05:
 1. priorfracYes
 2. age
 3. height
 4. armassistYes
 5. rateriskSame
 6. rateriskGreater
 7. fracscore
 8. bonemedYes
 9. bonemed_fuYes
 10. bonetreatYes
 
 This result is in-line with what we have observed through EDA for the continuous variables. 
 Next, let's fit all the variables and observe the effect and see how it is changing the significance of the predictors.


## Fit all variables at the same time to check effects 
```{r, all variables manual}
multi_var.log=glm(fracture~priorfrac+age+weight+height+bmi+premeno+momfrac+armassist+smoke+raterisk+fracscore+bonemed+
                     bonemed_fu+bonetreat,family=binomial(link='logit'),data=train)
multi_var.log.sum = summary(multi_var.log)
multi_var.log.sum$coefficients
```


### Observations:
* Fitting all variables to the logistic regression model, it shows that only 'rateriskGreater', 'bonmed', 'bonemed_fu' and 'bonetreat' are statistically significant. 

Let's test other feature selection methods as well.

## Stepwise selection

```{r, stepwise}
bonemed_df.step = train[,c('priorfrac', 'age', 'weight', 'height', 'bmi', 'premeno', 'momfrac', 'armassist', 'smoke', 'raterisk', 'fracscore', 
                                'bonemed', 'bonemed_fu', 'bonetreat', 'fracture')]
step.full.log = glm(fracture~.,family=binomial(link='logit'),data=bonemed_df.step)
step.log = step.full.log %>% stepAIC(trace=FALSE)
```

```{r}
summary(step.log)
exp(cbind("Odds ratio" = coef(step.log), confint.default(step.log, level = 0.95)))
```

## Observations and interpretations of the Stepwise selection coefficients:
* Running a stepwise selection to identify the predictors the process has selected 10 out of the 18 variables to be included into the model:
* priorfrac
* age
* weight
* bmi
* momfrac
* armassist
* raterisk
* bonemed
* bonemed_fu
* bonetreat
* Based on the summary statistics result at alpha 0.05 the priorfracYes, weight, bmi, rateriskGreater, bonemed_fuYes and bonetreatYes are statistically significant.
* Let's interpret the statistically significant variables:

* __Holding all other variables constant, the odds of a patient having fracture in the first year with a prior history of fracture is 1.96112 times higher than the odds for those who did not have fracture in the past.__ 

* __Holding all other variables constant, for a 10 kg of increase in weight, the odds of a patient having fracture in the first year increases by a factor of 9.5677.__ 

* __Holding all other variables constant, for one unit of increase in BMI, the odds of a patient having fracture in the first year increases by a factor of 1.12946.__ 

* __Holding all other variables constant, the odds of a patient having fracture in the first year with a greater rate risk is 1.98272 times higher than the odds for those whose rate risk is less than other patients.__ 


* __Holding all other variables constant, the odds of a patient having fracture in the first year with a greater rate risk is 1.98272 times higher than the odds for those whose rate risk is less.__ 

* __Holding all other variables constant, the odds of a patient having fracture in the first year who took bone medication at the follow-up visits is 3.58715 times higher than the odds for those who did not take the medication at the time of follow-up visits.__ This is counter intuitive as we would expect that someone who takes bone medication will have a lower or equal odds of fracture then those who don't. This indicates that there is a confounding variable that is impacted the treatment in the study. The imbalanced fractures classes could attribute to this interpretation.

* __Holding all other variables constant, the odds of a patient having fracture in the first year who took bone medication at both enrollment and at the follow-up visits is 0.09230 times higher than the odds for those who did not take the medication at the time of enrollment and follow-up visits.__ 

* __Confidence intervals__: The confidence intervals for Odds ratios indicates the following:
1. The variable 'bonetreatYes' and 'weight' are the only variables which helps mitigating fracture in the first year after enrolling into the study. This can be seen from the confidence interval and the odds ratio. The odds ratio is below 1 therefore bonetreatYes and weight are actually shows that the odds of having a fracture in the first year is lower than for those who don't take the bone treatment medication at enrollment and follow-ups or have higher weight. The confidence interval (both bounds) remains below 1 hence we can conclude that bone treatment medication at enrollment and follow-ups and lower weight is effective protecting against fracture in the first year.
2. Age is a possible mitigating factor but not significant as the the upper bound of the confidence interval is slightly above 1.

## PCA
```{r, PCA}
# Let's use PCA to see if the continuous variables separate or not

num_cols = train %>% dplyr::select(where(is.numeric)) %>% colnames()
pca_var = num_cols[c(-6)]
pca_df = train[pca_var]

pc.result=prcomp(pca_df,scale.=TRUE)
pc.scores=pc.result$x
pc.scores=data.frame(pc.scores)
pc.scores$fracture=train$fracture

#plot the first few pc's
ggplot(data = pc.scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=fracture), size=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("PC1 and PC2 on Fracture")

# Let's check lower PCs
ggplot(data = pc.scores, aes(x = PC1, y = PC3)) +
  geom_point(aes(col=fracture), size=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("PC1 and PC3 on Fracture")

ggplot(data = pc.scores, aes(x = PC1, y = PC4)) +
  geom_point(aes(col=fracture), size=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("PC1 and PC4 on Fracture")

ggplot(data = pc.scores, aes(x = PC1, y = PC5)) +
  geom_point(aes(col=fracture), size=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("PC1 and PC5 on Fracture")

ggplot(data = pc.scores, aes(x = PC2, y = PC3)) +
  geom_point(aes(col=fracture), size=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("PC2 and PC3 on Fracture")

ggplot(data = pc.scores, aes(x = PC2, y = PC4)) +
  geom_point(aes(col=fracture), size=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("PC2 and PC4 on Fracture")

ggplot(data = pc.scores, aes(x = PC2, y = PC5)) +
  geom_point(aes(col=fracture), size=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("PC2 and PC5 on Fracture")

ggplot(data = pc.scores, aes(x = PC3, y = PC4)) +
  geom_point(aes(col=fracture), size=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("PC3 and PC4 on Fracture")

ggplot(data = pc.scores, aes(x = PC3, y = PC5)) +
  geom_point(aes(col=fracture), size=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("PC3 and PC5 on Fracture")

ggplot(data = pc.scores, aes(x = PC4, y = PC5)) +
  geom_point(aes(col=fracture), size=1)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("PC4 and PC5 on Fracture")

par(mfrow=c(1,2))
eigenvals = (pc.result$sdev)^2
plot(1:5,eigenvals/sum(eigenvals),type="l",main="Scree Plot",ylab="Prop. Var. Explained")
cumulative.prop = cumsum(eigenvals/sum(eigenvals))
plot(1:5,cumulative.prop,type="l",main="Cumulative proportion",ylim=c(0,1))
```

```{r, loess for PCA}
pca_colnames = colnames(pc.scores)[-6]
for (i in pca_colnames)
{
loess_pca = pc.scores %>% 
ggplot(aes(x=.data[[i]],y=train$fracture.num))+
geom_point()+
geom_smooth(formula = y ~ x, method="loess")+
theme(plot.title = element_text(hjust = 0.5)) +
ggtitle(paste(i, " test"))
egg::ggarrange(loess_pca, ncol=2)
}
```


## Observations:
* The levels in the PCA graphs are completely intermixed. There is no obvious separation in the PCA plots between the class levels.
* The Scree Plot shows that if we include PC1 that will explain ~50% of the variation of the dataset
* The cumulative proportion plot helps us see that PC1, PC2, PC3 combined explains the variation ~98% but if we add PC4 than 100% of the variation is explained.
* This lets us conclude that we need 'age', 'weight', 'height' and 'bmi' to explain 100% of the variation in the data.
* The loess plot of PCA shows that PC1 and PC5 are the only continuous variables which could be important for predicting fracture in the first year.


## Penalized logistic regression (LASSO)

```{r, lasso feature selection}
# 5-fold cross validation
cv.lasso = trainControl(
  method = "repeatedcv", 
  number = 5,
  repeats = 10,
  savePredictions = TRUE,
  summaryFunction=mnLogLoss,
  classProbs = TRUE
)

lasso.logreg.mod = train(
  fracture ~  priorfrac+age+weight+height+bmi+premeno+momfrac+armassist+smoke+raterisk+fracscore+bonemed+bonemed_fu+bonetreat,
  data = train,
  method = "glmnet",
  family = "binomial",
  trControl = cv.lasso,
  metric = "logLoss")

plot(lasso.logreg.mod)
coef(lasso.logreg.mod$finalModel,lasso.logreg.mod$finalModel$lambdaOpt)

# Final LASSO model refit with glm
lasso.mod.final=glm(fracture ~  priorfrac+age+height+bmi+premeno+momfrac+armassist+smoke+raterisk+fracscore+bonemed+bonemed_fu+bonetreat, 
                    family = binomial(link='logit'), data = train)

summary(lasso.mod.final)

# ODD ratios for interpretation
exp(cbind("Odds ratio" = coef(lasso.mod.final), confint.default(lasso.mod.final, level = 0.95)))
```

## Observations and interpretations of the LASSO coeffficients:
* For the prediction we used the LASSO penalized regression model which has selected 15 variables out of the 18 to be included into the logistic regression model.
* Based on the summary statistics at alpha = 0.05 only rateriskGreater, bonemedYes, bonemed_fuYes and bonetreatYes variables are statistically significant.
* Let's interpret the statistically significant variables:

* __Holding all other variables constant, the odds of a patient having fracture in the first year with a greater rate risk is 1.93647 times higher than the odds for those whose rate risk is less than other patients.__ 

* __Holding all other variables constant, the odds of a patient having fracture in the first year who took bone medication at enrollment is 3.84105 times higher than the odds for those whose did not take the medication prior to enrollment.__ This is counter intuitive as we would expect that someone who takes bone medication will have a lower or equal changes of fracture than those who don't. This indicates that there is a confoundinding variable that has impacted the treatment in the study. The imbalanced fractures classes could attribute to this interpretation as well.

* __Holding all other variables constant, the odds of a patient having fracture in the first year who took bone medication at the follow-up visits is 3.63421 times higher than the odds for those who did not take the medication at the time of follow-up visits.__ This is counter intuitive as we would expect that someone who takes bone medication will have a lower or equal odds of fracture then those who don't. This indicates that there is a confounding variable that is impacted the treatment in the study. The imbalanced fractures classes could attribute to this interpretation.

* __Holding all other variables constant, the odds of a patient having fracture in the first year who took bone medication at both enrollment and at the follow-up visits is 0.09057 times higher than the odds for those who did not take the medication at the time of enrollment and follow-up visits.__ 

* __Confidence intervals__: The confidence intervals for Odds ratios indicates the following:
1. The variable 'bonetreatYes' is the only one which helps mitigating fracture in the first year after enrolling into the study. This can be seen from the confidence interval and the odds ratio. The odds ratio is below 1 therefore bonetreatYes is actually shows that the odds of having a fracture in the first year is lower than for those who don't take the bone treatment medication at enrollment and follow-ups. The confidence interval (both bounds) remains below 1 hence we can conclude that bone treatment medication at enrollment and follow-ups is effective protecting against fracture in the first year.
2. Height is a possible mitigating factor but not significant as it the upper bound of the confidence interval is slightly above 1.
3. BMI has similar interpretation to Height from a confidence interval perspective. 


#####################################################################################
#                                       Predictions                                 #
#####################################################################################

```{r, vif function}
# VIF score function
vif_score = function(model_name)
{
  vif_values = vif(model_name)
  barplot(vif_values, main = 'VIF Values', horiz = TRUE, col="blue", xlim = c(0,12))
  abline(v=10, col="red")
  return (vif_values)
}
```

```{r, assumptions function}
# Assumptions function
outlier_check = function(model_name)
{
  # Outliers / influential points
  plot(model_name, which = 4, id.n = 3)
  mod.final.data = augment(model_name) %>% mutate(index = 1:n()) 
  mod.final.data %>% top_n(3, .cooksd)

  standardized_plot = ggplot(mod.final.data, aes(index, .std.resid)) + 
  geom_point(aes(color = train$fracture), alpha = .5) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Standardized Residual Plot")
  
  show(standardized_plot)
  
  list_influence = mod.final.data %>% filter(abs(.std.resid) > 3)
  
  return (list_influence)
}
```

#####################################################################################
#                                       LASSO                                       #
#####################################################################################
```{r, LASSO Predict}

# Predicting on the final LASSO model TRAINing data
lasso.mod.final.pred.train = predict(lasso.mod.final, newdata = train, type = "response")

# Predicting on the final LASSO model TEST data
lasso.mod.final.pred.test = predict(lasso.mod.final, newdata = test, type = "response")

################ LASSO ROC Curve ######################################
lasso.roc=roc(response=test$fracture,predictor=lasso.mod.final.pred.test,levels=c("No","Yes"))
plot(lasso.roc,print.thres="best", main='LASSO ROC curve')

# Calculate Area Under the Curve and AIC score
AUC = auc(test$fracture, lasso.mod.final.pred.test)
print(paste("Area Under the Cuve: ", AUC))
print(paste("LASSO AIC Score: ", AIC(lasso.mod.final)))

#Set cut off value for labels
cutoff.lasso = 0.212

# Confusion matrix train
class.lasso.final.train = factor(ifelse(lasso.mod.final.pred.train>cutoff.lasso,"Yes","No"),levels=c("No","Yes"))

#Confusion Matrix for LASSO - Train
print("Confusion matrix for LASSO TRAINING")
confusionMatrix(table(class.lasso.final.train,train$fracture), positive = "Yes")


#Confusion Matrix for LASSO - Test
print("Confusion matrix for LASSO TEST")
class.lasso.final.test=factor(ifelse(lasso.mod.final.pred.test>cutoff.lasso,"Yes","No"),levels=c("No","Yes"))
confusionMatrix(table(class.lasso.final.test,test$fracture), positive = "Yes")

# Create variables to store metrics
lasso.final.test.accuracy = confusionMatrix(table(class.lasso.final.test,test$fracture), positive = "Yes")$overall[1]
lasso.final.test.sensitivity = confusionMatrix(table(class.lasso.final.test,test$fracture), positive = "Yes")$byClass[1]
lasso.final.test.specificity = confusionMatrix(table(class.lasso.final.test,test$fracture), positive = "Yes")$byClass[2]

##### Store model results for comparison ####
  Model_name = c('LASSO')
  AUC_score = c(AUC)
  Test_Accuracy  = c(lasso.final.test.accuracy)
  Test_Sensitivity  = c(lasso.final.test.sensitivity)
  Test_Specificity  = c(lasso.final.test.specificity)
  AIC  = c(AIC(lasso.mod.final))
  Threshold = c(cutoff.lasso)
```

## LASSO Logistic Regression Assumptions
```{r, assumtion logreg}
# Checking logistic regression model assumptions

# 1. There is a linear relationship between the logit of the outcome and each predictor variables.
# Select only numeric predictors
lasso.logit.assumption.cont.vars = train[,-16] %>% dplyr::select_if(is.numeric) 
predictors = colnames(lasso.logit.assumption.cont.vars)


# Bind the logit and tidying the data for plot
lasso.logit.assumption.cont.vars = lasso.logit.assumption.cont.vars %>%   mutate(logit = log(lasso.mod.final.pred.train/(1-lasso.mod.final.pred.train))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(lasso.logit.assumption.cont.vars, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# 2. Multicollinearity (re-fit the model with only continous variables as VIF does not make sense on categorical data)
lasso.mod.final.vif=glm(fracture ~  age+height+bmi+fracscore, family = binomial(link='logit'), data = train)
vif_score(lasso.mod.final.vif)

# 3. Outlier check
outlier_check(lasso.mod.final)
```


## Observations about assumptions:
* __The Response Variable is Binary__: 'Fracture' as a response variable is a factor with binary levels (Yes/No)
* __Linearity__: The scatter plot shows that 'age' and 'fracscores' are relatively linearly correlated with the 'fracture' response variable on a logit scale. The rest of the variables do not show linearity therefore these would require some polynomial terms when modeling. 
* __Independence__: We can assume that the observations are independent as we did not observe duplication / repetition in the measurements.
* __Multicolliearity__: Based on the VIF plot there is no multicollinearity among the continuous explanatory variables (VIF values show no multicollinearity)
* __Outliers__: The largest Cooks D value is slightly above 0.02. The residual plot along with the Cooks D value indicates that there is no extreme outlier.
* All necessary assumptions are met for modeling.


#####################################################################################
#                                       Stepwise                                    #
#####################################################################################
```{r, Stepwise Predict}

# Predicting on the final Stepwise model TRAINing data
stepwise.mod.final.pred.train = predict(step.log, newdata = train, type = "response")

# Predicting on the final Stepwise model TEST data
stepwise.mod.final.pred.test = predict(step.log, newdata = test, type = "response")

################ Stepwise ROC Curve ######################################
step.roc=roc(response=test$fracture,predictor=stepwise.mod.final.pred.test,levels=c("No","Yes"))
plot(step.roc,print.thres="best") 

AUC = auc(test$fracture, stepwise.mod.final.pred.test)
print(paste("Area Under the Cuve: ", AUC))
print(paste("Stepwise AIC Score: ", AIC(step.log)))

cutoff.step = 0.159

# Confusion matrix stepwise train
class.stepwise.final.train=factor(ifelse(stepwise.mod.final.pred.train>cutoff.step,"Yes","No"),levels=c("No","Yes"))

#Confusion Matrix for Stepwise train
print("Confusion matrix for Stepwise TRAINING with 0.5 cutoff")
confusionMatrix(table(class.stepwise.final.train,train$fracture), positive = "Yes")

#####################################################################################################

# Confusion matrix
class.stepwise.final.test=factor(ifelse(stepwise.mod.final.pred.test>cutoff.step,"Yes","No"),levels=c("No","Yes"))

#Confusion Matrix for Stepwise
print("Confusion matrix for Stepwise TEST with 0.5 cutoff")
confusionMatrix(table(class.stepwise.final.test,test$fracture), positive = "Yes")


# Create variables to store metrics
step.final.test.accuracy = confusionMatrix(table(class.stepwise.final.test,test$fracture), positive = "Yes")$overall[1]
step.final.test.sensitivity = confusionMatrix(table(class.stepwise.final.test,test$fracture), positive = "Yes")$byClass[1]
step.final.test.specificity = confusionMatrix(table(class.stepwise.final.test,test$fracture), positive = "Yes")$byClass[2]

#misClasificError.stepwise.test = mean(class.stepwise.final.test != test$fracture)
#print(paste('Misclassification Rate for stepwise on test set: ', misClasificError.stepwise.test))


# ODD ratios for interpretation
summary(step.log)
exp(cbind("Odds ratio" = coef(step.log), confint.default(step.log, level = 0.95)))

Model_name = c(Model_name, 'Stepwise')
AUC_score = c(AUC_score, AUC)
Test_Accuracy  = c(Test_Accuracy, step.final.test.accuracy)
Test_Sensitivity  = c(Test_Sensitivity, step.final.test.sensitivity)
Test_Specificity  = c(Test_Specificity, step.final.test.specificity)
AIC  = c(AIC, AIC(step.log))
Threshold = c(Threshold, cutoff.step)
```

## Stepwise Logistic Regression Assumptions
```{r, assumtion logreg stepwise}
# Checking logistic regression model assumptions

# 1. There is a linear relationship between the logit of the outcome and each predictor variables.
# Select only numeric predictors
step.logit.assumption.cont.vars = train[,-16] %>% dplyr::select_if(is.numeric) 
predictors = colnames(step.logit.assumption.cont.vars)


# Bind the logit and tidying the data for plot
step.logit.assumption.cont.vars = step.logit.assumption.cont.vars %>%   mutate(logit = log(stepwise.mod.final.pred.train/(1-stepwise.mod.final.pred.train))) %>% gather(key = "predictors", value = "predictor.value", -logit)

ggplot(step.logit.assumption.cont.vars, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# 2. Multicollinearity (re-fit the model with only continuous variables as VIF does not make sense on categorical data)
step.mod.final.vif=glm(fracture ~  age+weight+bmi, family = binomial(link='logit'), data = train)
vif_score(step.mod.final.vif)

# 3. Outlier check
outlier_check(step.log)
```

## Observations:
* __The Response Variable is Binary__: 'Fracture' as a response variable is a factor with binary levels (Yes/No)
* __Linearity__: The scatter plot shows that 'age' and 'fracscores' are relatively linearly correlated with the 'fracture' response variable on a logit scale. The rest of the variables do not show linearity therefore these would require some polynomial terms when modeling. 
* __Independence__: We can assume that the observations are independent as we did not observe duplication / repetition in the measurements.
* __Multicolliearity__: Based on the VIF plot there is no multicollinearity among the continuous explanatory variables (VIF values show no multicollinearity). However we can obsertve that BMI and Weight are highly correlated but still below our threshold of 10.
* __Outliers__: The largest Cooks D value is slightly below 0.03. The residual plot along with the Cooks D value indicates that there is no extreme outlier.
* All necessary assumptions are met for modeling.


#####################################################################################
#                                     Manual / Intuition                            #
#####################################################################################

```{r, manual selection prediction}
# 5-fold cross validation
cv = trainControl(
  method = "repeatedcv", 
  number = 5,
  repeats = 10,
  savePredictions = TRUE,
  summaryFunction=mnLogLoss,
  classProbs = TRUE
)

MLogReg = train(
  fracture ~  age + bonemed + bonemed_fu + bonetreat,
  data = train,
  method = "glm",
  family = "binomial",
  trControl = cv,
  metric = "logLoss")

### Hypothesis testing
summary(MLogReg$finalModel)
anova(MLogReg$finalModel, test="Chisq")
exp(cbind("Odds ratio" = coef(MLogReg$finalModel), confint.default(MLogReg$finalModel, level = 0.95)))

# Predicting
MLogReg.pred.train = predict(MLogReg, train, type = 'prob')
MLogReg.pred.test = predict(MLogReg, test, type = 'prob')

AUC = auc(test$fracture, MLogReg.pred.test$Yes)
print(paste("Area Under the Cuve: ", AUC))
print(paste("Manual model AIC Score: ", MLogReg$finalModel$aic))


################ Manual ROC Curve ######################################
manual.roc=roc(response=test$fracture,predictor=MLogReg.pred.test$Yes,levels=c("No","Yes"))
plot(manual.roc,print.thres="best")

cutoff.manual = 0.238

#Confusion Matrix for manual model
class.manual.train=ifelse(MLogReg.pred.train$Yes > cutoff.manual,"Yes","No")
class.manual.train=factor(class.manual.train)

class.manual.test=ifelse(MLogReg.pred.test$Yes > cutoff.manual,"Yes","No")
class.manual.test=factor(class.manual.test)

confusionMatrix(table(class.manual.train,train$fracture), positive = "Yes")
confusionMatrix(table(class.manual.test,test$fracture), positive = "Yes")


# Create variables to store metrics
manual.final.test.accuracy = confusionMatrix(table(class.manual.test,test$fracture), positive = "Yes")$overall[1]
manual.final.test.sensitivity = confusionMatrix(table(class.manual.test,test$fracture), positive = "Yes")$byClass[1]
manual.final.test.specificity = confusionMatrix(table(class.manual.test,test$fracture), positive = "Yes")$byClass[2]

Model_name = c(Model_name, 'Manual/Intuition model')
AUC_score = c(AUC_score, AUC)
Test_Accuracy  = c(Test_Accuracy, manual.final.test.accuracy)
Test_Sensitivity  = c(Test_Sensitivity, manual.final.test.sensitivity)
Test_Specificity  = c(Test_Specificity, manual.final.test.specificity)
AIC  = c(AIC, MLogReg$finalModel$aic)
Threshold = c(Threshold, cutoff.manual)
```

## Manual Logistic Regression Assumptions
```{r, assumtion logreg manual}
# Checking logistic regression model assumptions

# 1. There is a linear relationship between the logit of the outcome and each predictor variables.
# Select only numeric predictors
manual.logit.assumption.cont.vars = train[,-16] %>% dplyr::select_if(is.numeric) 
predictors = colnames(manual.logit.assumption.cont.vars)

# Bind the logit and tidying the data for plot
manual.logit.assumption.cont.vars = manual.logit.assumption.cont.vars %>% mutate(logit = log(MLogReg.pred.train$Yes/(1-MLogReg.pred.train$Yes))) %>% gather(key = "predictors", value = "predictor.value", -logit)

ggplot(manual.logit.assumption.cont.vars, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# 3. Outlier check
outlier_check(MLogReg$finalModel)

```

## Observations:
* __The Response Variable is Binary__: 'Fracture' as a response variable is a factor with binary levels (Yes/No)
* __Linearity__: The scatter plot shows slight linearity for 'age' and 'fracscore' but the other variables does not show any linearity therefore these would require some polynomial terms when modeling. 
* __Independence__: We can assume that the observations are independent as we did not observe duplication / repetition in the measurements.
* __Multicolliearity__: For the manual model the only continous variable is 'age' therefore there is no reason to measure multicollinearity.
* __Outliers__: The largest Cooks D value is slightly below 0.03. The residual plot along with the Cooks D value indicates that there is no extreme outlier.
* All necessary assumptions are met for modeling.


## Observations and interpretations of the Manual model coefficients:
* Based on the summary statistics at alpha = 0.05 only age, bonemed_fuYes and bonetreatYes variables are statistically significant.
* Interpret the statistically significant variables:

* __Holding all other variables constant, the odds of a patient having fracture in the first year with a 10 year of increase in age is 10.3594 times higher than the odds for those with lower age.__ 

* __Holding all other variables constant, the odds of a patient having fracture in the first year who took bone medication at the follow-up visits is 3.63421 times higher than the odds for those whose did not take the medication at the time of follow-up visits.__ Again, this is counter intuitive as we would expect that someone who takes bone medication will have a lower or equal changes of fracture then those who don't. This indicates that there is a confounding variable that is impacted the treatment in the study.

* __Holding all other variables constant, the odds of a patient having fracture in the first year who took bone medication at both  enrollment and at the follow-up visits is 0.09057 times higher than the odds for those whose did not take the medication at the time of enrollment and follow-up visits.__ 

* __Confidence intervals__: The confidence intervals for Odds ratios indicates the following:
1. The variable 'bonetreatYes' is the only one which helps mitigating fracture in the first year after enrolling into the study. This can be seen from the confidence interval and the odds ratio. The odds ratio is below 1 therefore bonetreatYes is actually shows that the odds of having a fracture in the first year is lower than for those who don't take the bone treatment medication at enrollment and follow-ups. The confidence interval (both bounds) remains below 1 hence we can conclude that bone treatment medication at enrollment and follow-ups is effective protecting against fracture in the first year.
2. The both of the bounds of the confidence intervals for the rest of the variables are above 1 indicating that theses variables are significant risks of having a fracture in the first year. 


#####################################################################################
#                          Final conclusion from Objective 1                        #
#####################################################################################
* Our objective was to assess risk factors if a woman with osteoporosis will have a bone fracture within the first year after joining the study.
* We have analysed a data set which contains 500 observations and 18 features. The data set has imbalanced class levels. There are more cases with "No" fracture than ones with "Yes".
* We have observed extreme values in the data set but those are not influential points, therefore we kept them in the data set.
* We have built LASSO, Stepwise models and used EDA and intuition to select the most significant variables that helps interpreting and predicting bone fractures in the first year.
* We noticed that there is no obvious separation between the fracture levels which indicates that the predictors are not significant.
* Out of the three models we have built the Manual/Intuition one performed as this model has the highest Area Under the Curve value however the AIC value is slightly higher then the other two (LASSO and Stepwise) models.
* Our study focused on optimizing the model for low error rate, high AUC and high sensitivity scores. For this particular medical study it is important that the model predicts a positive outcome for an observation with high probability when the outcome is indeed positive.  Correctly identifying a true positive case outweighs the false positive rate as it would potentially save lives.   
* The manual model suggests that the variable 'bonetreatYes' is the only one which helps mitigating fracture in the first year after enrolling into the study. This can be seen from the confidence interval and the odds ratio. The odds ratio is below 1 therefore bonetreatYes is actually shows that the odds of having a fracture in the first year is lower than for those who don't take the bone treatment medication at enrollment and follow-ups. The confidence interval (both bounds) remains below 1 hence we can conclude that bone treatment medication at enrollment and follow-ups is effective protecting against fracture in the first year.
* The other statistically significant variables are representing significant risk for having bone fracture in the first year. The most notable of those variables are rateriskGreater, bonemedYes and bonemed_fuYes.
* In this study we believe that the statistically significant and practical significance does not fully overlap. We have observed counter intuitive results as the odds of a patient having fracture in the first year who took bone medication at the follow-up visits is 3.63421 times higher than the odds for those whose did not take the medication at the time of follow-up visits. This is counter intuitive and against the practical interpretations as we would expect that someone who takes bone medication will have a lower or equal changes of fracture then those who don't. This indicates that there is a confounding variable that has impacted the treatment in the study.
* The second best model is the Stepwise selection for Logistic Regression however that model included lots of variables which likely increased the variance and over fitted the model as the sensitivity score is 1 for the test set.


#####################################################################################
#                                       Objective II                                #
#####################################################################################

```{r, interaction}
#####################################################################################
#                                Interaction                                #
#####################################################################################

cv.ii = trainControl(
  method = "repeatedcv", 
  number = 5,
  repeats = 10,
  savePredictions = TRUE,
  summaryFunction=mnLogLoss,
  classProbs = TRUE
)

MLogReg.ii = train(
  fracture ~  age + height + bonemed_fu + bonetreat + bmi*bonemed_fu + bmi*bonemed + I(age^2) + I(height^3),
  data = train,
  method = "glm",
  family = "binomial",
  trControl = cv.ii,
  metric = "logLoss")

### Hypothesis testing
summary(MLogReg.ii$finalModel)

anova(MLogReg.ii$finalModel, test="Chisq")

hoslem.test(MLogReg.ii$finalModel$y,fitted(MLogReg.ii))

# Predicting
MLogReg.ii.pred.train = predict(MLogReg.ii, train, type = 'prob')
MLogReg.ii.pred.test = predict(MLogReg.ii, test, type = 'prob')

AUC = auc(test$fracture, MLogReg.ii.pred.test$Yes)
print(paste("Area Under the Cuve: ", AUC))
print(paste("Manual model with interactions and polynomial terms AIC Score: ", MLogReg.ii$finalModel$aic))


################ Manual ROC Curve ######################################
manual.roc.ii=roc(response=test$fracture,predictor=MLogReg.ii.pred.test$Yes,levels=c("No","Yes"))
plot(manual.roc.ii,print.thres="best")

cutoff.manual.ii = 0.234

#Confusion Matrix for manual model
class.manual.train.ii=ifelse(MLogReg.ii.pred.train$Yes > cutoff.manual.ii,"Yes","No")
class.manual.train.ii=factor(class.manual.train)

class.manual.test.ii=ifelse(MLogReg.ii.pred.test$Yes > cutoff.manual.ii,"Yes","No")
class.manual.test.ii=factor(class.manual.test.ii)

confusionMatrix(table(class.manual.train.ii,train$fracture), positive = "Yes")
confusionMatrix(table(class.manual.test.ii,test$fracture), positive = "Yes")

# Create variables to store metrics
MLogReg.ii.final.test.accuracy = confusionMatrix(table(class.manual.test.ii,test$fracture), positive = "Yes")$overall[1]
MLogReg.ii.final.test.sensitivity = confusionMatrix(table(class.manual.test.ii,test$fracture), positive = "Yes")$byClass[1]
MLogReg.ii.final.test.specificity = confusionMatrix(table(class.manual.test.ii,test$fracture), positive = "Yes")$byClass[2]

Model_name = c(Model_name, 'Complex model')
AUC_score = c(AUC_score, AUC)
Test_Accuracy  = c(Test_Accuracy, MLogReg.ii.final.test.accuracy)
Test_Sensitivity  = c(Test_Sensitivity, MLogReg.ii.final.test.sensitivity)
Test_Specificity  = c(Test_Specificity, MLogReg.ii.final.test.specificity)
AIC  = c(AIC, MLogReg.ii$finalModel$aic)
Threshold = c(Threshold, cutoff.manual.ii)
```

```{r, interaction, warning=FALSE}
#####################################################################################
#                                Interaction                              #
#####################################################################################

#set the cutoff and make the model
cutoff.manual = 0.5
LogReg.Interaction<-glm(fracture~log(age) + height + priorfrac + log(bmi)*bonetreat,
                        family="binomial"(link='logit'), data = train)

#check for co-linearity outside of interaction
vif(LogReg.Interaction, type='terms')

#look at the coefficients
summary(LogReg.Interaction)
plot(LogReg.Interaction)

#predict
LogReg.Interaction.pred.train <- predict(LogReg.Interaction, newdata = train, type = "response")
LogReg.Int.final.pred.test <- predict(LogReg.Interaction, newdata = test, type = "response")

Anova(LogReg.Interaction,type=3)
```


```{r}
exp(cbind("Odds ratio" = coef(LogReg.Interaction), confint.default(LogReg.Interaction, level = 0.95)))
```

```{r}
results.int<-prediction(LogReg.Interaction.pred.train, train$fracture,label.ordering=c("No","Yes"))
roc.int = performance(results.int, measure = "tpr", x.measure = "fpr")
plot(roc.int,colorize = TRUE)
abline(a=0, b= 1)

results.int<-prediction(LogReg.Int.final.pred.test, test$fracture,label.ordering=c("No","Yes"))
roc.int = performance(results.int, measure = "tpr", x.measure = "fpr")
plot(roc.int,colorize = TRUE)
abline(a=0, b= 1)

```

```{r}
# Confusion matrix
class.LogReg.Interaction.test<-factor(ifelse(LogReg.Int.final.pred.test>cutoff.manual,"Yes","No"),levels=c("No","Yes"))

# Confusion matrix
class.LogReg.Interaction.train<-factor(ifelse(LogReg.Interaction.pred.train>cutoff.manual,"Yes","No"),levels=c("No","Yes"))

#Confusion Matrix for interaction
print("Confusion matrix for Interaction TRAINING with 0.5 cutoff")
```

```{r}
confusionMatrix(table(class.LogReg.Interaction.train,train$fracture))
confusionMatrix(table(class.LogReg.Interaction.test,test$fracture))
```


```{r}
################ Misclassification rate train ######################################
cross.table.interaction.train = table(class.LogReg.Interaction.train,train$fracture)
MCR_interaction.train = (cross.table.interaction.train[2]+cross.table.interaction.train[3])/dim(train)[1]
print(paste('Misclassification Rate for manual interaction selection on training set: ', MCR_interaction.train))
```
```{r}
################ Misclassification rate test ######################################
cross.table.interaction.test = table(class.LogReg.Interaction.test,test$fracture)
MCR_interaction.test = (cross.table.interaction.test[2]+cross.table.interaction.test[3])/dim(test)[1]
print(paste('Misclassification Rate for manual interaction selection on test set: ', MCR_interaction.test))
```
```{r}
hoslem.test(train$fracture.num,fitted(LogReg.Interaction))

# Predicting
LogReg.Interaction.pred.train = predict(LogReg.Interaction, train, type = 'response')
LogReg.Interaction.pred.test = predict(LogReg.Interaction, test, type = 'response')

AUC = auc(test$fracture, LogReg.Interaction.pred.test)
print(AUC)
```

## Observations
* Built two models with different interaction terms based on EDA interpretations.
* One model was using a cut off value of 0.234 and the other model 0.5.
* The different cut off values helps tuning the overall accuracy, sensitivity and specificity values in the confusion matrix.
* Based on the Hosmer-Lemeshow test one of the model is not a good fit (p = 0.03). The other model has a p-value = 0.1 which shows that the model is a good fit.
* The ROC curve helped determine the optimal cut-off value and improve the model performance as consequently reducing the misclassification rate.


#####################################################################################
#                                        LDA/QDA                                    #
#####################################################################################
```{r, LDA QDA}
# LDA
lda.train = train[,-16]
lda.test = test[,-16]

lda.model = lda(fracture ~age+weight+height, prior = c(.75, .25), data = lda.train)
plot(lda.model)


lda.model.pred.train = predict(lda.model, lda.train)
lda.model.pred.test = predict(lda.model, lda.test)

# LDA ROC cuve
lda.roc=roc(response=lda.test$fracture,predictor=lda.model.pred.test$posterior[,2],levels=c("No","Yes"))
plot(lda.roc,print.thres="best")

cutoff.lda = 0.343
lda.pred.adj.train = ifelse(lda.model.pred.train$posterior[,2] > cutoff.lda, "Yes", "No")
lda.pred.adj.test = ifelse(lda.model.pred.test$posterior[,2] > cutoff.lda, "Yes", "No")

# LDA Confusion matrix
confusionMatrix(table(lda.pred.adj.train,lda.train$fracture), positive = "Yes")
confusionMatrix(table(lda.pred.adj.test,lda.test$fracture), positive = "Yes")

#Scoring
lda.prob.test = predict(lda.model, lda.test, type = "prob")
AUC = auc(lda.test$fracture, lda.prob.test$posterior[,2])
print(paste("Area Under the Cuve: ", AUC))

# Create variables to store metrics
lda.final.test.accuracy = confusionMatrix(table(lda.pred.adj.test,test$fracture), positive = "Yes")$overall[1]
lda.final.test.sensitivity = confusionMatrix(table(lda.pred.adj.test,test$fracture), positive = "Yes")$byClass[1]
lda.final.test.specificity = confusionMatrix(table(lda.pred.adj.test,test$fracture), positive = "Yes")$byClass[2]

Model_name = c(Model_name, 'LDA')
AUC_score = c(AUC_score, AUC)
Test_Accuracy  = c(Test_Accuracy, lda.final.test.accuracy)
Test_Sensitivity  = c(Test_Sensitivity, lda.final.test.sensitivity)
Test_Specificity  = c(Test_Specificity, lda.final.test.specificity)
AIC  = c(AIC, 'NA')
Threshold = c(Threshold, cutoff.lda)


# QDA
qda.train = train[,-16]
qda.test = test[,-16]

qda.model = qda(fracture ~age+weight+height, prior = c(.75, .25), data = qda.train)
plot(lda.model)

qda.model.pred.train = predict(qda.model, qda.train)
qda.model.pred.test = predict(qda.model, qda.test)

# QDA ROC cuve
qda.roc=roc(response=qda.test$fracture,predictor=qda.model.pred.test$posterior[,2],levels=c("No","Yes"))
plot(qda.roc,print.thres="best")

cutoff.qda = 0.289
qda.pred.adj.train = ifelse(qda.model.pred.train$posterior[,2] > cutoff.qda, "Yes", "No")
qda.pred.adj.test = ifelse(qda.model.pred.test$posterior[,2] > cutoff.qda, "Yes", "No")

# QDA Confusion matrix
confusionMatrix(table(qda.pred.adj.train,qda.train$fracture), positive = "Yes")
confusionMatrix(table(qda.pred.adj.test,qda.test$fracture), positive = "Yes")

#Scoring
qda.prob.test = predict(qda.model, qda.test, type = "prob")
AUC = auc(qda.test$fracture, qda.prob.test$posterior[,2])
print(paste("Area Under the Cuve: ", AUC))


# Create variables to store metrics
qda.final.test.accuracy = confusionMatrix(table(qda.pred.adj.test,test$fracture), positive = "Yes")$overall[1]
qda.final.test.sensitivity = confusionMatrix(table(qda.pred.adj.test,test$fracture), positive = "Yes")$byClass[1]
qda.final.test.specificity = confusionMatrix(table(qda.pred.adj.test,test$fracture), positive = "Yes")$byClass[2]

Model_name = c(Model_name, 'QDA')
AUC_score = c(AUC_score, AUC)
Test_Accuracy  = c(Test_Accuracy, qda.final.test.accuracy)
Test_Sensitivity  = c(Test_Sensitivity, qda.final.test.sensitivity)
Test_Specificity  = c(Test_Specificity, qda.final.test.specificity)
AIC  = c(AIC, 'NA')
Threshold = c(Threshold, cutoff.qda)
```

## Observations
* Built LDA/QDA models as competing models for the complex model.
* LDA outperformed the QDA model on a set of continuous variables. The reason for that is the LDA model is more linear in nature and the variables used for the models are more linear based on the previous EDA observations. 
* The different cut off values helps tuning the overall accuracy, sensitivity and specificity values in the confusion matrix for LDA and QDA models too.
* The ROC curve helped determine the optimal cut-off value and improve the model performance but still the LDA or QDA models did not outperform the logistic regression models. It is due to the fact that we only fitted the continous variables but as stated earlier the statistically significant variables in this data set are categorical.


#####################################################################################
#                                   Decision Tree                                   #
#####################################################################################
```{r, decision tree}

dt.train = train[,-16]
dt.test = test[,-16]

cv.dtree = trainControl(
  method = "cv", 
  number = 5,
  savePredictions = TRUE,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

dtree.model = train(
  fracture ~ ., 
  data = dt.train, 
  method = "rpart",
  metric = "ROC",
  tuneGrid = expand.grid(cp=0.0048),
  trControl=cv.dtree,
  tuneLength = 100
  )


# print the tree
fancyRpartPlot(dtree.model$finalModel)
bestcp = dtree.model$finalModel$cptable[which.min(dtree.model$finalModel$cptable[,"rel error"]),"CP"]
print(paste("Best complexity score: ", bestcp))

# Predicting
dtree.model.pred.train = predict(dtree.model, dt.train, type = "prob")
dtree.model.pred.test = predict(dtree.model, dt.test, type = "prob")

# ROC Curve 
dtree.roc=roc(response=qda.test$fracture,predictor=dtree.model.pred.test$Yes,levels=c("No","Yes"))
plot(dtree.roc,print.thres="best")

cutoff.dtree = 0.138
dtree.pred.adj.train = ifelse(dtree.model.pred.train$Yes > cutoff.dtree, "Yes", "No")
dtree.pred.adj.test = ifelse(dtree.model.pred.test$Yes > cutoff.dtree, "Yes", "No")


# Confusion matrix
confusionMatrix(table(dtree.pred.adj.train,dt.train$fracture), positive = "Yes")
confusionMatrix(table(dtree.pred.adj.test,dt.test$fracture), positive = "Yes")

#Scoring
dt.prob.test = predict(dtree.model, dt.test, type = "prob")
AUC = auc(dt.test$fracture, dt.prob.test$Yes)
print(paste("Area Under the Cuve: ", AUC))


# Create variables to store metrics
dtree.final.test.accuracy = confusionMatrix(table(dtree.pred.adj.test,test$fracture), positive = "Yes")$overall[1]
dtree.final.test.sensitivity = confusionMatrix(table(dtree.pred.adj.test,test$fracture), positive = "Yes")$byClass[1]
dtree.final.test.specificity = confusionMatrix(table(dtree.pred.adj.test,test$fracture), positive = "Yes")$byClass[2]

Model_name = c(Model_name, 'Decision Tree')
AUC_score = c(AUC_score, AUC)
Test_Accuracy  = c(Test_Accuracy, dtree.final.test.accuracy)
Test_Sensitivity  = c(Test_Sensitivity, dtree.final.test.sensitivity)
Test_Specificity  = c(Test_Specificity, dtree.final.test.specificity)
AIC  = c(AIC, 'NA')
Threshold = c(Threshold, cutoff.dtree)

```

## Observations
* Built a decision tree model where we tuned the complexity parameter of the tree.
* We have fitted all the variables to the Decision tree model. The model only created a tree if we had all the variables included otherwise it was not able to build a tree just a root node.
* Performance of this model is the second worst of all but can be easily interpreted and we were able to tune the cut off value for the confusion matrix as well to improve the model performance. This model has a low optimal cut off rate which also indicates that it will produce a lot of false positive observations.
* This model identified that 
* __if the woman had prior fracture and her fracture score is greater than 7.5 then this woman has a significant risk of having a fracture in the first year__
* __if the woman had prior fracture and her fracture score is less than 7.5 and this woman is smaller or equal to 165 cm, has a BMI lower than 27 then this woman has a significant risk of having a fracture in the first year__
* __if the woman did not have prior fracture but her rate risk is higher than 50% then this woman has a significant risk of having a fracture in the first year__
* __if the woman did not have prior fracture but her rate risk is lower than 50% and taller than 156 cm then this woman has a significant risk of having a fracture in the first year__


#####################################################################################
#                                 Random Forest                              #
#####################################################################################
``` {r, RF}
## Libraries
library(mlbench)

## rf model
library(randomForest)

set.seed(329)

index<-sample(1:dim(bonemed_df)[1],round(dim(bonemed_df)[1]*0.70),replace=F)
train.rf = bonemed_df[index,]
test.rf = bonemed_df[-index,]


rf <- randomForest(fracture~priorfrac+age+height+bmi+momfrac+armassist+raterisk+fracscore+bonemed+bonemed_fu+bonetreat ,data=train.rf,maxnodes=10,ntree=50)
print(rf)
attributes(rf)
```

#test rf
```{r rf test}
rfTest <- predict(rf, test.rf)
length(test.rf$fracture)
summary(rfTest)

pvodf <- data.frame(Predicted = rfTest, Observed = test.rf$fracture)
#print(pvodf)

#auc
rf_p_test <- predict(rf,test.rf, type="prob")[,2]
rf_pr_test <- prediction(rf_p_test, test.rf$fracture)
r_auc_test1 = auc(test.rf$fracture, rf_p_test)
r_auc_test1

# ROC Curve 
rf.roc=roc(response=test.rf$fracture,predictor=rf_p_test,levels=c("No","Yes"))
plot(rf.roc,print.thres="best")


cutoff.rf = 0.107
rf.pred.adj.test = ifelse(rf_p_test > cutoff.rf, "Yes", "No")


#confusion martrix
confusionMatrix(table(rf.pred.adj.test, test.rf$fracture), positive = "Yes")

# Create variables to store metrics
rf.test.accuracy = confusionMatrix(pvodf$Predicted,pvodf$Observed,positive = 'Yes')$overall[1]
rf.test.sensitivity = confusionMatrix(pvodf$Predicted,pvodf$Observed,positive = 'Yes')$byClass[1]
rf.test.test.specificity = confusionMatrix(pvodf$Predicted,pvodf$Observed,positive = 'Yes')$byClass[2]

Model_name = c(Model_name, 'Random Forest')
AUC_score = c(AUC_score, r_auc_test1)
Test_Accuracy  = c(Test_Accuracy, rf.test.accuracy)
Test_Sensitivity  = c(Test_Sensitivity, rf.test.sensitivity)
Test_Specificity  = c(Test_Specificity, rf.test.test.specificity)
AIC  = c(AIC, 'NA')
Threshold = c(Threshold, cutoff.rf)

```

#####################################################################################
#     Overall report of the error metrics and model performance on a test set       #
#####################################################################################
```{r, model summary, warning=FALSE}
# model summary
model.sum.df = data.frame()
model.sum.df = data.frame(Model_name, AUC_score, AIC, Test_Accuracy, Test_Sensitivity, Test_Specificity, Threshold)
model.sum.df$AIC = format(round(as.numeric(model.sum.df$AIC), 4), nsmall = 4)
ordered.model.sum.df = model.sum.df[order(-AUC_score),]
rownames(ordered.model.sum.df) = 1:nrow(ordered.model.sum.df)
knitr::kable(ordered.model.sum.df)
```

#####################################################################################
#                              Conclusion / Discussion                              #
#####################################################################################

* We have built four models to compete against the models built in Objective 1. 
* We have tuned the cut-off value for the models to improve the overall performance of the models.
* We used AUC as a measure to select the best model however we also considered AIC wherever it was applicable. A more parsimonious model  would have a better AIC value and help eliminating or mitigating bias-variance trade-off problem.
* We used the ROC curves to find the optimal threshold for the confusion matrices and we observed that each model performed best at different threshold values. The common pattern noticed was that each threshold value is below the default 0.5 value. This is likely due to the imbalanced nature of the data.
* Overall our recommendation is to use the Complex model for prediction as it performs the best however this may not be the most suitable for interpretation. 
* For interpretation purposes we would recommend either the Decision Tree model or any of the simple Logistic Regression models from Objective 1.
* The reason why the complex model win in terms of best AUC is because adding more complexity to a model will increase the prediction accuracy and during the EDA we observed the different interactions between the variables which helps explaining the variance in the data more than the additive model.
* We would also recommend resampling this data set to improve the class balance and potentially remove some confounding variables which creates counter intuitive results for interpretation. 
* This is an observational study without random selection or random assignments to groups therefore no casual inferencing can be made. Inferencing from this study conclusion can be applied to similar scenarios and the woman included into this research. 

