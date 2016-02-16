---
title: 'Tooth Growth: Ascorbic Acid vs Orange Juice'
author: "Alyssa Goldberg"
date: "November 20, 2015"
output: html_document
---
The Effect of Vitamin C on Tooth Growth in Guinea Pigs


I. Synopsis

This document is the final report of the Peer Assessment project - Part 2 from Coursera's course Statistical Inference, as part of the Specialization in Data Science. It was built up in RStudio, using its knitr functions, meant to be published in pdf format.
Here, we perform some exploratory data analysis and use statistical inference methods in the ToothGrowth dataset already present in R.


II. Overview

The response variable in this analysis is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs.

Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods: orange juice (OJ) or ascorbic acid (a form of vitamin C) and coded as VC.
The complete dataset is a data frame with 60 observations on 4 variables:
[,1] gpig integer Guinea Pig
[,2] len numeric Tooth length
[,3] supp factor Supplement type (VC or OJ)
[,4] dose numeric Dose (in milligrams/day)




```{r}
# rm(list=ls())      
library(knitr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(datasets)
```

You can also embed plots, for example:

```{r}
data(ToothGrowth)
str(ToothGrowth)

require(graphics)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")
```

```{r}
gr01 <- ggplot(ToothGrowth, aes(x = supp, y = len, fill = supp)) +
               geom_boxplot() +
               ggtitle(expression(atop("Tooth Growth",
                                  atop(italic("by Supplement Type"),"")))) +
               xlab("Supplement Type") + ylab("Tooth Lenght")

gr02 <- ggplot(ToothGrowth, aes(x = factor(dose), y = len, fill = factor(dose))) +
               geom_boxplot() +
               ggtitle(expression(atop("Tooth Growth",
                                  atop(italic("by Vitamin C Dose"),"")))) +
               xlab("Vitamin C Dose (mg/day)") + ylab("Tooth Lenght")

grid.arrange(gr01, gr02, ncol = 2)        
```

The graphs above show possible differences in the average of tooth growth lenght and drive us to further analyse the impact of Vitamin C on both Supplement Type and Vitamin C Dose.

IV. Comparing Tooth Growth by Supplement Type and Dose
a) Checking for Normality

```{r}
subSuppOJ  <- subset(ToothGrowth, supp == "OJ")
subSuppVC  <- subset(ToothGrowth, supp == "VC")
subDose005 <- subset(ToothGrowth, dose == 0.5)
subDose010 <- subset(ToothGrowth, dose == 1.0)
subDose020 <- subset(ToothGrowth, dose == 2.0)

par(mfrow=c(1, 2))
qqnorm(subSuppOJ$len, main = "supp = OJ")
qqline(subSuppOJ$len, col = "red", lwd = 1)
qqnorm(subSuppVC$len, main = "supp = VC")
qqline(subSuppVC$len, col = "red", lwd = 1)
```

One-sample Kolmogorov-Smirnov test for each of the dose levels
```{r}

ks.test(subSuppOJ$len, "pnorm", mean = mean(subSuppOJ$len), sd = sd(subSuppOJ$len)/sqrt(length(subSuppOJ$len)))

ks.test(subSuppVC$len, "pnorm", mean = mean(subSuppVC$len), sd = sd(subSuppVC$len))

ks.test(subDose020$len, "pnorm", mean = mean(subDose020$len), sd = sd(subDose020$len))

ks.test(subDose020$len, "pnorm", mean = mean(subDose020$len), sd = sd(subDose010$len))

ks.test(subDose020$len, "pnorm", mean = mean(subDose020$len), sd = sd(subDose005$len))
```

Confidence Intervals:
```{r}
dataDose <- as.data.frame(rbind(
                 c(mean(subDose005$len), t.test(subDose005$len)$conf),
                 c(mean(subDose010$len), t.test(subDose010$len)$conf),
                 c(mean(subDose020$len), t.test(subDose020$len)$conf)))

dataDose <- cbind(as.factor(c(0.5,1.0,2.0)),dataDose)

names(dataDose) <- c("DoseCode","meanX","CImin","CImax")

dataSupp <- as.data.frame(rbind(
                 c(mean(subSuppOJ$len), t.test(subSuppOJ$len)$conf),
                 c(mean(subSuppVC$len), t.test(subSuppVC$len)$conf)))
dataSupp <- cbind(as.factor(c("OJ","VC")),dataSupp)
names(dataSupp) <- c("DoseCode","meanX","CImin","CImax")

gr03 <- ggplot(dataDose, aes(x=DoseCode, y=meanX,
                             colour=DoseCode,group=DoseCode)) + 
        geom_errorbar(aes(ymin=CImin, ymax=CImax),colour="black", width=.1) +
        geom_point(size=4) +
        ggtitle("CI for dose") +
        xlab("Vitamin C Dose (mg/day)") + ylab("Tooth Lenght")

gr04 <- ggplot(dataSupp, aes(x=DoseCode, y=meanX, 
                            colour=DoseCode,group=DoseCode)) + 
        geom_errorbar(aes(ymin=CImin, ymax=CImax),colour="black", width=.1) +
        geom_point(size=4) +
        ggtitle("CI for supp") +
        xlab("Supplement Type") + ylab("Tooth Lenght")

grid.arrange(gr03,gr04,ncol=2)
```

The confidence intervals for Vitamin C Dose are visually distant from each other, which indicates higher probability of differences in the averages.
We cannot say the same about the Supplement Type, where CI's are overlapping.

c) t Tests for Difference on Supplement Types averages

Checking for differences in variance:

```{r}
chisq.test(subSuppOJ$len, subSuppVC$len)
```

The p-value is high, so we assume equal variances for the t test:
```{r}
t.test(subSuppOJ$len, subSuppVC$len, paired = FALSE, var.equal = TRUE)
```
The p-value of the test was too close to the significance level   we assumed for the test.
In that case, I would not reject     and assume there is no effect of the Supplement Type (OJ or VC) on the tooth growth lengt of those samples.


d) t Tests for Difference on Vitamin C Dose averages

Checking for differences in variance (run in pairs of sets per level):
```{r}
chisq.test(subDose005$len, subDose010$len)
chisq.test(subDose005$len, subDose020$len)
chisq.test(subDose010$len, subDose020$len)
```

Once again, all p-values are high and, in that case, there is no evidence of differences in the variances. We will assume equal variances for the t tests.
The t tests will be run in pairs of sets per level:
```{r}
t.test(subDose005$len, subDose010$len, paired = FALSE, var.equal = TRUE)

t.test(subDose005$len, subDose020$len, paired = FALSE, var.equal = TRUE)

t.test(subDose010$len, subDose020$len, paired = FALSE, var.equal = TRUE)
```

Here, we can observe very low p-values for the t tests, and we can assume significant differences in the averages of the samples, meaning that we have high level of confidence to assume that the averages increase when the dose of Vitamin C also increase.


V. Conclusions

As a general conclusions:

We were able to observe that the factor that impacts the most on the lenght of tooth growth is the level of Vitamin C Dose. As the dose increase, the larger the tooth growth. This can be validated by the very low p-values detected in the t tests above.
    In the other hand, there is no evidence of the same impact generated by the Supplement Type (p-value = 0.06, too close to  ).
    Concerning to the analysis, there were limitations on number of pages and tests to be used, which can be better developed in a future study.
