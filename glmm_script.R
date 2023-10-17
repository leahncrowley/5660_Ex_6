# Mixed Models 
# Leah N. Crowley - Fall 2023 

# Call requried packages to workspace: 
library(ggplot2)
library(dplyr)
library(arm)
library(MASS)
library(ggfortify)
library(nlme)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggtext)
library(patchwork)

# Read in data you are using for this assignment: 
  birds <- read.csv("birds.csv")

# Make initial plot of data:
  ggplot(birds, aes(State, Mass, colour = as.factor(Rep), shape=Sex)) + 
    geom_jitter(width =0.15, size=5)

# First model of data:
  birdy.states <- lmer(Mass ~ (1|State/Rep), data = birds)
  summary(birdy.states)
  anova(birdy.states)
  
# ANOVA for linear model of data: 
  anova(lm(Mass ~ State/Rep, data = birds))

# Interested in differences among the different states; use state as a fixed effect: 
  birdy.states.2 <- lmer(Mass ~ State + (1|State/Rep), data = birds)
  summary(birdy.states.2)
  anova(birdy.states.2)
 
# Test to see which model fits the bird data the best:
  anova(birdy.states.2,birdy.states, test = "Chisq")
  # They are incredibly close (AIC of 1997.5 vs 1999.6). 
  
# Fit a generalized linear mixed model with point ID as a random effect (random intercept)
  glmm <- glmer(Presence ~ Elevation + (1|PointID), data = birds, family = binomial(link = "logit"),
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  summary(glmm)

# Fit a generalized linear mixed model with point ID as a random effect (random intercept) that varies by state (random slope)
  glmm2 <- glmer(Presence ~ Elevation + (1 + State|PointID), data = birds, family = binomial(link = "logit"))
  summary(glmm2)

# Fit a generalized linear mixed model with point ID as a random effect only
  glmm3 <- glmer(Presence ~ (1|PointID), data = birds, family = binomial(link = "logit"))
  summary(glmm3)

# Compare models:
  AIC(glmm, glmm2, glmm3)
  AIC(glmm, glmm2, glmm3)[2]-min(AIC(glmm, glmm2, glmm3)[2])
