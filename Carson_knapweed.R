#
#
#
# Analysis of plant and bee community data for Brendan Carson
# Study performed in 2012-2013, examingin bee communities in knapweed dominated and restored floral communities

#bring data in
Observations<-read.csv(file="https://raw.githubusercontent.com/cbahlai/Carson2015/master/Carson_knapweed_2015.csv", header=TRUE)

#check for errors
summary(Observations)


library(vegan)

#divide data fame into working subsets with plant community, bee community and bee morphotype data

env<-Observations[c(1:9)]
plant.com<-Observations[c(10:34)]
bee.com<-Observations[c(38:91)]
bee.morpho<-Observations[c(35:37)]

#plant metrics
#compute flowering plant density accounting for changes in sampling protocol between seasons
env$bloom.density<-rowSums(plant.com)/env$SamplingArea
#compute bloom richness
env$bloom.richness<-specnumber(plant.com)
#compute bloom diversity
env$bloom.diversity<-diversity(plant.com)

#compute total bee abundance
#note that total abundance is computed using the bee morphotype data, not the community data
#because community data represents the subset of bees that were captured during bee morphotype observations
env$bee.abundance<-rowSums(bee.morpho)
#compute bee species richness from the community data
env$bee.richness<-specnumber(bee.com)
#compute bee diversity from the community data
env$bee.diversity<-diversity(bee.com)


#create a vector for the interaction of day by vegtype that we're trying to get at
env$type.day.interaction<-interaction(env[c(4, 3)], drop=TRUE)


#models

#models use a linear structure for continuous variables (ie diversity) and poisson/ negative binomila error structure for integers.
#models used vegtype, day of year and also year and field pair to account for variation due to these factors
#poisson models were used initially for abundance data, but if the residual deviance was >> residual degrees of freedom, we switched to a negative binomial model
#Analysis of deviance was then used to determine if there was evidence of variation by vegtype, day of year, or an interaction between the two
#if there was a significant interaction, the analysis of deviance was follwoed by a post-hoc pairwise t-test that was holm adjusted for multiple comparisons
#summary statistics were compiled using dplyr

#bloom density model 
library(pscl)

bloom.density.model<-glm(bloom.density~as.factor(Pair)+vegtype*DOY+Year, na.action="na.fail", data=env)
summary(bloom.density.model)

anova(bloom.density.model, test="Chisq")
summary(anova(bloom.density.model, test="Chisq"))


#get means and SEs out
library(plyr)
#year
ddply(env, c("Year"), summarise,
      mean = mean(bloom.density), sd = sd(bloom.density),
      sem = sd(bloom.density)/sqrt(length(bloom.density)))
#vegtype
ddply(env, c("vegtype"), summarise,
      mean = mean(bloom.density), sd = sd(bloom.density),
      sem = sd(bloom.density)/sqrt(length(bloom.density)))
#sample date
ddply(env, c("date"), summarise,
      mean = mean(bloom.density), sd = sd(bloom.density),
      sem = sd(bloom.density)/sqrt(length(bloom.density)))

#bloom richness model

bloom.richness.model<-glm(bloom.richness~as.factor(Pair)+vegtype*DOY+Year, na.action="na.fail", data=env, family="poisson")
summary(bloom.richness.model)

anova(bloom.richness.model, test="Chisq")
summary(anova(bloom.richness.model, test="Chisq"))


#get means and SEs out
#year
ddply(env, c("Year"), summarise,
      mean = mean(bloom.richness), sd = sd(bloom.richness),
      sem = sd(bloom.richness)/sqrt(length(bloom.richness)))
#vegtype
ddply(env, c("vegtype"), summarise,
      mean = mean(bloom.richness), sd = sd(bloom.richness),
      sem = sd(bloom.richness)/sqrt(length(bloom.richness)))
#sample date
ddply(env, c("date"), summarise,
      mean = mean(bloom.richness), sd = sd(bloom.richness),
      sem = sd(bloom.richness)/sqrt(length(bloom.richness)))

#bloom diversity model

bloom.diversity.model<-glm(bloom.diversity~as.factor(Pair)+vegtype*DOY+Year, na.action="na.fail", data=env)
summary(bloom.diversity.model)

anova(bloom.diversity.model, test="Chisq")
summary(anova(bloom.diversity.model, test="Chisq"))

#significant interaction, do pairwise comparisons
pairwise.t.test(env$bloom.diversity, env$type.day.interaction, p.adj = "holm")


#get means and SEs out
#year
ddply(env, c("Year"), summarise,
      mean = mean(bloom.diversity), sd = sd(bloom.diversity),
      sem = sd(bloom.diversity)/sqrt(length(bloom.diversity)))
#vegtype
ddply(env, c("vegtype"), summarise,
      mean = mean(bloom.diversity), sd = sd(bloom.diversity),
      sem = sd(bloom.diversity)/sqrt(length(bloom.diversity)))
#sample date
ddply(env, c("date"), summarise,
      mean = mean(bloom.diversity), sd = sd(bloom.diversity),
      sem = sd(bloom.diversity)/sqrt(length(bloom.diversity)))
#sample date by vegtype
ddply(env, c("date", "vegtype"), summarise,
      mean = mean(bloom.diversity), sd = sd(bloom.diversity),
      sem = sd(bloom.diversity)/sqrt(length(bloom.diversity)))

#bee models
#bee abundance
bee.abundance.model<-glm.nb(bee.abundance~as.factor(Pair)+vegtype*DOY+Year, na.action="na.fail", data=env)
summary(bee.abundance.model)

anova(bee.abundance.model, test="Chisq")
summary(anova(bee.abundance.model, test="Chisq"))

#significant interaction, do pairwise comparisons
pairwise.t.test(env$bee.abundance, env$type.day.interaction, p.adj = "holm")


#get means and SEs out
#year
ddply(env, c("Year"), summarise,
      mean = mean(bee.abundance), sd = sd(bee.abundance),
      sem = sd(bee.abundance)/sqrt(length(bee.abundance)))
#vegtype
ddply(env, c("vegtype"), summarise,
      mean = mean(bee.abundance), sd = sd(bee.abundance),
      sem = sd(bee.abundance)/sqrt(length(bee.abundance)))
#sample date
ddply(env, c("date"), summarise,
      mean = mean(bee.abundance), sd = sd(bee.abundance),
      sem = sd(bee.abundance)/sqrt(length(bee.abundance)))
#sample date by vegtype
ddply(env, c("date", "vegtype"), summarise,
      mean = mean(bee.abundance), sd = sd(bee.abundance),
      sem = sd(bee.abundance)/sqrt(length(bee.abundance)))

#bee richness
bee.richness.model<-glm(bee.richness~as.factor(Pair)+vegtype*DOY+Year, na.action="na.fail", data=env, family="poisson")
summary(bee.richness.model)

anova(bee.richness.model, test="Chisq")
summary(anova(bee.richness.model, test="Chisq"))


#get means and SEs out
#year
ddply(env, c("Year"), summarise,
      mean = mean(bee.richness), sd = sd(bee.richness),
      sem = sd(bee.richness)/sqrt(length(bee.richness)))
#vegtype
ddply(env, c("vegtype"), summarise,
      mean = mean(bee.richness), sd = sd(bee.richness),
      sem = sd(bee.richness)/sqrt(length(bee.richness)))
#sample date
ddply(env, c("date"), summarise,
      mean = mean(bee.richness), sd = sd(bee.richness),
      sem = sd(bee.richness)/sqrt(length(bee.richness)))

#bee diversity

bee.diversity.model<-glm(bee.diversity~as.factor(Pair)+vegtype*DOY+Year, na.action="na.fail", data=env)
summary(bee.diversity.model)

anova(bee.diversity.model, test="Chisq")
summary(anova(bee.diversity.model, test="Chisq"))


#get means and SEs out
#year
ddply(env, c("Year"), summarise,
      mean = mean(bee.diversity), sd = sd(bee.diversity),
      sem = sd(bee.diversity)/sqrt(length(bee.diversity)))
#vegtype
ddply(env, c("vegtype"), summarise,
      mean = mean(bee.diversity), sd = sd(bee.diversity),
      sem = sd(bee.diversity)/sqrt(length(bee.diversity)))
#sample date
ddply(env, c("date"), summarise,
      mean = mean(bee.diversity), sd = sd(bee.diversity),
      sem = sd(bee.diversity)/sqrt(length(bee.diversity)))

#merge morphotype data into env data frame
env<-cbind(env, bee.morpho)

#honey bees
Honeybees.model<-glm.nb(Honeybees~as.factor(Pair)+vegtype*DOY+Year, na.action="na.fail", data=env)
summary(Honeybees.model)

anova(Honeybees.model, test="Chisq")
summary(anova(Honeybees.model, test="Chisq"))

#significant interaction, do pairwise comparisons
pairwise.t.test(env$Honeybees, env$type.day.interaction, p.adj = "holm")


#get means and SEs out
#year
ddply(env, c("Year"), summarise,
      mean = mean(Honeybees), sd = sd(Honeybees),
      sem = sd(Honeybees)/sqrt(length(Honeybees)))
#vegtype
ddply(env, c("vegtype"), summarise,
      mean = mean(Honeybees), sd = sd(Honeybees),
      sem = sd(Honeybees)/sqrt(length(Honeybees)))
#sample date
ddply(env, c("date"), summarise,
      mean = mean(Honeybees), sd = sd(Honeybees),
      sem = sd(Honeybees)/sqrt(length(Honeybees)))

#sample date by vegtype
ddply(env, c("date", "vegtype"), summarise,
      mean = mean(Honeybees), sd = sd(Honeybees),
      sem = sd(Honeybees)/sqrt(length(Honeybees)))
  
#solitary bees
Solitarybees.model<-glm.nb(Solitarybees~as.factor(Pair)+vegtype*DOY+Year, na.action="na.fail", data=env)
summary(Solitarybees.model)

anova(Solitarybees.model, test="Chisq")
summary(anova(Solitarybees.model, test="Chisq"))

#significant interaction, do pairwise comparisons
pairwise.t.test(env$Solitarybees, env$type.day.interaction, p.adj = "holm")


#get means and SEs out
#year
ddply(env, c("Year"), summarise,
      mean = mean(Solitarybees), sd = sd(Solitarybees),
      sem = sd(Solitarybees)/sqrt(length(Solitarybees)))
#vegtype
ddply(env, c("vegtype"), summarise,
      mean = mean(Solitarybees), sd = sd(Solitarybees),
      sem = sd(Solitarybees)/sqrt(length(Solitarybees)))
#sample date
ddply(env, c("date"), summarise,
      mean = mean(Solitarybees), sd = sd(Solitarybees),
      sem = sd(Solitarybees)/sqrt(length(Solitarybees)))

#sample date by vegtype
ddply(env, c("date", "vegtype"), summarise,
      mean = mean(Solitarybees), sd = sd(Solitarybees),
      sem = sd(Solitarybees)/sqrt(length(Solitarybees)))
      

#bumble bees
Bumblebees.model<-glm.nb(Bumblebees~as.factor(Pair)+vegtype*DOY+Year, na.action="na.fail", data=env)
summary(Bumblebees.model)

anova(Bumblebees.model, test="Chisq")
summary(anova(Bumblebees.model, test="Chisq"))

#get means and SEs out
#year
ddply(env, c("Year"), summarise,
      mean = mean(Bumblebees), sd = sd(Bumblebees),
      sem = sd(Bumblebees)/sqrt(length(Bumblebees)))
#vegtype
ddply(env, c("vegtype"), summarise,
      mean = mean(Bumblebees), sd = sd(Bumblebees),
      sem = sd(Bumblebees)/sqrt(length(Bumblebees)))
#sample date
ddply(env, c("date"), summarise,
      mean = mean(Bumblebees), sd = sd(Bumblebees),
      sem = sd(Bumblebees)/sqrt(length(Bumblebees)))






      