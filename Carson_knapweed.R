Observations<-read.csv(file="C:/Rdata/Carson_knapweed_2015.csv", header=TRUE)
summary(Observations)
library(vegan)


env<-Observations[c(1:9)]
plant.com<-Observations[c(10:34)]
bee.com<-Observations[c(38:91)]
bee.morpho<-Observations[c(35:37)]


env$bloom.density<-rowSums(plant.com)/env$SamplingArea
env$bloom.richness<-specnumber(plant.com)
env$bloom.diversity<-diversity(plant.com)

env$bee.abundance<-rowSums(bee.morpho)
env$bee.richness<-specnumber(bee.com)
env$bee.diversity<-diversity(bee.com)
#create a vector for the interaction we're trying to get at
env$type.day.interaction<-interaction(env[c(4, 3)], drop=TRUE)

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
bee.abundance.model<-glm(bee.abundance~as.factor(Pair)+vegtype*DOY+Year, na.action="na.fail", data=env, family="poisson")
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
      mean = mean(bee.abundance, sd = sd(bee.abundance),
      sem = sd(bee.abundance)/sqrt(length(bee.abundance)))
#sample date
ddply(env, c("date"), summarise,
      mean = mean(bee.abundance), sd = sd(bee.abundance),
      sem = sd(bee.abundance)/sqrt(length(bee.abundance)))
#sample date by vegtype
ddply(env, c("date", "vegtype"), summarise,
      mean = mean(bee.abundance), sd = sd(bee.abundance),
      sem = sd(bee.abundance)/sqrt(length(bee.abundance))

#bee richness
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
