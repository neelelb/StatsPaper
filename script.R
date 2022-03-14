#############################################################
# TERM PAPER - CODE FOR DATA ANALYSIS
# name...................Neele Elbersgerd
# matriculation..........5564097
# course.................Probabilistic and Statistical Modelling
# program................Master of Cognitive Neuroscience, FU Berlin
# instructor.............Dr. Benjamin Eppinger
# semester...............Winter term 2021/2022
#############################################################
# associated Rproject: 'StatsPaper' 
# file '.Rprofile' automatically loads necessary packages 

rm(list = ls())

#--------- DATA QUICK IMPORT ---------#
#--- read in data file created in script 'import'
data = read.table("data/data_all.txt", header = TRUE, sep = ",", dec = ".")
head(data)
    # ID: participant ID,
    # A: amount of reward for first option; DA: delay until reward A is received (7, 14 days)
    # B: amount of reward for second option; DB: additional delay in days (on top of DA) of receiving reward B (14, 28, 42 days)
    # R: chose later (delayed) option (= 1), binary
    # RT: response time in ms
    # diff: % difference in reward
    # agegroup: younger adults vs. older adults
    # conflict: low vs. high conflict (based on % difference)
    # condition: immediate vs. delayed option A
    # delay: two vs. six weeks
    # choice: did the participants choose the early or late option
    # em: episodic memory score; wm: working memory score; gf: fluid intelligence score; speed: speed of processing score
    ##

# convert some variables to factors
cols = c("ID", "agegroup", "conflict", "condition", "delay", "choice")
data[cols] = lapply(data[cols], factor) # convert to factor

# subset of younger or older adults
data_y = data %>% subset(agegroup == "younger adults")
data_o = data %>% subset(agegroup == "older adults")


#--------- EDA ---------#

#--- summaries
summary(data)
# summary of age
data_y %>% summary(); data_y %>% summarise(SD = sd(age))
data_o %>% summary(); data_o %>% summarise(SD = sd(age))

# counting number of trials for each participant
trials = data %>% group_by(ID) %>% count() 
trials$n %>% summary()
plot(trials$n)

# count trials for each level of experimental variables
data$diff %>% table()
data$condition %>% table()
data$delay %>% table()

# summary of working memory variable
data$wm %>% summary()

# describe response variables
data[c("RT", "R")] %>% summary()

# sd of monetary reward
data %>% summarise(SD = sd(A))
data %>% summarise(SD = sd(B))


#--- plotting 
# raw RT
plot(data$RT)
hist(data$RT, breaks = 40, col="steelblue")
boxplot(data$RT, col="steelblue")

ggplot(data, aes(x = RT)) +
  geom_histogram(aes(color = condition, fill = condition),
                 position = "identity", bins = 40, alpha = 0.4) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) + 
  theme_bw()

# boxplot agegroup & RT
plot(data$agegroup, data$RT, col = "steelblue")
ggplot(data, aes(x = condition, y = RT, color = agegroup)) +
  geom_boxplot() +
  theme_bw()


#--- outlier checking
# cutoff via 300ms lower threshold >> 135 trials
subset(data, RT < 300)["RT"] %>% nrow()
data = data %>% subset(RT >= 300)

# cutoffs via standard deviation ?
cut_low = mean(data$RT) - 3*sd(data$RT)
cut_high = mean(data$RT) + 3*sd(data$RT)
# how many values are above or below 3SD of mean? >> 264 below, 0 above
data %>% subset(RT < cut_low | RT > cut_high) %>% nrow()
### REMOVE? 


#--------- ANALYSIS AND MODEL BUILD ---------#
#--- null model, random intercept
m0 = lmer(RT ~ 1 + (1|ID), data = data) 
summary(m0)

#--- contrasts for SIMPLE EFFECTS
# compares each level of a variable to the specified reference level
contrasts(factor(data$condition))
contrasts(factor(data$agegroup))
#relevel factors if necessary
data$condition = relevel(factor(data$condition), ref = "delayed")
data$agegroup = relevel(factor(data$agegroup), ref = "younger adults")

m1.sim = lmer(RT ~ condition + (1|ID), data = data)
summary(m1.sim)
# delayed as the reference level: we can see that there is no significant difference in reaction times
# between immediate and delayed conditions
# Participants seemed not to react faster to choice pairs with immediate options, 
# compared to choice pairs with only delayed option

# does it make sense to include a random slope, if there is no simple 
# effect for condition before?

# is there an interaction between age group and condition?
m2.sim = lmer(RT ~ condition*agegroup + (1|ID), data = data)
summary(m2.sim)
# older participants do not differ significantly from younger adults in their reaction time
# there is no interaction between agegroup and the condition


m3.sim = lmer(RT ~ condition*agegroup + (1+condition|ID), data = data)
summary(m3.sim)

anova(m0, m1.sim, m2.sim)





#--- contrasts for MAIN EFFECTS
# compares each level's deviance from the grand mean ("true" main effect)
# just to check, does this make things different?
contrasts(data$condition) = contr.sum 

m2.main = lmer(RT ~ condition*agegroup + (1|ID), data = data)
summary(m2.main)
# no harsh differences in fixed effect estimations






















#--- initial simple regression model to check basic assumptions
simreg = lm(RT ~ condition, data = data)
plot(fitted(simreg),residuals(simreg))
# new column: log-transform RT
data$logRT = log(data$RT)
# plot logRT
hist(data$logRT, breaks = 80, col="steelblue")





#--------- MODEL ASSUMPTIONS ---------#
qqnorm(resid(m0))
plot(fitted(m0), resid(m0))
hist(resid(m0), breaks=100)

fixef(m0) # extract models fixed effects
ranef(m0) # extract models random effects
resid(m0) # extract models residuals
# coef(null)            # extracts model coefficients
# confint(res, level = 0.95, method = "Wald")


#--------- LIST OF SUBSETTED/AGGREGATED DATA SETS ---------#
#--- running list of all the subsets & aggregates used for exploration & analysis

# agg for every condition combination per ID
agg_all = data %>% group_by(ID, agegroup, wm, diff, conflict, condition) %>%
  summarise(.groups = "keep", n = n(), logRT=mean(logRT), R=mean(R))

# agg per ID
agg_id = data %>% group_by(ID, wm) %>%
  summarise(.groups = "keep", n = n(), logRT = mean(logRT), R = mean(R))

# agg per agegroup
agg_age = data %>% group_by(agegroup) %>%
  summarise(.groups = "keep", n = n(), logRT = mean(logRT), R = mean(R), 
            wm = mean(wm, na.rm = TRUE))

# agg per conflict
agg_cnf = data %>% group_by(conflict) %>%
  summarise(.groups = "keep", n = n(), logRT = mean(logRT), R = mean(R), 
            wm = mean(wm, na.rm = TRUE))



#--------- PLOT ANALYSIS RESULTS ---------#


