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



#--------- LIST OF SUBSETTED/AGGREGATED DATA SETS ---------#
# running list of all the subsets & aggregates used for exploration & analysis

# subset of younger or older adults
data_y = data %>% subset(agegroup == "younger adults")
data_o = data %>% subset(agegroup == "older adults")


# agg for every condition combination per ID
agg_all = data %>% group_by(ID, agegroup, wm, diff, conflict, condition) %>%
  summarise(.groups = "keep", n = n(), logRT=mean(logRT), R=mean(R))

# agg per ID
agg_id = data %>% group_by(ID, wm) %>%
  summarise(.groups = "keep", n = n(), logRT = mean(logRT), R = mean(R))
# agg per agegroup
agg_age = data %>% group_by(agegroup) %>%
  summarise(.groups = "keep", n = n(), logRT = mean(logRT), R = mean(R), wm = mean(wm, na.rm = TRUE))
# agg per conflict
agg_cnf = data %>% group_by(conflict) %>%
  summarise(.groups = "keep", n = n(), logRT = mean(logRT), R = mean(R), wm = mean(wm, na.rm = TRUE))





#--------- DATA EXPLORE ---------#
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
agg_id$wm %>% summary()

# describe response variables
data[c("RT", "R")] %>% summary()

# sd of monetary reward
data %>% summarise(SD = sd(A))
data %>% summarise(SD = sd(B))


### plotting of raw RT
plot(data$RT)
hist(data$RT, breaks = 40, col="steelblue")
  skewness(data$RT)
boxplot(data$RT, col="steelblue")
# log-transform RT column
data$logRT = log(data$RT)

### plotting logRT
hist(data$logRT, breaks = 80, col="steelblue")
boxplot(data$logRT, col="steelblue")






### outlier checking
# cutoff via 300ms lower threshold >> 135
subset(data, RT < 300)["logRT"] %>% nrow()
data = data %>% subset(RT >= 300)

# cutoffs via standard deviation ?? 
cut_low = mean(data$logRT) - 3*sd(data$logRT)
cut_high = mean(data$logRT) + 3*sd(data$logRT)
# how many values are above or below 3SD of mean? >> 264 below, 0 above
data %>% subset(logRT < cut_low | logRT > cut_high) %>% nrow()
### REMOVE? 


### plotting working memory
hist(data$wm, breaks = 40, col="steelblue")




# scatterplot wm & RT
plot(data$wm, data$logRT, col = "steelblue")


ggplot(data, aes(x=choice, y=RT)) +
  geom_boxplot() +
  theme_bw()

ggplot(data, aes(x=condition, y=RT)) +
  geom_boxplot() +
  theme_bw()





#--------- ANALYSIS AND MODEL BUILD ---------#
# look at coding scheme
contrasts(factor(data$condition))
#relevel factors if necessary
data$condition = relevel(factor(data$condition), ref = "immediate")

m0 = lmer(data$RT ~ 1 + (1|ID), data = data) 
summary(m0)


# this tests the amount of which delay differs from immediate: 
# a non significant effect of condition on RT. 
m1 = lmer(data$RT ~ 1 + condition + (1|ID), data = data) 
summary(m1)


# this tests "simple effects" for the levels of condition, when compared to 0
# What you see is that there is whether a significant difference in RT compared to 0 for the immediate nor for the delayed condition
m1.1 = lmer(data$RT ~ 0 + condition + (1|ID), data = data) 
summary(m1.1)


# random slope model
# did not converge without lmerControl options
m2 = lmer(data$RT ~ condition + (condition|ID), data = data,
          control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))) 
summary(m2)



anova(m0, m1)
anova(m1, m2)


# different contrasting
# Ben: if you want the “true” main effects and/or you have a factor with multiple levels it may make more sense to go for effects coding
contrasts(data$condition) = contr.sum




#--------- PLOT ANALYSIS RESULTS ---------#


