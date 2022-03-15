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
hist(data$RT, breaks = 40, col="#008080")
boxplot(data$RT, col="#008080")

# histogram condition (RT)
ggplot(data, aes(x = RT)) +
  geom_histogram(aes(color = condition, fill = condition),
                 position = "identity", bins = 40, alpha = 0.4) +
  scale_color_manual(values = c("#008080", "#E7B800")) +
  scale_fill_manual(values = c("#008080", "#E7B800")) + 
  theme_bw()

# boxplot agegroup (RT)
ggplot(data, aes(y = RT)) +
  geom_boxplot(aes(color = agegroup, fill = agegroup), alpha = 0.4) +
  scale_color_manual(values = c("#008080", "#E7B800")) +
  scale_fill_manual(values = c("#008080", "#E7B800")) + 
  theme_bw()

# boxplot agegroup x condition (RT)
ggplot(data, aes(x = condition, y = RT, color = agegroup)) +
  geom_boxplot() +
  scale_color_manual(values = c("#008080", "#E7B800")) +
  theme_bw()

#--- outlier checking
# cutoff via 300ms lower threshold >> 135 trials
subset(data, RT < 300)["RT"] %>% nrow()
data = data %>% subset(RT >= 300)

# cutoffs via standard deviation ?
cut_low = mean(data$RT) - 3*sd(data$RT)
cut_high = mean(data$RT) + 3*sd(data$RT)
# how many values are above or below 3SD of mean? >> 264 below, 0 above
# data %>% subset(RT < cut_low | RT > cut_high) %>% nrow()
### REMOVE? 




#--------- ANALYSIS AND MODEL BUILD ---------#
#--- null model, random intercept
m0 = lmer(RT ~ 1 + (1|ID), data = data) 
summary(m0)

#--- contrasts checking, Dummy Coding
# compares each level of a variable to the specified reference level
contrasts(factor(data$condition))
contrasts(factor(data$agegroup))
# relevel factors if necessary
data$condition = relevel(factor(data$condition), ref = "delayed")
data$agegroup = relevel(factor(data$agegroup), ref = "younger adults")

#--- adding condition
m_cond = lmer(RT ~ condition + (1|ID), data = data)
summary(m_cond)
anova(m0, m_cond)
# delayed as the reference level: we can see that there is a significant effect of condition 
# Participants seemed to react faster to choice pairs with immediate options, 
# compared to choice pairs with only delayed options

#--- adding age group
# is there an effect of agegroup and an interaction between age group and condition?
m_cond_age = lmer(RT ~ condition*agegroup + (1|ID), data = data)
summary(m_cond_age)
# older participants differ significantly from younger adults in their reaction time
# there is a significant interaction between age group and condition
anova(m0, m_cond_age)


#--- adding random slope for condition
m_ranslop = lmer(RT ~ condition*agegroup + (1+condition|ID), data = data)
summary(m_ranslop)
anova(m_cond_age, m_ranslop)


#--- probing the interaction
data_imm = subset(data, condition == "immediate")
data_del = subset(data, condition == "delayed")

m_imm = lmer(RT ~ agegroup + (1|ID), data = data_imm)
m_del = lmer(RT ~ agegroup + (1|ID), data = data_del)
summary(m_imm)
summary(m_del)

#--- confidence intervals
confint(m_ranslop)



#--------- MODEL ASSUMPTIONS ---------#
#--- normality assumption
qqnorm(resid(m_ranslop), col = "#008080", pch = 1, cex = 0.8, 
       bty = "l", main = NULL); qqline(resid(m_ranslop))
#hist(resid(m0), breaks=100)

#--- resid fitted plot
plot(fitted(m_ranslop), resid(m_ranslop), col = "#008080", pch = 1, cex = 0.8,
     xlab="fitted", ylab="residuals", bty = "l", main = NULL)

# log-transform RT?
#data$logRT = log(data$RT)
#hist(data$logRT, breaks = 80, col="steelblue")








#--------- PLOT ANALYSIS RESULTS ---------#
# interaction plot with raw data underneath
ggplot(data, aes(y = RT, x = condition)) +
  geom_point(aes(colour = agegroup), position = "jitter", alpha = 0.05) +
  stat_summary(aes(colour=agegroup), fun = "mean", size = 2, geom = "point") +
  stat_summary(aes(group = agegroup, colour = agegroup), fun = mean, geom= "line") + 
  scale_color_manual(values = c("#008080", "#E7B800")) +
  theme_bw()

# extracting random intercept and slope values
plot_fit = data.frame(ID = levels(data$ID),
                      intercepts = coef(m_ranslop)$ID[,1],
                      slopes = coef(m_ranslop)$ID[,2])
plot_fit$agegroup = 0
plot_fit$agegroup[plot_fit$ID <= 362] = "younger adults"
plot_fit$agegroup[plot_fit$ID > 362] = "older adults"
plot_fit$agegroup = as.factor(plot_fit$agegroup)


data$predicted = predict(m_ranslop)   # save the predicted values from the model
data$residuals = residuals(m_ranslop) # Save the residual values from the model

# plot the actual and predicted values
ggplot(data, aes(x = condition, y = RT)) +
  geom_smooth(aes(group=agegroup),method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = condition, yend = predicted), alpha = .2) +  # alpha to fade lines
  # > Color AND size adjustments made here...
  #geom_point(aes(color = abs(residuals), size = abs(residuals))) + # size also mapped
  #scale_color_continuous(low = "black", high = "red") +
  #guides(color = FALSE, size = FALSE) +  # Size legend also removed
  #geom_point(aes(x = predicted), shape = 1) +
  theme_bw()


dotplot(ranef(m_ranslop))

ggplot(data, aes(x = condition, y = RT)) + 
  geom_point(shape = 20, alpha = 0.4, position="jitter") + 
  #geom_abline(slope = plot_fit$slopes, intercept=plot_fit$intercepts, alpha = 0.2) +
  geom_abline(slope=summary(m_ranslop)$coeff[2,1], intercept=summary(m_ranslop)$coeff[1,1], colour="red") +
  theme_bw()
  

describeBy(x = data$RT, list(data$condition,data$agegroup), mat = TRUE)

  