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
    # R: chose later (delayed) option (= 1); RT: response time in ms
    # diff: % difference in reward; conflict: low vs. high conflict (based on % difference)
    # agegroup: younger adults vs. older adults
    # condition: immediate vs. delayed option A
    # delay: two, four, six weeks
    # choice: did the participants choose the early or late option
    # em: episodic memory; wm: working memory ; gf: fluid intelligence; speed: speed of processing
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

# histogram agegroup (RT)
ggplot(data, aes(x = RT)) +
  geom_histogram(aes(color = agegroup, fill = agegroup),
                 position = "identity", bins = 40, alpha = 0.4) +
  scale_color_manual(values = c("#008080", "#E7B800")) +
  scale_fill_manual(values = c("#008080", "#E7B800")) + 
  theme_bw()
# look at skewness of RT data
skewness(data$RT)
skewness(data_y$RT);skewness(data_o$RT)

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
# how many values are above or below 3SD of mean?
data %>% subset(RT < cut_low | RT > cut_high) %>% nrow()
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
hist(resid(m0), breaks=100)

#--- resid fitted plot
plot(fitted(m_ranslop), resid(m_ranslop), col = "#008080", pch = 1, cex = 0.8,
     xlab="fitted", ylab="residuals", bty = "l", main = NULL)

# log-transform RT?
#data$logRT = log(data$RT)
#hist(data$logRT, breaks = 80, col="steelblue")




#--------- PLOT ANALYSIS RESULTS ---------#
summary = describeBy(x = data$RT, list(data$condition,data$agegroup), mat = TRUE)

#--- boxplot with densities and raw data
aggdata = data %>% group_by(ID, agegroup, condition) %>% summarise(.groups = "keep", n = n(), RT = mean(RT)/1000)

pdf(file = "figures/pirateplot.pdf", width = 12, height = 10)
pirateplot(formula = RT ~ condition + agegroup, data = aggdata,
           theme = 3, pal = c("#008080", "#E7B800"),
           bean.f.o = 0.3, bean.b.col = c("#008080", "#E7B800"), bean.b.o = 0.3,
           point.o = 0.5, point.pch = 20, point.cex = 1.3,
           point.col = c("#008080", "#E7B800"),
           inf.method = "iqr", inf.disp = "line", inf.f.col = "black", 
           avg.line.lwd = 3, 
           sortx = "mean",
           gl.col = "lightgrey", ylim = c(0.4,3.6), bty="n", 
           ylab = 'Reaction Time [in s]', xaxt='n', cex.lab = 1.5)
          text(c(1.5, 4.5), c(0.4,0.4), c("Younger Adults", "Older Adults"), cex = 1.5)
          legend("bottom", yjust = 1, legend = c("Immediate", "Delayed"), bty="n", col = c("#008080", "#E7B800"), pch = c(20, 20), cex = 1.5)
dev.off()


#--- plot the random effects
my.settings = list(
  plot.symbol = list(pch = 20, col = "#008080"),
  strip.background = list(col = "white"),
  strip.border = list(col = "transparent"),
  plot.line.col = "black",
  axis.line = list(col = c(1,0)),
  par.main.text = list(col = "transparent")
)

pdf(file = "figures/randomeffects.pdf", width = 12, height = 10)
dotplot(ranef(m_ranslop), 
        strip = strip.custom(factor.levels = c("Random Intercepts", "Random slopes for condition")),
        ylab = list(label="Participant", cex=1.5), bty="n",
        scales = list(alternating=FALSE, tck = c(1,0), y = list(tick.number= NULL, cex=0), x = list(cex = 1.3)), 
        between = list(x = 0.5), par.strip.text=list(cex=1.5),
        par.settings = my.settings)
dev.off()


