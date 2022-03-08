#############################################################
########### PROBABILISTIC & STATISTICAL MODELLING ########### 
#############################################################
# supporting R code for plotting & analysis, winter semester 2021/2022
# name: neele elbersgerd

# Rproject 'StatsPaper' 
# file '.Rprofile' automatically loads necessary packages 
rm(list = ls())

#--------- DATA QUICK IMPORT ---------#
## read in data file created in script 'import'
data = read.table("data/data_delay_conditioning.txt", header = TRUE, sep = ",", dec = ".")
# look at the data
head(data)
  ## 
  # ID: participant ID,
  # A: amount of reward for first option
  # DA: delay until reward A is received (7, 14 days)
  # B: amount of reward for second option
  # DB: additional delay in days (on top of DA) of receiving reward B (14, 28, 42 days)
  # R: chose later (delayed) option (= 1), binary
  # RT: response time in ms
  # diff: % difference in reward
  # agegroup: younger adults vs. older adults
  # conflict: low vs. high conflict (based on % difference)
  # option: immediate vs. delayed
  # delay: two vs. six weeks
  # ...
  ##

# convert some variables to factors
cols = c("agegroup", "conflict", "option")
data[cols] = sapply(data[cols], factor) # convert to factor
# add logRT column
data$logRT = log(data$RT)


#--------- DATA EXPLORE ---------#
# aggregate response variables over trials
data_agg = data %>% group_by(ID, agegroup, diff, conflict, option, wm) %>%
  summarise(.groups="keep", n = n(), logRT=mean(logRT), R=mean(R))

# describe covariates
cols = c("agegroup", "diff", "conflict", "option", "wm")
summary(data_agg[cols])
# describe response variables
summary(data_agg[c("RT", "R")])


### plotting

# scatterplot wm & RT
ggplot(data, aes(wm, logRT)) + 
  geom_point(aes(colour = conflict), na.rm = TRUE) + 
  geom_smooth(aes(colour = conflict), method = lm, na.rm = TRUE,  #aes(fill=R),
              alpha = 0.2) +
  theme_bw()

# boxplot & histogram logRT
ggplot(data, aes(logRT)) +
  geom_histogram(bins=40) +
  theme_bw()
ggplot(data, aes(logRT)) +
  geom_boxplot(na.rm = TRUE) + 
  theme_bw()

# boxplot working memory
ggplot(data, aes(wm)) +
  geom_boxplot(na.rm = TRUE) + 
  theme_bw()

#--------- ANALYSIS AND MODEL BUILD ---------#



#--------- PLOT ANALYSIS RESULTS ---------#

