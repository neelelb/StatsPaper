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
cols = c("ID", "agegroup", "conflict", "option")
data[cols] = lapply(data[cols], factor) # convert to factor
# add logRT column
data$logRT = log(data$RT)


#--------- LIST OF SUBSETTED/AGGREGATED DATA SETS ---------#
# running list of all the subsets & aggregates used for exploration & analysis

# agg for every condition combination per ID
agg_all = data %>% group_by(ID, agegroup, wm, diff, conflict, option) %>%
  summarise(.groups="keep", n = n(), logRT=mean(logRT), R=mean(R))

# agg per ID
agg_id = data %>% group_by(ID, wm) %>%
  summarise(.groups="keep", n = n(), logRT=mean(logRT), R=mean(R))
# agg per agegroup
agg_age = data %>% group_by(agegroup) %>%
  summarise(.groups="keep", n = n(), logRT=mean(logRT), R=mean(R), wm = mean(wm, na.rm=TRUE))
# agg per conflict
agg_cnf = data %>% group_by(conflict) %>%
  summarise(.groups="keep", n = n(), logRT=mean(logRT), R=mean(R), wm = mean(wm, na.rm=TRUE))

# subset of younger or older adults
data_y = data %>% subset(agegroup = "younger adults")
data_o = data %>% subset(agegroup = "older adults")





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
data$option %>% table()
data$delay %>% table()

# summary of working memory variable
agg_id$wm %>% summary()

# describe response variables
data[c("RT", "R")] %>% summary()

# sd of monetary reward
data %>% summarise(SD = sd(A))
data %>% summarise(SD = sd(B))


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

