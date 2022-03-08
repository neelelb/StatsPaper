#############################################################
########### PROBABILISTIC & STATISTICAL MODELLING ########### 
#############################################################
# supporting R code for import of data, winter semester 2021/2022
# name: neele elbersgerd

# Rproject 'StatsPaper' 
# file '.Rprofile' automatically loads necessary packages 

#--------- DATA IMPORT EXPERIMENTAL ---------#
# read in single text files
setwd('data/data_single/')
filelist = list.files()
exp_data = rbindlist(sapply(filelist, fread, simplify = FALSE))
setwd('../..')
# exclude trials on which subjects did not respond
exp_data = subset(exp_data, exp_data$RT > 0)


### create variables of interest
# difference between rewards
exp_data$diff = ((exp_data$B-exp_data$A)/exp_data$A) %>% round(digits = 3)

# create age group variable
exp_data$agegroup=0
for (i in 1:length(exp_data$ID)){
  if (exp_data$ID[i] <= 362){
    exp_data$agegroup[i] = "younger adults"}
  else if (exp_data$ID[i] > 362){
    exp_data$agegroup[i] = "older adults"} 
}

# create conflict variable
exp_data$conflict = 0
for (i in 1:length(exp_data$diff)){
  if (exp_data$diff[i] <= 0.01 | exp_data$diff[i] == 0.50){
    exp_data$conflict[i] = "low conflict"}
  else {
    exp_data$conflict[i] = "high conflict"} 
}

# create option variable coding "immediate" or "delayed" condition
exp_data$option = 0
for (i in 1:length(exp_data$DA)){
  if (exp_data$DA[i] == 7){
    exp_data$option[i] = "immediate"}
  else if (exp_data$DA[i] == 14){
    exp_data$option[i] = "delayed"} 
}

# create delay variable translating delay of B into weeks
exp_data$delay = 0
for (i in 1:length(exp_data$DB)){
  if (exp_data$DB[i] == 14){
    exp_data$delay[i] = "two weeks"}
  else if (exp_data$DB[i] == 28){
    exp_data$delay[i] = "four weeks"} 
  else if (exp_data$DB[i] == 42){
    exp_data$delay[i] = "six weeks"} 
}


#--------- DATA IMPORT COVARIATES ---------#
# import
cov_y = read.table("data/itc_young_cov.txt", header = TRUE, dec = ".")
cov_o = read.table("data/itc_old_cov.txt", header = TRUE, dec = ".")
# add column of age group 
cov_y$agegroup = "younger adults"
cov_o$agegroup = "older adults"
# combine into one covariate file & rename to fit exp_data
cov_all = rbind(cov_y, cov_o)
names(cov_all)[1] = "ID"


#--------- COMBINE ---------#
# create one data file with experimental and covariate data
data = left_join(exp_data, cov_all, by = c("agegroup", "ID"))

# write final data to text file to make import easy in script 2
# this way, variables are created once and stored for use in another session
write.table(data, 'data/data_delay_conditioning.txt',
            sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)


