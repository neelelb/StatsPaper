#############################################################
# TERM PAPER - CODE FOR DATA IMPORT
# name...................Neele Elbersgerd
# matriculation..........5564097
# course.................Probabilistic and Statistical Modelling
# program................Master of Cognitive Neuroscience, FU Berlin
# instructor.............Dr. Benjamin Eppinger
# semester...............Winter term 2021/2022
#############################################################
# associated Rproject: 'StatsPaper' 
# file '.Rprofile' automatically loads necessary packages 


#--------- DATA IMPORT EXPERIMENTAL ---------#
#--- read in single text files
setwd('data/data_single/')
filelist = list.files()
exp_data = rbindlist(sapply(filelist, fread, simplify = FALSE))
setwd('../..')
# exclude trials on which subjects did not respond
exp_data = subset(exp_data, exp_data$RT > 0)


#--- create variables of interest
# difference between rewards
exp_data$diff = ((exp_data$B-exp_data$A)/exp_data$A) %>% round(digits = 3)

# create age group variable
exp_data$agegroup=0
exp_data$agegroup[exp_data$ID <= 362] = "younger adults"
exp_data$agegroup[exp_data$ID > 362] = "older adults"

# create conflict variable
exp_data$conflict = 0
exp_data$conflict[exp_data$diff <= 0.01 | exp_data$diff == 0.50] = "low conflict"
exp_data$conflict[exp_data$diff > 0.01 & exp_data$diff < 0.50] = "high conflict"

# create condition variable coding "immediate" or "delayed" condition
exp_data$condition = 0
exp_data$condition[exp_data$DA == 7]  = "immediate"
exp_data$condition[exp_data$DA == 14]  = "delayed"

# create delay variable translating delay of B into weeks
exp_data$delay = 0
exp_data$delay[exp_data$DB == 14]  = "two weeks"
exp_data$delay[exp_data$DB == 28]  = "four weeks"
exp_data$delay[exp_data$DB == 42]  = "six weeks"

# create choice of participants as character variable
exp_data$choice = 0
exp_data$choice[exp_data$R == 1] = "late" 
exp_data$choice[exp_data$R == 0] = "early" 



#--------- DATA IMPORT COVARIATES ---------#
cov_y = read.table("data/itc_young_cov.txt", header = TRUE, dec = ".")
cov_o = read.table("data/itc_old_cov.txt", header = TRUE, dec = ".")

# add column of age group 
cov_y$agegroup = "younger adults"
cov_o$agegroup = "older adults"

# combine into one covariate file & rename ID column to match exp_data
cov_all = rbind(cov_y, cov_o)
names(cov_all)[1] = "ID"


#--------- COMBINE TO ONE FILE ---------#

#--- create one data file with experimental and covariate data
data = left_join(exp_data, cov_all, by = c("agegroup", "ID"))

# write final data to text file
# this way, one can load the data with all variables already created in the analysis script
write.table(data, 'data/data_all.txt',
            sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)
