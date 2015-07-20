setwd("~/Desktop/R_Work")
#source("/Users/David/Desktop/R_Work/compress_news.r")
install.packages("arules")
library(arules)
#install.packages("arulesViz")
#library(arulesViz)

# query used to pull the .psv file: 
# select pii_id, gender_full, marriage_groups, age_groups, income_groups, education_groups, race_decode, 
# wm_score from davidw.wm_demos where wm_score > -1 and wm_score < 10

data<-read.table("~/Desktop/R_Work/wm_score_demos_analysis.psv", header=F, sep = "|", na.strings = c("","NA"), colClasses=c(rep("character",8)))
# 2,073,331 individuals with wm_scores between 0 and 9 inclusive

##colClasses=c(rep("factor",53),rep("numeric",3)

# copy data to a new dataframe
data_original <- data

# create new column for wm_score bins and populate with bins for wm_score
data$score_bin <- "primary"

# give names to the columns
names(data) <- c("pii_id", "gender", "marital_status", "age_bucket", "income","education","race", "wm_score", "score_bin")


data$score_bin[data$wm_score %in% c(1:3)]<-"occasional"
data$score_bin[data$wm_score %in% c(4:6)]<-"secondary"
data$score_bin[data$wm_score %in% c(0)]<-"1"
data$score_bin <- as.factor(data$score_bin)
data[is.na(data)]<-"Unknown"


# convert columns data types to factors because it's needed for arules
#data$pii_id <- as.factor(data$pii_id)
data$gender <- as.factor(data$gender)
data$marital_status <- as.factor(data$marital_status)
data$age_bucket <- as.factor(data$age_bucket)
data$income <- as.factor(data$income)
data$education <- as.factor(data$education)
data$race <- as.factor(data$race)
#data$wm_score <- as.factor(data$wm_score)

# drop pii_id column and wm_score because they're uninformative 
data <- subset(data, select = -c(pii_id, wm_score))

## data$occupation<-as.factor(apply(data,1,compress_occupation))

## choose only a certain number of demo columns
## sub_data <- data[,-c(2,3,4,8,13,14,40,41,42,48,49,50,26,34,37,40,31)] 

# coerce sub_data into a class called "transactions"
trans <- as(data,"transactions")

#rule<-apriori(trans,appearance=list(rhs=c("class=Same","class=Decrease","class=Increase"),default="lhs"))

    
table(data$score_bin)/dim(data)[1] # counts the proportion of the different values in a column


#######################################################

rule.primary <- apriori(trans, parameter = list(minlen = 1, maxlen = 6, supp = 0.02, conf = 0.40), appearance = list(rhs=c("score_bin=primary"), default="lhs"))
rule.primary <- sort(rule.primary, by = "lift")
inspect(rule.primary)
## plot(rule.primary[1:2])

# prune to remove redundant subsets 
prune.subset <- is.subset(rule.primary, rule.primary)
prune.subset[lower.tri(prune.subset,diag=T)]<-NA
redundant<-colSums(prune.subset,na.rm=T)>=1
rules.pruned.primary<-rule.primary[!redundant]
inspect(rules.pruned.primary)


#######################################################

rule.secondary <- apriori(trans, parameter = list(minlen = 1, maxlen = 6, supp = 0.02, conf = 0.315), appearance = list(rhs=c("score_bin=secondary"),default="lhs"))
rule.secondary <- sort(rule.secondary, by = "lift")
inspect(rule.secondary)
## plot(rule.secondary[1:2])

# prune to remove redundant subsets 
prune.subset <- is.subset(rule.secondary, rule.secondary)
prune.subset[lower.tri(prune.subset,diag=T)]<-NA
redundant<-colSums(prune.subset,na.rm=T)>=1
rules.pruned.secondary <- rule.secondary[!redundant]
inspect(rules.pruned.secondary)

#######################################################

rule.occasional  <- apriori(trans, parameter = list(minlen = 1, maxlen = 6, supp = 0.02, conf = 0.315), appearance = list(rhs=c("score_bin=occasional"),default="lhs"))
rule.occasional  <- sort(rule.occasional , by = "lift")
inspect(rule.occasional)
## plot(rule.occasional [1:2])

# prune to remove redundant subsets 
prune.subset <- is.subset(rule.occasional , rule.occasional )
prune.subset[lower.tri(prune.subset,diag=T)]<-NA
redundant<-colSums(prune.subset,na.rm=T)>=1
rules.pruned.occasional <- rule.occasional [!redundant]
inspect(rules.pruned.occasional)

#######################################################

rule.one  <- apriori(trans, parameter = list(minlen = 1, maxlen = 6, supp = 0.015, conf = 0.03), appearance = list(rhs=c("score_bin=1"),default="lhs"))
rule.one  <- sort(rule.one , by = "lift")
inspect(rule.one)
## plot(rule.one [1:2])

# prune to remove redundant subsets 
prune.subset <- is.subset(rule.one , rule.one )
prune.subset[lower.tri(prune.subset,diag=T)]<-NA
redundant<-colSums(prune.subset,na.rm=T)>=1
rules.pruned.one <- rule.one [!redundant]
inspect(rules.pruned.one)

