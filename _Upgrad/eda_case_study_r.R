setwd( "D:/MyDocs/_Upgrad")
setwd("/Users/Vijay/Downloads/eda_data/_Upgrad")

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)


loan_df <- read.csv("loan.csv",stringsAsFactors = F)
head(loan_df)

str(loan_df)

# Data Cleaning

loan_df$id <- as.factor(loan_df$id)
loan_df$member_id <- as.factor(loan_df$member_id)

# summary of loan_data frame
sapply(loan_df, summary)

# ignore unwanted columns
which(colnames(loan_df) == "collections_12_mths_ex_med"):length(colnames(loan_df))


# ignoring columns that do not have data
new_loan_df <- loan_df[,1:which(colnames(loan_df) == "collections_12_mths_ex_med")-1]


# converting the loan data to factors
new_loan_df$loan_status <- as.factor(new_loan_df$loan_status)
new_loan_df$addr_state <- as.factor(new_loan_df$addr_state)
new_loan_df$grade <- as.factor(new_loan_df$grade)
new_loan_df$grade <- as.factor(new_loan_df$sub_grade)
new_loan_df$delinq_2yrs <- as.factor(new_loan_df$delinq_2yrs)
new_loan_df$pub_rec <- as.factor(pub_rec)

# deleting unwanted columns
new_loan_df <- new_loan_df[,-which(colnames(new_loan_df) == "initial_list_status")]
new_loan_df <- new_loan_df[,-which(colnames(new_loan_df) == "url")]
new_loan_df <- new_loan_df[,-which(colnames(new_loan_df) == "pymnt_plan")]
summary(as.factor(new_loan_df$delinq_2yrs))
summary(as.factor(new_loan_df$pub_rec))

sapply(new_loan_df, summary)

View(new_loan_df)

# converting the columns to lower case characters
new_loan_df$loan_status <- tolower(new_loan_df$ loan_status)
new_loan_df$ emp_title <- tolower(new_loan_df$emp_title)
new_loan_df$home_ownership <- tolower(new_loan_df$home_ownership)
new_loan_df$verification_status <- tolower(new_loan_df$verification_status)
new_loan_df$title <- tolower(new_loan_df$title)
new_loan_df$purpose <- tolower(new_loan_df$purpose)


# year and date formating of dates

last_payment_due <- parse_date_time(x = new_loan_df$last_pymnt_d,orders = "%b-%y",tz = "Asia/Kolkata")
new_loan_df$lpd_year <- format(last_payment_due,"%Y")

new_loan_df$lpd_month <- format(last_payment_due,"%m")

new_loan_df$lpd_month <- as.factor(new_loan_df$lpd_year)




# "Identification of RISKY applicants using EDA is the aim of this case study."
# - lending loans to ?risky? applicants is the largest source of financial loss (called credit loss). 
# - The credit loss is the amount of money lost by the lender when the borrower refuses to pay or runs away with the money owed. In other words, borrowers who default cause the largest amount of loss to the lenders. 
# - In this case, the customers labelled as 'charged-off' are the 'defaulters'. 


# RISK ANALYTICS
# the company wants to understand the driving factors (or driver variables) behind loan default,
# i.e. the variables which are strong indicators of default. 
# The company can utilise this knowledge for its portfolio and risk assessment. 

# To develop your understanding of the domain, 
# you are advised to independently research a little about risk analytics 
# (understanding the types of variables and their significance should be enough).


# UNIVARIATE ANLYSIS

# Univariate analysis of Loan_status
ggplot(new_loan_df,aes(loan_status)) + 
  geom_bar(stat = "count") + 
  geom_text(stat = "count",aes(label = paste(round(x = (..count..)*100/sum(..count..),digits = 2),"%"),vjust = -0.5, hjust = 0.5))

# number of loans by year
ggplot(new_loan_df,aes(x = lpd_year)) + geom_bar()


# numer of loans by month
ggplot(new_loan_df,aes(x=lpd_month)) + geom_bar()

# interest rates
ggplot(new_loan_df,aes(x = id,y = int_rate)) + geom_point()


# number of loans in the month
# study the profile of full paid customers
# study the profile of Charged-off customers
# study the profile of current customers


# UNIVARIATE ANALYSIS OF STATES
new_loan_df %>% 
  group_by(addr_state) %>% 
  tally() %>% 
  arrange(desc(n))%>% 
  ggplot(aes(addr_state,n)) + geom_col(aes(reorder(addr_state,-n)))

# Method#2 to reorder the states by ascending order.
ggplot(new_loan_df,aes(reorder(addr_state,-table(addr_state)[addr_state]))) +geom_bar()

# method#3 
library(forcats)
ggplot(new_loan_df,aes(fct_infreq(addr_state,ordered = T))) + geom_bar()

# Sub_grade
ggplot(new_loan_df,aes(fct_infreq(grade,ordered = T))) + geom_bar()

new_loan_df_charged_off <- subset(new_loan_df,new_loan_df$loan_status == "charged off")
str(new_loan_df_charged_off)

