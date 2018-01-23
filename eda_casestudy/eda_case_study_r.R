
setwd( "D:/MyDocs/eda/eda_data/_Upgrad")
setwd("~/Downloads/eda_data/eda_casestudy")
getwd()

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(scales)

# import the loan csv file for analysis
loan_df <- read.csv("loan.csv",stringsAsFactors = F)

# back up the loan data_frame
loan_bkp <- loan_df

# review the loan data frame
str(loan_df)
# review data frame has several unwanted columns from "collections_12_mths_ex_med"

# Data Cleaning

# summary of loan_data frame
sapply(loan_df, summary)
colnames(loan_df)
# ignore unwanted columns
which(colnames(loan_df) == "collections_12_mths_ex_med"):length(colnames(loan_df))


# ignoring columns that has NAs
new_loan_df <- loan_df[,1:which(colnames(loan_df) == "collections_12_mths_ex_med")-1]


# identifying and deleting other unwanted columns

# column numbers of unwanted columns are
-which(colnames(loan_df) %in% c("initial_list_status","url","pymnt_plan","funded_amnt_inv","zip_code","earliest_cr_line","mths_since_last_record","pub_rec","out_prncp_inv","total_pymnt_inv","next_pymnt_d"))

new_loan_df <- new_loan_df[,-which(colnames(new_loan_df) %in% c ("initial_list_status",
                                   "url","pymnt_plan","funded_amnt_inv","zip_code","earliest_cr_line",
                                   "mths_since_last_record","pub_rec","out_prncp_inv","total_pymnt_inv","next_pymnt_d"))]

View(new_loan_df)

# converting the loan data to factors
# converting data to factors

new_loan_df$id <- as.factor(new_loan_df$id)
new_loan_df$member_id <- as.factor(new_loan_df$member_id)
new_loan_df$loan_status <- as.factor(new_loan_df$loan_status)
new_loan_df$addr_state <- as.factor(new_loan_df$addr_state)
new_loan_df$grade <- as.factor(new_loan_df$grade)
new_loan_df$sub_grade <- as.factor(new_loan_df$sub_grade)
new_loan_df$delinq_2yrs <- as.factor(new_loan_df$delinq_2yrs)



# converting the columns to lower case characters
new_loan_df$loan_status <- tolower(new_loan_df$ loan_status)
new_loan_df$ emp_title <- tolower(new_loan_df$emp_title)
new_loan_df$home_ownership <- tolower(new_loan_df$home_ownership)
new_loan_df$verification_status <- tolower(new_loan_df$verification_status)
new_loan_df$title <- tolower(new_loan_df$title)
new_loan_df$purpose <- tolower(new_loan_df$purpose)


# removing the "years" from years columns, "%"
# renaming column headers
names(new_loan_df)[which(colnames(new_loan_df) %in% c("term","int_rate","emp_length","revol_util"))] <- c("term_mnths","int_per","emp_yrs","revol_util_per")
View(new_loan_df)

# removing years, months, % symbols from data
new_loan_df$int_per <- str_replace(new_loan_df$int_per,pattern = "%",replacement = "")
new_loan_df$term_mnths <-str_replace(new_loan_df$term_mnths,pattern = " months",replacement = "")
new_loan_df$emp_yrs <- gsub(pattern = "  years| year|s|+s| |/",replacement = "",x = new_loan_df$emp_yrs,ignore.case = T)
new_loan_df$revol_util_per <- str_replace(new_loan_df$revol_util_per,pattern = "%",replacement = "")


# convert the interest rates to numeric
new_loan_df$int_per <- as.numeric(new_loan_df$int_per)

# rounding up the interest rates to 2 decimals
new_loan_df$int_per <- round(new_loan_df$int_per,digits = 2)

#convert term loan, revol_util_per to numeric
new_loan_df$term_mnths <- as.numeric(new_loan_df$term_mnths)
new_loan_df$revol_util_per <- as.numeric(new_loan_df$revol_util_per)


# remove decimals from the annual income, principal, late fee, total payments, recovery fees etc as these details are insignificant
colnames(new_loan_df)
new_loan_df[c("annual_inc","out_prncp","total_rec_late_fee","total_pymnt",
              "installment","total_rec_prncp","total_rec_int","recoveries",
              "collection_recovery_fee","last_pymnt_amnt")] <- round(new_loan_df[c("annual_inc","out_prncp",
                                                                                   "total_rec_late_fee","total_pymnt",
                                                                                   "installment","total_rec_prncp",
                                                                                   "total_rec_int","recoveries","collection_recovery_fee",
                                                                                   "last_pymnt_amnt")],0)



# checking for duplicates
sum(duplicated(new_loan_df$id,new_loan_df$member_id))
# no duplicates identified.

# checking outliers in loan amounts, interest rates
quantile(x = new_loan_df$loan_amnt,na.rm = T)



# outliers in loan amount
ggplot(new_loan_df,aes(x = loan_status,y = loan_amnt)) + geom_boxplot()
# There are outliers in charged_off and fully_paid customers

# outliers in interest rate
ggplot(new_loan_df,aes(x = loan_status,y = int_per)) + geom_boxplot()
quantile(new_loan_df$int_per)

#

# calcuating mean loan amounts
summary(c(new_loan_df$loan_amnt,new_loan_df$int_per,new_loan_df$collection_recovery_fee))

library(scales)
percent(sum((new_loan_df$loan_status == "charged off") & (new_loan_df$loan_amnt > quantile(x = new_loan_df$loan_amnt,probs = 0.95)))/sum(new_loan_df$loan_status == "charged off"))
percent(sum((new_loan_df$loan_status == "current") & (new_loan_df$loan_amnt > quantile(x = new_loan_df$loan_amnt,probs = 0.95)))/sum(new_loan_df$loan_status == "current"))
percent(sum((new_loan_df$loan_status == "fully paid") & (new_loan_df$loan_amnt > quantile(x = new_loan_df$loan_amnt,probs = 0.95)))/sum(new_loan_df$loan_status == "fully paid",na.rm = T))


# year and date formating of dates

# get the year of last payment due
last_payment_due <- parse_date_time(x = new_loan_df$last_pymnt_d,orders = "%b-%y",tz = "Asia/Kolkata")
new_loan_df$lpd_year <- format(last_payment_due,"%Y")

# Get the month of the last payment due
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

# loan grade
new_loan_df %>% filter(new_loan_df$loan_status != "fully paid" ) %>% 
  ggplot(aes(fct_infreq(grade,ordered = T),fill=loan_status)) + geom_bar(stat = "count",aes(identity = "count")) +
  geom_text(stat = "count",aes(label = (..count..),vjust = -1, hjust = 0.5))

# most of the charged_off loans are B and C grade loans

# Sub_grade
ggplot(new_loan_df,aes(fct_infreq(grade,ordered = T))) + 
  geom_bar() +  
  geom_text(stat = "count",aes(label = paste(round(x = (..count..)*100/sum(..count..),digits = 2),"%"),vjust = -0.5, hjust = 0.5))

new_loan_df_charged_off <- subset(new_loan_df,loan_status == "charged off")
View(new_loan_df_charged_off)



str(new_loan_df_charged_off)
# calculating the percentage of pub rec with Zeros
options(scipen=999)
round(table(new_loan_df$pub_rec)/length(new_loan_df$pub_rec),digits = 2)









