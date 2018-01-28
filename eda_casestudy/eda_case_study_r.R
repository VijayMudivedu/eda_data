
#setwd("D:/MyDocs/eda/eda_data/_Upgrad")
setwd("../eda_casestudy")
# setwd("~/OneDrive/OneDrive - Atimi Software Inc/Upgrad/_Upgrad/EDA/eda_casestudy")


#install.packages("tidyverse")
#install.packages("tvm")
#install.packages("scales")


library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(scales)
library(tvm)
library(reshape2)
library(ggthemes)
library(ggplot2)
library(forcats)
options(scipen = 999)
library(RColorBrewer)


# import the loan csv file for analysis
loan_master_df <- read.csv("loan.csv", stringsAsFactors = F)

# review the loan data frame
str(loan_master_df)

# subsetting charged_off loans for Risk Analytics
table(loan_master_df$loan_status)
loan_df_charged_off <- subset(x = loan_master_df, loan_status == "Charged Off")
head(loan_df_charged_off,n = 2)
View(loan_df_charged_off)

#------------------------------------------------------------------
# Data Cleaning
#------------------------------------------------------------------

# data frame has several unwanted columns from "collections_12_mths_ex_med"

# summary of loan_data frame
sapply(loan_df_charged_off, summary)
# total columns in the data frame
colnames(loan_df_charged_off)

#----------------------------
# list of columns to ignore unwanted columns with NAs and Zeros
#----------------------------
which(colnames(loan_df_charged_off) == "collections_12_mths_ex_med"):length(colnames(loan_df_charged_off))

# ignoring columns that has NAs
loan_df_charged_off <-
  loan_df_charged_off[, 1:which(colnames(loan_df_charged_off) == "collections_12_mths_ex_med") - 1]

colnames(loan_df_charged_off)

# identifying and deleting other unwanted columns that aren't required for analysis
# column numbers of unwanted columns are-which(
which(colnames(loan_df_charged_off) %in% c(
  "initial_list_status",
  "url",
  "pymnt_plan",
  "funded_amnt_inv",
  "zip_code",
  "earliest_cr_line",
  "mths_since_last_record",
  "pub_rec",
  "out_prncp_inv",
  "total_pymnt_inv",
  "next_pymnt_d",
  "out_prncp",
  "delinq_2yrs",
  "mths_since_last_delinq",
  "last_credit_pull_d",
  "last_pymnt_amnt"
))



#----------------------------
# removing the identified the other unwanted columns. Columns do not provide enough insight
#----------------------------
loan_df_charged_off <-
  loan_df_charged_off[, -which(
    colnames(loan_df_charged_off) %in% c(
      "initial_list_status",
      "url",
      "pymnt_plan",
      "funded_amnt_inv",
      "zip_code",
      "earliest_cr_line",
      "mths_since_last_record",
      "pub_rec",
      "out_prncp",
      "out_prncp_inv",
      "total_pymnt_inv",
      "next_pymnt_d",
      "delinq_2yrs",
      "mths_since_last_delinq",
      "last_credit_pull_d",
      "last_pymnt_amnt"
    )
  )]

#----------------------------
# converting the columns to lower case characters
#----------------------------

loan_df_charged_off$emp_title <- tolower(loan_df_charged_off$emp_title)
loan_df_charged_off$home_ownership <- tolower(loan_df_charged_off$home_ownership)
loan_df_charged_off$verification_status <- tolower(loan_df_charged_off$verification_status)
loan_df_charged_off$title <- tolower(loan_df_charged_off$title)
loan_df_charged_off$purpose <- tolower(loan_df_charged_off$purpose)
loan_df_charged_off$loan_status <- tolower(loan_df_charged_off$loan_status)


# remaining columns are
colnames(loan_df_charged_off)

#----------------------------
# converting the loan_df_charged_off dataframe to factors
#----------------------------

loan_df_charged_off$grade <- as.factor(loan_df_charged_off$grade)
loan_df_charged_off$sub_grade <- as.factor(loan_df_charged_off$sub_grade)
loan_df_charged_off$home_ownership <- as.factor(loan_df_charged_off$home_ownership)
loan_df_charged_off$purpose <- as.factor(loan_df_charged_off$purpose)
loan_df_charged_off$inq_last_6mths <- as.factor(loan_df_charged_off$inq_last_6mths)

#----------------------------
# renaming the column headers of the below columns
#----------------------------

names(loan_df_charged_off)[which(colnames(loan_df_charged_off) %in% c("term", "int_rate", "emp_length", "revol_util"))] <-
  c("term_mnths", "int_per", "emp_yrs", "revol_util_per")

# removing years, months, % characters from the above listed columns
loan_df_charged_off$int_per <-
  str_replace(string = loan_df_charged_off$int_per,
              pattern = "%",
              replacement = "")
loan_df_charged_off$term_mnths <-
  str_replace(string = loan_df_charged_off$term_mnths,
              pattern = " months",
              replacement = "")


#loan_df_charged_off$emp_yrs <- loan_master_df[which(loan_master_df$loan_status == "Charged Off"),]$emp_length
#----------------------------
# replace " year", " years", " ", "+" with an exmplty space
#----------------------------

loan_df_charged_off$emp_yrs <-
  gsub(
    pattern = "+ years| years| year|s|+s| |/",
    replacement = "",
    x = loan_df_charged_off$emp_yrs,
    ignore.case = T
  )


# removing the percentage character from revol_util_per column
loan_df_charged_off$revol_util_per <-
  str_replace(string = loan_df_charged_off$revol_util_per,
              pattern = "%",
              replacement = "")

#----------------------------
# convert the interest rates to numeric
#----------------------------

loan_df_charged_off$int_per <- as.numeric(loan_df_charged_off$int_per)

# rounding up the interest rates to 2 decimals
loan_df_charged_off$int_per <- round(loan_df_charged_off$int_per, digits = 2)

#convert term_mnths, revol_util_per to numeric
loan_df_charged_off$term_mnths <- as.numeric(loan_df_charged_off$term_mnths)
loan_df_charged_off$revol_util_per <- as.numeric(loan_df_charged_off$revol_util_per)


#----------------------------
# rounding of decimals in columns: annual income, principal, late fee, total payments, recovery fees etc as these details are insignificant
#----------------------------

loan_df_charged_off[c(
  "annual_inc",
  "total_rec_late_fee",
  "total_pymnt",
  "installment",
  "total_rec_prncp",
  "total_rec_int",
  "recoveries",
  "collection_recovery_fee",
  "last_pymnt_amnt"   #we haven't used it in any analysis calculations
)] <- round(loan_df_charged_off[c(
  "annual_inc",
  "total_rec_late_fee",
  "total_pymnt",
  "installment",
  "total_rec_prncp",
  "total_rec_int",
  "recoveries",
  "collection_recovery_fee",
  "last_pymnt_amnt"
)], 0)


#----------------------------
# checking for duplicates
#----------------------------

sum(duplicated(loan_df_charged_off$id, loan_df_charged_off$member_id))
# Comments: All rows are unique in the data frame.
# no duplicates identified.

#----------------------------
# blanks in data_frame
#----------------------------

# blanks in last_payment_due, emp_title, interest rate, loan amount,
sum(loan_df_charged_off$last_pymnt_d == "")
sum(loan_df_charged_off$emp_title == "")
sum(loan_df_charged_off$loan_amnt == "")
sum(loan_df_charged_off$int_per == "")

# replacing the empty rows in  "emp_title" with "missing"
loan_df_charged_off[which(loan_df_charged_off$emp_title == ""),]$emp_title <- "missing"

#---
# cleaning employee job_title column
#---

# replacing cleaning job title of employee
loan_df_charged_off$emp_title <- gsub(pattern = ("us army legal services agency|us army corps of engineers|dept of the army|dod us army civilian|oh army national guard|army fleet support|army|department of the army|u.s. army|u.s. army \\(active\\)|united sates army|united states army|us army"),
     replacement = "us army",x = loan_df_charged_off$emp_title,
     ignore.case = T)


loan_df_charged_off$emp_title <- gsub(pattern = ("bank of a|bank of america|bank of america corp."),
                                      replacement = "bank of america",x = loan_df_charged_off$emp_title,
                                      ignore.case = T)



loan_df_charged_off$emp_title <- gsub(pattern = ("walmart|walmart corporate|walmart corporation|walmart stores inc|walmart stores\\, inc.|walmart\\.com"),
                                      replacement = "walmart",x = loan_df_charged_off$emp_title,
                                      ignore.case = T)

loan_df_charged_off$emp_title <- gsub(pattern = ("\\bat and t\\b|\\bat and t mobility\\b|\\bat&t mobility\\b|\\batt\\b]"),
                                      replacement = "at&t",x = loan_df_charged_off$emp_title,
                                      ignore.case = T)

loan_df_charged_off$emp_title <- gsub(pattern = ("\\bverizon\\b|\\bverizon comm\\b|\\bverizon communication\\b|\\bverizon communications\\b|\\bverizon telecom\\b|\\bverizon wireless\\b"),
                                      replacement = "verizon wireless",x = loan_df_charged_off$emp_title,
                                      ignore.case = T)

loan_df_charged_off$emp_title <- gsub(pattern = ("assured self employed storage|catherine reitmyer\\(self employed\\)|\\b\\(self employed\\) castleforte group\\b|\\bmjmi \\-\\ self employed\\b|\\bself\\b|\\bself emp\\b|\\bself employed\\b|\\bself employed \\(cate design\\)\\b|\\bself employed consultant\\b|\\bself-contract labor\\b|\\bself-employed\\b|\\bself-employed ebay  loan request is to start a business\\b|\\bself-employeed\\b"),
                                      replacement = "self employed",x = loan_df_charged_off$emp_title,ignore.case = T)

loan_df_charged_off$emp_title <- gsub(pattern = ("home depot|the home depot|the home depot rdc 5086"),
                                      replacement = "home depot",x = loan_df_charged_off$emp_title,ignore.case = T)

loan_df_charged_off$emp_title <- gsub(pattern = ("\\busps\\b|\\busps post office\\b|\\bu.s postal service\\b|\\bu.s. postal service\\b|\\bu.s.postal service\\b|\\bunited state postal service\\b|\\bunited states postal service\\b|\\bus postal inspection service\\b|\\bus postal service\\b|\\bu.s.p.s\\b|\\bu.s.p.s.\\b"),
                                      replacement = "us postal service",x = loan_df_charged_off$emp_title,ignore.case = T)

loan_df_charged_off$emp_title <- gsub(pattern = ("ibm|ibm corp|ibm corporation"),
                                      replacement = "ibm",x = loan_df_charged_off$emp_title,ignore.case = T)

loan_df_charged_off$emp_title <- gsub(pattern = ("kaiser permanente|\\bkaiser permanente \\& south pacific rehab\\b|kaiser permanete nw|kaiser permenete"),
                                      replacement = "kaiser permanente",x = loan_df_charged_off$emp_title,ignore.case = T)

loan_df_charged_off$emp_title <- gsub(pattern = ("wells fargo|wells fargo advisors\\, llc |wells fargo bank|wells fargo bank |wells fargo bank, n.a|wells fargo home mortgage|wells fargo insurance services usa, inc.|wellsfargo|wellsfargo home mortgage"),
                                      replacement = "wells fargo",x = loan_df_charged_off$emp_title,ignore.case = T)

loan_df_charged_off$emp_title <- gsub(pattern = ("jp morgan|jp morgan chase|jp morgan chase |jpm chase|jpmorgan|jpmorgan chase|jpmorgan chase bank"),
                                      replacement = "jp morgan",x = loan_df_charged_off$emp_title,ignore.case = T)

loan_df_charged_off$emp_title <- gsub(pattern = ("target|target corp|target corporation|target dc|target distribution"),
                                      replacement = "target",x = loan_df_charged_off$emp_title,ignore.case = T)

loan_df_charged_off$emp_title <- gsub(pattern = ("robins us air force base|\\bus air force det 3\\,544th intelligence group\\b|air force|u.s. air force|united states air force|united states air force dod|united states airforce|us air force"),
                                      replacement = "us air force",x = loan_df_charged_off$emp_title,ignore.case = T)


# companies with loan defaults. 
loanee_employers <- table(loan_df_charged_off[which(duplicated(loan_df_charged_off$emp_title)),]$emp_title)
loanee_employers <- as.data.frame(loanee_employers)

# eliminating missing and considering more than 3 repetitions for analysis
loanee_employers <- loanee_employers %>% filter(loanee_employers$Var1 != "missing" & loanee_employers$Freq > 3) %>% arrange(desc(Freq))
head(loanee_employers)
View(loanee_employers)



#----------------------------
# check rows with NAs
#----------------------------

which(is.na(loan_df_charged_off))

# checking for missing values in annual income,interest rate, loan amount, total payment, issue date, last payment due
sum(is.na(loan_df_charged_off$annual_inc))
sum(is.na(loan_df_charged_off$int_per))
sum(is.na(loan_df_charged_off$loan_amnt))
sum(is.na(loan_df_charged_off$installment))
sum(is.na(loan_df_charged_off$total_pymnt))
sum(is.na(loan_df_charged_off$issue_d))
sum(is.na(loan_df_charged_off$last_pymnt_d))


#----------------------------
# outliers in data_frame
#----------------------------

ggplot(loan_df_charged_off, aes(x = loan_status, y = loan_amnt)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Loan Amount", labels = comma, breaks = seq(0,40000,5000)) + 
  labs(title = expression(paste(bold("Outliers in the Loan Amount for Charged Off loans")))) +
  theme(text = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        # axis.line = element_line(color = "black"),
        panel.grid.major = element_line(colour = "grey75"),
        panel.background = element_rect(fill = "grey90"),
        panel.border = element_rect(linetype = "solid",fill =  NA)
        )
# Comments : Charged off customers has a few outliers

# outliers in interest rate
ggplot(loan_df_charged_off, aes(x = loan_status, y = int_per/100)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Interest Rate", labels = percent) + 
  labs(title = expression(paste(bold("Outliers in the interest Rate for Charged Off loans")))) +
  theme(text = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        # axis.line = element_line(color = "black"),
        panel.grid.major = element_line(colour = "grey75"),
        panel.background = element_rect(fill = "grey90"),
        panel.border = element_rect(linetype = "solid",fill =  NA)
        )

# examining outliers in annual_income
ggplot(loan_df_charged_off, 
       aes(x = loan_status, y = annual_inc/1000)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Annual Income (in `000s)", labels = comma, breaks = seq(0,1200,100)) + 
  labs(title = expression(paste(bold("Outliers in the Annual Income for Charged Off loans")))) +
  theme(text = element_text(size = 11),
        axis.title.x = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        # axis.line = element_line(color = "black"),
        panel.grid.major = element_line(colour = "grey75"),
        panel.background = element_rect(fill = "grey90"),
        panel.border = element_rect(linetype = "solid",fill =  NA)
        )
#Comments:  There are siginificant outliers in the annual_income which would skew the mean value.

# checking outliers in loan amounts, interest rates, annual incomes
quantile(x = loan_df_charged_off$loan_amnt, na.rm = T)
quantile(x = loan_df_charged_off$int_per, na.rm = T)
quantile(x = loan_df_charged_off$annual_inc, na.rm = T)
# 
# # mean and median of annual income of charged off customers
# summary(object = loan_df_charged_off$annual_inc)

# number of customers with annual income more than 99percentile of the median annual income
sum(loan_df_charged_off$annual_inc > quantile(x = loan_df_charged_off$annual_inc, probs = 0.99))

# percentage of charged-off customers, current and fully paid customers above 95 the percentile of data
percent(sum(loan_df_charged_off$annual_inc > quantile(x = loan_df_charged_off$annual_inc, probs = 0.95)) / length(loan_df_charged_off$id))
# Thus 5% of customers are above 95% percentile of annual income 

 
#---------------------------
# cleaning the date formats 
#---------------------------

# cleaning the date format of last_payment_d
loan_df_charged_off$last_payment_due <-
  as.Date(parse_date_time(x = loan_df_charged_off$last_pymnt_d, orders = "%b-%y"))

# cleaning the dates in Issue_d
loan_df_charged_off$issue_date  <-
  as.Date(parse_date_time(x = loan_df_charged_off$issue_d, orders = "%b-%y"))



############################################
# Objectives of the assignment
############################################

#  "Identification of RISKY applicants using EDA is the aim of this case study."
# - lending loans to "risky" applicants is the largest source of financial loss (called credit loss).
# - The credit loss is the amount of money lost by the lender when the borrower refuses to pay or runs away with the money owed.

# The aim is to identify patterns which indicate if a person is likely to default, 
# which may be used for taking actions such as: 
# denying the loan, reducing the amount of loan, lending (to risky applicants) at a higher interest rate, etc.

# who is likely to default?


# RISK ANALYTICS
# the company wants to understand the driving factors (or driver variables) behind loan default,
# i.e. the variables which are strong indicators of default.


#------------------------------
# credit loss calculation
#------------------------------

# Step1:
# credit_loss = prepaid_credit_amount + loan amount given to the borrower + interest lost during unpaid months - (principal repaid by the borrower + net recoveries)
# total payments = total received principal + Interest + late fee + recoveries
# recovery collection fee is not part of the total payment

# Step:2
# loan_df_charged_off$calc_total_pymnt <-
#   loan_df_charged_off[, "total_rec_prncp"] +
#   loan_df_charged_off[, "total_rec_int"] +
#   loan_df_charged_off[, "total_rec_late_fee"] +
#   loan_df_charged_off[, "recoveries"]

# Step: 3
# Calculating the paid_months and unpaid_months.

#interval(start = loan_df_charged_off$last_payment_due,end = loan_df_charged_off$issue_date) %/% months(1,abbreviate = F)
# method4
# Assumption: Customer hasn't paid after the last payment due. 

loan_df_charged_off$paid_mths <-
  (year(loan_df_charged_off$last_payment_due) - year(loan_df_charged_off$issue_date)) * 12 +
  (month(loan_df_charged_off$last_payment_due) - month(loan_df_charged_off$issue_date))

# remaing months unpaid
loan_df_charged_off$unpaid_mths <- loan_df_charged_off$term_mnths - loan_df_charged_off$paid_mths

# balance loan amount (the future value) = loan amount paid - principal received
loan_df_charged_off$future_value = (loan_df_charged_off$loan_amnt - loan_df_charged_off$total_rec_prncp)

# Step 4:
# Calculating EMI using "PMT" library
# credit loss = emi of the balance unpaid * number of unpaid months - recoveries made of default.
loan_df_charged_off$credit_loss <-
  round(
    pmt(
      amt = loan_df_charged_off$future_value,
      maturity = loan_df_charged_off$unpaid_mths,
      rate = loan_df_charged_off$int_per / 1200
    ),
    0
  ) * loan_df_charged_off$unpaid_mths -  loan_df_charged_off$recoveries

# Step 5:
# total credit loss due to charge-off
paste("$", formatC(
  sum(loan_df_charged_off$credit_loss, na.rm = T),
  big.mark = "," ,
  format = "f"
))

# Replacing the NA with mean mean credit_loss
loan_df_charged_off[which(is.na(loan_df_charged_off$credit_loss)),]$credit_loss <- mean(loan_df_charged_off$credit_loss,na.rm = T)

# Thus the total credit loss = "- $ 52,518,650.0000"


#------------Calculation of credit_loss ends here-------------------------------------------

loan_bkp <- loan_df_charged_off

#write.csv(x = loan_df_charged_off,file = "loan_df_charged_off.csv")



#-------------------------------------------------------
# UNIVARIATE ANLYSIS
#-------------------------------------------------------



# histogram for annual income and charged of loans
ggplot(data = subset(loan_df_charged_off,
                     loan_df_charged_off$annual_inc < quantile(x = loan_df_charged_off$annual_inc, probs = 0.99)),
       aes(x = annual_inc/1000,fill = grade)) +
  geom_histogram(binwidth = 10) +
  scale_x_continuous(name = "Annual Income (in `000s)", labels = comma, breaks = seq(0,200,25)) +
  scale_y_continuous(name = "Frequency", labels = comma, breaks = seq(0,1000,250)) +
  # scale_fill_brewer(palette = "Set2") +
  labs(title = expression(paste(bold("Annual income distribution for each grade")))) +
  theme(text = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        # axis.line = element_line(color = "black"),
        panel.grid.major = element_line(colour = "grey75"),
        panel.background = element_rect(fill = "grey90"),
        panel.border = element_rect(linetype = "solid",fill =  NA)
        )

# Comments: Data suggests that loan applicants with grades between B,C,D,E are most likely to default and cause heavy credit loss.
# and income between 30K to 60K are most likely to default.


# Percentage of loan grades
# loan grade

loan_df_charged_off %>% 
  group_by(grade) %>%
  summarise(num_loans = n()) %>%
  mutate(loanpercent = num_loans/sum(num_loans)) %>%
  ggplot(aes(x = grade, y = loanpercent)) + 
    geom_bar(stat = "identity", fill = "cornflowerblue") +
    geom_text(aes(label = sprintf("%.01f %%",100*loanpercent)),
              fontface = "italic",size = 3.3,
              position = position_stack(vjust = 0.5)) +
  scale_y_continuous(name = "Percent", labels = percent, breaks = seq(0,.3,.05)) +
  labs(title = expression(paste(bold("Distribution of Customer Grades"))),
             x = expression(paste(bold("Grade")))) +
    theme(text = element_text(size = 11),
          legend.position = "none",
          axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          # axis.text.y = element_blank(),
          # axis.title.y = element_blank(),
          panel.grid.major = element_line(colour = "grey75"),
          panel.background = element_rect(fill = "grey90"),
          panel.border = element_rect(linetype = "solid",fill =  NA))

# most of the charged_off loans are B,C,D,and E grade loans



# Observations:
# customers with B,C,D grade loans and annual income of these loans in $30,000-$60,000 are more likely to default
# The income increase default count decreasing

# impact of variables on classification of credit loss
# using histogram to calculate the credit loss

#----
# impact of grades on credit loss
#----

ggplot(data = subset(loan_df_charged_off,
                     loan_df_charged_off$credit_loss < quantile(x = loan_df_charged_off$credit_loss, 
                                                                probs = 0.95,na.rm = T)),
        aes(x = (credit_loss),fill = home_ownership)) +
  geom_histogram(binwidth = 5000) +
  scale_x_continuous(name = "Credit Loss", labels = comma, breaks = seq(0,30000,5000)) +
  scale_y_continuous(name = "Frequency", labels = comma, breaks = seq(0,2000,500)) +
  scale_fill_manual("legend", values = c("mortgage" = "peru",
                                         "other" = "black", 
                                         "own" = "bisque3",
                                         "rent" = "coral4")) +
  labs(title = expression(paste(bold("Impact of home ownership on credit loss")))) +
  theme(text = element_text(size = 11),
        legend.direction = "horizontal",
        legend.position = "bottom",
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        panel.grid.major = element_line(colour = "grey75"),
        panel.background = element_rect(fill = "grey90"),
        panel.border = element_rect(linetype = "solid",fill =  NA))


# rent and mortaged customers show significant impact on charged-off loans .
# customers with credit loss in the range of 5,000-10,000 show significant number of charged off loans


#-----
# impact of verification experience on credit loss and defaults
#-----

ggplot(data = subset(loan_df_charged_off,
                     (loan_df_charged_off$credit_loss < quantile(x = loan_df_charged_off$credit_loss, 
                                                                 probs = 0.95,na.rm = T) & 
                      loan_df_charged_off$emp_yrs != "na")),
      aes(x = credit_loss,fill = verification_status)) +
  geom_histogram(binwidth = 5000) +
  scale_x_continuous(name = "Credit Loss", labels = comma, breaks = seq(0,30000,5000)) +
  scale_y_continuous(name = "Frequency", labels = comma, breaks = seq(0,2000,500)) +
  scale_fill_manual("legend", values = c("not verified" = "firebrick2",
                                         "source verified" = "forestgreen", 
                                         "verified" = "orange")) +
  labs(title = expression(paste(bold("Impact of verification status on credit loss")))) +
  theme(text = element_text(size = 11),
        legend.direction = "horizontal",
        legend.position = "bottom",
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        panel.grid.major = element_line(colour = "grey75"),
        panel.background = element_rect(fill = "grey90"),
        panel.border = element_rect(linetype = "solid",fill =  NA))


# credit loss between 1 to 15000 is significant due to not verified and verified customers.

#-----
# impact of ch on credit loss and defaults
#----

ggplot(data = subset(loan_df_charged_off,
                     loan_df_charged_off$credit_loss < quantile(x = loan_df_charged_off$credit_loss, 
                                                                 probs = 0.95,na.rm = T)),
       aes(x = credit_loss,fill = purpose)) +
  geom_histogram(binwidth = 5000) +
  scale_x_continuous(name = "Credit Loss", labels = comma, breaks = seq(0,30000,5000)) +
  scale_y_continuous(name = "Frequency", labels = comma, breaks = seq(0,2000,500)) +
  scale_fill_brewer(palette = "Paired") +
  labs(title = expression(paste(bold("Impact of 'purpose' on credit loss")))) +
  theme(text = element_text(size = 11),
        legend.direction = "horizontal",
        legend.position = "bottom",
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        panel.grid.major = element_line(colour = "grey75"),
        panel.background = element_rect(fill = "grey90"),
        panel.border = element_rect(linetype = "solid",fill =  NA))
  

# Comments: majority of the credit loss is as a result of "debt consolidation" and "credit card" defaults. 
# credit loss between $1 to $15000 is significant due to not verified and only source verified customers.


# impact of inquires in loast 6 months

ggplot(loan_df_charged_off,aes(inq_last_6mths, credit_loss,col = "bisque2")) + geom_col() +
  labs(title = expression(paste(bold("Probability of default with inquiries"))),
       x = expression(paste(bold("inquiries last 6 months")))) +
  theme(text = element_text(size = 11),
        legend.position = "none",
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        # axis.text.y = element_blank(),
        # axis.title.y = element_blank(),
        panel.grid.major = element_line(colour = "grey75"),
        panel.background = element_rect(fill = "grey90"),
        panel.border = element_rect(linetype = "solid",fill =  NA))



#----
# univariate analysis of job title
#----


ggplot(data = subset(loanee_employers, loanee_employers$Freq > 4),
       aes(x = fct_infreq(Var1,ordered = T),
           y = Freq,
           fill = factor(Freq))) +

  geom_col(aes(reorder(Var1, -Freq))) +
  geom_text(aes(label = Freq),position = position_stack(vjust = 0.5),size = 3) +
  # scale_fill_brewer(palette = "Blues", direction = -1) +
  labs(title = expression(paste(bold("Employer-wise count of defaults"))),
           x = expression(paste(bold("Employee's Company"))),
           y = expression(paste(bold("No of Defaults")))) +
  theme(text = element_text(size = 11),
        legend.position = "none",
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_line(colour = "grey75"),
        panel.background = element_rect(fill = "grey90"),
        panel.border = element_rect(linetype = "solid",fill =  NA))
# Data shows that majority of the defaulters are from US Army serving as ex-servicemen, bank of america, walmart, ups, at&t and verizon

# 
# loan_df_charged_off %>% filter(emp_title %in% loanee_employers$Var1) %>% top_n(n=20) %>%
# ggplot(aes(fct_infreq(emp_title, ordered = T))) + geom_bar() +
#   theme(axis.text.x = element_text(angle = 90))
#   


# Data shows that are leading defaulters from:
# 1           us army   49
# 2 us postal service   37
# 3     self employed   35
# 4   bank of america   26
# 5           walmart   24
# 6  verizon wireless   21

# interest rates
ggplot(loan_df_charged_off, aes(int_per,fill = grade)) +
  geom_histogram(binwidth = 2, aes(y = ..count..)) +
  stat_function(fun = dnorm,
                colour = "red",
                args = list(
                  mean = mean(loan_df_charged_off$int_per, na.rm = TRUE),
                  sd = sd(loan_df_charged_off$int_per, na.rm = TRUE)
                )) +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "bottom",
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


#######################################
# Segmented univariate analysis
#######################################

summary(loan_df_charged_off$credit_loss)

#---
# UNIVARIATE ANALYSIS OF Average credit Loss and Years of experience in conjunction with Verification status.
#----
loan_df_charged_off %>%
  group_by(emp_yrs,verification_status) %>%
  summarise(cr_loss = median(credit_loss,na.rm = T)) %>%
  arrange(desc(cr_loss)) %>%
  ggplot(aes(x = emp_yrs, 
             y = cr_loss,
             fill = verification_status)) +
    geom_col(aes(reorder(emp_yrs, -cr_loss))) +
    scale_y_continuous(labels = comma, breaks = seq(0,30000,5000)) +
    scale_fill_manual("legend", values = c("not verified" = "firebrick2",
                                         "source verified" = "forestgreen", 
                                         "verified" = "orange")) +   
    labs(title = expression(paste(bold("Mean credit loss/employment in years and verfi status"))),
             x = expression(paste(bold("Employment (years)"))),
             y = expression(paste(bold("Median credit loss")))) +
    theme(text = element_text(size = 11),
          legend.direction = "horizontal",
          legend.position = "bottom", 
          axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          panel.grid.major = element_line(colour = "grey75"),
          panel.background = element_rect(fill = "grey90"),
          panel.border = element_rect(linetype = "solid",fill =  NA))


#Comments: average credit loss is highest due to 10+, 9 and 7 years expereience in verified category
# Trend shows that verified customers customers lead the chart


# Segmented Univariate analysis of credit_loss and home ownership and purpose for which loan is sought
#aes(reorder(home_ownership, -table(home-ownership)[home_ownership])

loan_df_charged_off %>%
  group_by(home_ownership,purpose) %>%
  summarise(cr_loss = mean(credit_loss,na.rm = T)) %>%
  arrange(desc(cr_loss)) %>%
  ggplot(aes(x = home_ownership, 
             y = cr_loss,
             fill = home_ownership)) +
    geom_col(aes(reorder(home_ownership, -cr_loss))) +
    scale_y_continuous(labels = comma,breaks = seq(0,150000,25000)) +
    scale_fill_manual("legend", values = c("mortgage" = "peru",
                                           "other" = "black", 
                                           "own" = "bisque3",
                                           "rent" = "coral4")) +
    labs(title = expression(paste(bold("Average credit loss by home ownership"))),
             x = expression(paste(bold("Home Ownership"))),
             y = expression(paste(bold("Average credit loss")))) +
    theme(text = element_text(size = 11),
          legend.direction = "horizontal",
          legend.position = "bottom",          
          axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          panel.grid.major = element_line(colour = "grey75"),
          panel.background = element_rect(fill = "grey90"),
          panel.border = element_rect(linetype = "solid",fill =  NA))
    
  

# About 50.45% percent of people charged-off of lived in "rental accomodation"
# About 41.35% percent of people took loan for "mortaging"


#----
#Average credit loss by Grade
#---


loan_df_charged_off %>% 
        group_by(grade,purpose) %>% 
        summarise(avg_credit_loss = mean(credit_loss,na.rm = T)) %>% 
        arrange(desc(avg_credit_loss)) %>%
    ggplot(aes(x = grade,
               y = avg_credit_loss,
               fill = grade)) + 
      geom_col(alpha = 1) +
      scale_y_continuous(labels = comma,breaks = seq(0,250000,50000)) +
      labs(title = expression(paste(bold("Average credit loss by Grade"))),
             x = expression(paste(bold("Grade"))),
             y = expression(paste(bold("Average credit loss")))) +
      theme(text = element_text(size = 11),
          legend.position = "none",          
          axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          panel.grid.major = element_line(colour = "grey75"),
          panel.background = element_rect(fill = "grey90"),
          panel.border = element_rect(linetype = "solid",fill =  NA))

# E,F,G grade loans have highest average credit loss, though B,C,D loans are largest by count.  


#----
# Average credit loss by Purpose
#---

loan_df_charged_off %>% 
        group_by(grade,purpose) %>% 
        summarise(avg_credit_loss = mean(credit_loss,na.rm = T)) %>% 
        arrange(desc(avg_credit_loss)) %>%
    ggplot(aes(x = purpose,
               y = avg_credit_loss,
               fill = purpose)) + 
      geom_col(aes(reorder(purpose,-avg_credit_loss))) +
      scale_y_continuous(labels = comma,breaks = seq(0,100000,25000)) +
      labs(title = expression(paste(bold("Average credit loss by Purpose"))),
             x = expression(paste(bold("Purpose"))),
             y = expression(paste(bold("Average credit loss")))) +
      theme(text = element_text(size = 11),
          legend.position = "none",          
          axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 90),
          panel.grid.major = element_line(colour = "grey75"),
          panel.background = element_rect(fill = "grey90"),
          panel.border = element_rect(linetype = "solid",fill =  NA))

# home_improvement, small_business and debt_consolidation have highest average credit loss.


## segmenting Interest rates int_per column into a new column intRate_type (Normal, High, VeryHigh)
# Save into separate Interest Rate Types; 
loan_df_charged_off$Int_Rate_Type = ifelse(loan_df_charged_off$int_per < 10, "Normal", 
                                           ifelse(loan_df_charged_off$int_per < 15,"High", "Very High"))

ggplot(loan_df_charged_off, aes(purpose, fill = Int_Rate_Type)) + 
  geom_bar(stat = 'count',position = "dodge") + 
  ggtitle("Purpose vs Interest Rate") +
  theme(axis.text.x = element_text(angle = 90))



#Comments: Average credit loss is most contributed by home improvement and small business, debt consolidation, major purchase


ggplot(loan_df_charged_off,
       aes(x = verification_status,
           y = credit_loss/1000,
           fill = (annual_inc/1000))) + 
  geom_col(alpha = 0.4) +
  scale_y_continuous(labels = comma,breaks = seq(0,30000,5000)) +
  labs(title = expression(paste(bold("Credit Loss Vs Annual Income by Verification Status"))),
       x = expression(paste(bold("Verifcation Status"))),
       y = expression(paste(bold("Credit Loss (in`000s)")))) +
  guides(fill = guide_legend(title = "Annual Income (in`000s)")) +
  theme(text = element_text(size = 11),
        legend.direction = "horizontal",
        legend.position = "bottom",         
        axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold"),
        panel.grid.major = element_line(colour = "grey75"),
        panel.background = element_rect(fill = "grey90"),
        panel.border = element_rect(linetype = "solid",fill =  NA))

# credit loss of due to verifiied customers is considerably large






##############################################
# Bi-variate analysis categorical variables 
##############################################
# of Purpose loan taken and verification status

purpose_verification <-  as.data.frame(table(loan_df_charged_off$purpose,loan_df_charged_off$emp))

purpose_verification %>% filter(purpose_verification$Var2 != "missing" & purpose_verification$Freq > 4) %>%
ggplot(aes(x = Var1, y = Freq,fill = Var2)) +
  geom_col(aes(reorder(Var1,-Freq)),position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = expression(paste(bold("Bivariate analysis of Purpose of Vs job_title"))),
         x = expression(paste(bold("Purpose"))),
         y = expression(paste(bold("Frequency")))) +
  guides(fill = guide_legend(title = "Verification Status")) +
  
  theme(text = element_text(size = 11),
      legend.direction = "vertical",
      legend.position = "right",         
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 90),
      panel.grid.major = element_line(colour = "grey75"),
      panel.background = element_rect(fill = "grey90"),
      panel.border = element_rect(linetype = "solid",fill =  NA))

# Comments: majority of the debt consolidation loans are from US arym, Us Postal services


#########################################################
# Bivariate Analysis of continuous variables.
#########################################################


# impact of bivariate analysis of  annual income and credit loss 

ggplot(subset(loan_df_charged_off,
               (loan_df_charged_off$annual_inc < quantile(x = loan_df_charged_off$annual_inc, probs = 0.95) & 
                !is.na(loan_df_charged_off$credit_loss))),
     aes(x = annual_inc, 
         y = credit_loss, 
         col = verification_status)) +
  geom_point(position = "dodge",alpha = 0.4) + 
  geom_smooth() +
  scale_x_continuous(labels = comma,breaks = seq(0,150000,25000)) +
  scale_y_continuous(labels = comma,breaks = seq(0,50000,10000)) +
  scale_fill_manual("legend", values = c("not verified" = "firebrick2",
                                         "source verified" = "forestgreen", 
                                         "verified" = "orange")) +  
  labs(title = expression(paste(bold("Credit Loss Vs Annual Income"))),
         x = expression(paste(bold("Annual Income"))),
         y = expression(paste(bold("Credit Loss")))) +
  guides(fill = guide_legend(title = "Verification Status")) +
  theme(text = element_text(size = 11),
      legend.direction = "horizontal",
      legend.position = "bottom",         
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0),
      panel.grid.major = element_line(colour = "grey75"),
      panel.background = element_rect(fill = "grey90"),
      panel.border = element_rect(linetype = "solid",fill =  NA))


# bivariate analysis of loan amount
# loan amount and credit loss

ggplot(loan_df_charged_off,aes(x = loan_amnt,y = credit_loss)) + geom_point() +
  labs(title = " loan amount and credit loss")
s


# calculating the correlation between the annual income and credit loss
cor(loan_df_charged_off$annual_inc,loan_df_charged_off$credit_loss,use = "pairwise.complete.obs")

#  Thus there exists good correlation between annual income an credit loss

#cor(c(loan_df_charged_off$int_per,loan_df_charged_off$open_acc,loan_df_charged_off$revol_bal,loan_df_charged_off$revol_util_per),loan_df_charged_off$credit_loss,use = "na.or.complete")

cor(loan_df_charged_off$int_per,loan_df_charged_off$credit_loss,use = "na.or.complete")
cor(loan_df_charged_off$open_acc,loan_df_charged_off$credit_loss,use = "na.or.complete")
cor(loan_df_charged_off$revol_bal,loan_df_charged_off$credit_loss,use = "na.or.complete")
cor(loan_df_charged_off$revol_util_per,loan_df_charged_off$credit_loss,use = "na.or.complete")
cor(loan_df_charged_off$term_mnths,loan_df_charged_off$credit_loss,use = "na.or.complete")
cor(loan_df_charged_off$total_pymnt,loan_df_charged_off$credit_loss,use = "na.or.complete")
cor(loan_df_charged_off$unpaid_mths,loan_df_charged_off$credit_loss,use = "na.or.complete")
cor(loan_df_charged_off$loan_amnt,loan_df_charged_off$credit_loss,use = "na.or.complete")
cor(loan_df_charged_off$funded_amnt,loan_df_charged_off$credit_loss,use = "na.or.complete")

# create a heat-map matrix of bivariate analysis of variables
my_data_cols <- which(colnames(loan_df_charged_off) %in% c("loan_amnt","int_per","term_mnths",
                                                           "open_acc","revol_bal","revol_util_per","annual_inc",
                                                           "unpaid_mths","credit_loss"))

my_data <- loan_df_charged_off[,my_data_cols]

my_data_cor <- round(cor(my_data,use = "na.or.complete"),2)



ggplot(melt(my_data_cor),aes(x = Var1,y = Var2)) + 
  geom_tile(aes(fill = value), colour = "white") +
  geom_text(aes(label = value),size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", na.value = "white", 
                       midpoint = 0, limit = c(0,1),name = "Credit_loss \ncorrelation") +
  labs(title = expression(paste(bold("Correlation of various parameters on credit loss")))) +
  theme(text = element_text(size = 11),
      legend.direction = "vertical",
      legend.position = "none",     
      legend.justification = c(1, 0),
      axis.title = element_blank(),
      axis.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, vjust = 1, size = 9, hjust = 1),
      axis.text.y = element_text(vjust = 1, size = 9, hjust = 1),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(colour = "grey75"),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(linetype = "solid",fill =  NA))








#-------------- code to convert emp_years to numeric-------------
# # replace <1 experience as 0 years of experience
# loan_df_charged_off$emp_yrs <-
#   gsub(
#     pattern = "<1",
#     replacement = "0",
#     x = loan_df_charged_off$emp_yrs,
#     ignore.case = T
#   )
# unique(loan_df_charged_off$emp_yrs)
# 
# typeof(loan_df_charged_off$emp_yrs)
# loan_df_charged_off$emp_yrs <- round(as.numeric(loan_df_charged_off$emp_yrs),0)
# 
# #percent of NA is the employment years. 
# percent(sum(is.na(loan_df_charged_off$emp_yrs)/length(loan_df_charged_off$emp_yrs)))
# # replacing the NAs with mean value of the experience to reduce the impact of working experience on the data
# 
# # assigning the mean value to the missing NAs
# loan_df_charged_off[which(is.na(loan_df_charged_off$emp_yrs)),]$emp_yrs <- mean(loan_df_charged_off$emp_yrs,na.rm = T)
# 
#--------------


#  Another significant 
#  This is further established that considerable losses of the losses are due to not-verified customers.
# 
# 
# #univariate analysis for charged_off loans
# ggplot(loan_df_charged_off, aes(iss_yr, fill = grade)) + geom_bar() +
#   geom_text(
#     stat = "count",
#     size = 3,
#     position = position_stack(vjust = 0.5),
#     aes(label = (..count..))
#   )
# # several of these charged_off loans were 2010 and 2011.
# 
# 
# 
# str(loan_df_charged_off)
# 
# # Annualized mean interest rates
# loan_df_charged_off %>% group_by(year = iss_yr, grade) %>% summarise(avg_int_rate = mean(int_per)) %>%
#   ggplot(aes(year, avg_int_rate, fill = grade)) + geom_col() +
#   geom_text(aes(label = round(avg_int_rate, 2)),
#             size = 3,
#             position = position_stack(vjust = 0.5))
# 

