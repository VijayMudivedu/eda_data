
#setwd("D:/MyDocs/eda/eda_data/_Upgrad")
#setwd("~/Downloads/eda_data/eda_casestudy")
setwd("~/OneDrive/OneDrive - Atimi Software Inc/Upgrad/_Upgrad/EDA/eda_casestudy")


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


# import the loan csv file for analysis
loan_df <- read.csv("loan.csv", stringsAsFactors = F)
# 
# # back up the loan data_frame
# loan_bkp <- loan_df

# review the loan data frame
str(loan_df)

# subsetting charged_off loans for Risk Analytics
table(loan_df$loan_status)
loan_df_charged_off <- subset(x = loan_df, loan_status == "Charged Off")
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
  "last_credit_pull_d"
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
      "last_credit_pull_d"
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


#loan_df_charged_off$emp_yrs <- loan_df[which(loan_df$loan_status == "Charged Off"),]$emp_length
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

# replace 


# empty rows in  "emp_title" being replaced with "missing"
loan_df_charged_off[which(loan_df_charged_off$emp_title == ""),]$emp_title <- "missing"

#---
# cleaning employee job_title data
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

# eliminating missing and considering more than two repetitions for analysis
loanee_employers <- loanee_employers %>% filter(loanee_employers$Var1 != "missing" & loanee_employers$Freq > 2) %>% arrange(desc(Freq))
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

ggplot(loan_df_charged_off, aes(x = loan_status, y = loan_amnt)) + geom_boxplot()
# There are outliers in charged_off and fully_paid customers

# outliers in interest rate
ggplot(loan_df_charged_off, aes(x = loan_status, y = int_per)) + geom_boxplot()

# examining outliers in annual_income
ggplot(loan_df_charged_off, aes(x = loan_status, y = annual_inc)) + geom_boxplot()

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

# years of last payment due and issue date, these can be used in visual analytics to see the distribution of loans by year and defaults for charged-off
# loan_df_charged_off$lpd_yr <- format(loan_df_charged_off$last_payment_due, "%Y")
# loan_df_charged_off$iss_yr <- format(loan_df_charged_off$issue_date, "%Y")

#loan_df_charged_off[,-which(colnames(loan_df_charged_off) %in% c("lpd_yr","iss_yr"))]



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

# credit_loss = prepaid_credit_amount + loan amount given to the borrower + interest lost during unpaid months - (principal repaid by the borrower + net recoveries)
# total payments = total received principal + Interest + late fee + recoveries
# recovery collection fee is not part of the total payment

# loan_df_charged_off$calc_total_pymnt <-
#   loan_df_charged_off[, "total_rec_prncp"] +
#   loan_df_charged_off[, "total_rec_int"] +
#   loan_df_charged_off[, "total_rec_late_fee"] +
#   loan_df_charged_off[, "recoveries"]


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


# total credit loss due to charge-off
paste("$", formatC(
  sum(loan_df_charged_off$credit_loss, na.rm = T),
  big.mark = "," ,
  format = "f"
))

# Thus the total credit loss = "- $ 52,518,650.0000"


#------------Calculation of credit_loss ends here-------------------------------------------


write.csv(x = loan_df_charged_off,file = "loan_df_charged_off.csv")


#-------------------------------------------------------
# UNIVARIATE ANLYSIS
#-------------------------------------------------------

#---
# histogram for Annual Income
#---
ggplot(
  subset(
    loan_df_charged_off,
    loan_df_charged_off$annual_inc < quantile(x = loan_df_charged_off$annual_inc, probs = 0.99)
  ),
  aes(annual_inc,fill = grade)
) +
  geom_histogram(binwidth = 10000) +
  ggtitle("Annual income distribution on credit loss") +
  theme(legend.position = "bottom") 

# Comments: Data suggests that loan applicants with grades between B,C,D,E are most likely to default and cause heavy credit loss.
# and income between 30K to 60K are most likely to default.


# loan grade
loan_df_charged_off %>%
  ggplot(aes(fct_infreq(grade, ordered = T), fill = "red")) + geom_bar(stat = "count", aes(identity = "count")) +
  geom_text(
    stat = "count",
    aes(label = paste(round((..count..)*100/sum(..count..),1),"%")),
    size = 3,
    position = position_stack(vjust = 0.5)) +
  xlab(label = "grade") + ggtitle(label = "Count of Loan Grades") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

# most of the charged_off loans are B,C and D grade loans



# Observations:
# customers with B,C,D grade loans and annual income of these loans in $30,000-$60,000 are more likely to default
# The income increase default count decreasing

# impact of variables on classification of credit loss
# using histogram to calculate the credit loss

#----
# impact of grades on credit loss
#----

ggplot(
  subset(
    loan_df_charged_off,loan_df_charged_off$credit_loss < quantile(x = loan_df_charged_off$credit_loss, probs = 0.95,na.rm = T)
  ),
  aes(credit_loss,fill = home_ownership)
) +
  geom_histogram(binwidth = 5000) +
  theme(legend.direction = "horizontal",
    legend.position = "bottom",
    panel.background = element_blank(),
        axis.title.y = element_blank(),
    axis.ticks.y = element_blank()) +
  ggtitle("Impact of grades on credit loss") +
  scale_fill_manual("legend", values = c("mortgage" = "cornflowerblue","other" = "black", "own" = "orange","rent" = "azure3"))

# rent and mortaged customers show significant impact on charged-off loans .
# customers with credit loss in the range of 5,000-10,000 show significant number of charged off loans


#-----
# impact of employment experience on credit loss and defaults
#-----

ggplot(
  subset(loan_df_charged_off,
    (loan_df_charged_off$credit_loss < quantile(x = loan_df_charged_off$credit_loss, probs = 0.95,na.rm = T) & 
       (loan_df_charged_off$emp_yrs != "na")
    )),
  aes(credit_loss,fill = verification_status)
) +
  geom_histogram(binwidth = 5000) +
  ggtitle("Impact of verification status on credit loss ") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_manual("legend", values = c("not verified" = "cornflowerblue","source verified" = "forestgreen", "verified" = "orange"))


# credit loss between 1 to 15000 is significant due to not verified and verified customers.
# impact of ch on credit loss and defaults

ggplot(
  subset(loan_df_charged_off,
    (loan_df_charged_off$credit_loss < quantile(x = loan_df_charged_off$credit_loss, probs = 0.95,na.rm = T) 
    )),
  aes(credit_loss,fill = purpose)
) +
  geom_histogram(binwidth = 5000) +
  ggtitle("Frequency histogram of mean credit loss by 'purpose' of loan") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        panel.background = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_brewer(palette = "Paired")

# Comments: majority of the credit loss is as a result of "debt consolidation" and "credit card" defaults. 
# credit loss between $1 to $15000 is significant due to not verified and only source verified customers.



#----
# univariate analysis of job title
#---

ggplot(subset(loan_employers, loan_employers$Freq > 4) ,aes(x = fct_infreq(Var1,ordered = T),y = Freq,fill = "red")) +
  geom_col(aes(reorder(Var1, -Freq))) +
  geom_text(aes(label = Freq),position = position_stack(vjust = 1.2),size = 3) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none",
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  xlab("Company Working for") + ylab("No of Defaults")



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

median(loan_df_charged_off$credit_loss,na.rm = T)
mean(loan_df_charged_off$credit_loss,na.rm = T)

#---
# UNIVARIATE ANALYSIS OF Average credit Loss and Years of experience in conjunction with Verification status.
#----
loan_df_charged_off %>%
  group_by(emp_yrs,verification_status) %>%
  summarise(cr_loss = median(credit_loss,na.rm = T)) %>%
  arrange(desc(cr_loss)) %>%
  ggplot(aes(emp_yrs, cr_loss,fill = verification_status)) + geom_col(aes(reorder(emp_yrs, -cr_loss))) +
  xlab(" Employment (years)") + ylab("Average credit loss") + 
  ggtitle("Average credit loss/Employment in years and verif. status") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom",
        title = element_text(size = 9))


#Comments: average credit loss is most due to 10+, 7 and 9 years expereience. 
# Trend shows that verified customers customers lead the chart


# Segmented Univariate analysis of credit_loss and home ownership and purpose for which loan is sought
#aes(reorder(home_ownership, -table(home-ownership)[home_ownership])

loan_df_charged_off %>%
  group_by(home_ownership,purpose) %>%
  summarise(cr_loss = mean(credit_loss,na.rm = T)) %>%
  arrange(desc(cr_loss)) %>%
  ggplot(aes(home_ownership, cr_loss,fill = "red")) + geom_col(aes(reorder(home_ownership, -cr_loss))) + 
  xlab(" Employment (years)") + ylab("Average credit loss") + 
  ggtitle("Average credit loss and Purpose") +
  theme(legend.direction = "horizontal",
        legend.position = 
        title = element_text(size = 9)) +
  scale_color_tableau("tableau10") +
  theme_classic()
  

# About 50.45% percent of people charged of lived in "rental accomodation"
# About 41.35% percent of people took loan for "mortaging"


# E,F,G grade loans have highest average credit loss, though B,C,D loans are largest by count.
loan_df_charged_off %>% 
  group_by(grade,purpose) %>% 
  summarise(avg_credit_loss = mean(credit_loss,na.rm = T)) %>% 
  arrange(desc(avg_credit_loss)) %>%
  ggplot(aes(grade,avg_credit_loss)) + 
  geom_col(alpha = 1)


# E,F,G grade loans have highest average credit loss, though BCD loans are largest by count.
loan_df_charged_off %>% 
  group_by(grade,purpose) %>% 
  summarise(avg_credit_loss = mean(credit_loss,na.rm = T)) %>% 
  arrange(desc(avg_credit_loss)) %>%
  ggplot(aes(purpose,avg_credit_loss)) + 
  geom_col(aes(reorder(purpose,-avg_credit_loss))) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("Purpose") + ylab("average credit loss")

#Comments: Average credit loss is most contributed by home improvement and small business, debt consolidation, major purchase

##############################################
# Bi-variate analysis categorical variables 
##############################################
# of Purpose loan taken and verification status

purpose_verification <-  as.data.frame(table(loan_df_charged_off$purpose,loan_df_charged_off$verification_status))

ggplot(purpose_verification,aes(x = Var1, y = Freq,fill = Var2)) + geom_col(aes(reorder(Var1,-Freq))) +
  theme(axis.text.x = element_text(angle = 90))  + 
  ggtitle("Bivariate analysis of Purpose of Vs Veri.Status") +
  guides(fill = guide_legend(title = "Verification Status")) +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        title = element_text(size = 8))


# ggplot(loan_df_charged_off, aes(reorder(verification_status, -table(verification_status)[verification_status]),fill = purpose)) +
#   geom_bar() +
#   geom_text(stat = "count",aes(label = (..count..)),size = 3,position = position_stack(vjust = 0.5)) +
#   xlab("Verification Status") +
#   theme(axis.title.y = element_blank()) +
 




#########################################################
# Bivariate Analysis of continuous variables.
#########################################################


# impact of bivariate analysis of  annual income and credit loss 


ggplot(
   subset(
     loan_df_charged_off,
     (loan_df_charged_off$annual_inc < quantile(x = loan_df_charged_off$annual_inc,probs = 0.95) & !is.na(loan_df_charged_off$credit_loss))),
     aes(x = annual_inc, y = credit_loss, col = verification_status)) + geom_point(position = "dodge",alpha = 0.4) + geom_smooth() +
  ggtitle("Credit Loss Vs Annual Income") +
  theme(legend.direction = "horizontal",
        legend.position = "bottom")


# bivariate analysis of loan amount
# loan amount and credit loss

ggplot(loan_df_charged_off,aes(x = loan_amnt,y = credit_loss)) + geom_point()



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

my_data_cor[lower.tri(my_data_cor)] <- NA


ggplot(melt(my_data_cor),aes(x = Var1,y = Var2)) + 
  geom_tile(aes(fill = value), colour = "white") +
  geom_text(aes(label = value),size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", na.value = "white", 
                       midpoint = 0, limit = c(0,1),name = "Credit_loss \ncorrelation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 9, hjust = 1), 
       axis.text.y = element_text(vjust = 1, size = 9, hjust = 1),
       axis.title.x = element_blank(),
       axis.title.y = element_blank(),
       axis.ticks = element_blank(),
       legend.justification = c(1, 0),
       legend.position = c(0.99, 0.1),
       legend.direction = "horizontal") +
  ggtitle("Correlation of various parameters on credit loss")
  

ggplot(loan_df_charged_off,aes(x = verification_status,y = credit_loss,fill = (annual_inc))) + geom_col(alpha = 0.4)








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

