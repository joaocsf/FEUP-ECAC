library(tidyverse)
library(psych)
clients <- read.csv('./csv_db/clients.csv',sep = ';', header=TRUE)
trans <- read.csv('./csv_db/trans_test.csv',sep = ',', header=TRUE)
account <- read.csv('./csv_db/account.csv',sep = ';', header=TRUE)
district <- read.csv('./csv_db/district.csv',sep = ';', header=TRUE)
disp <- read.csv('./csv_db/disp.csv',sep = ';', header=TRUE)
loan <- read.csv('./csv_db/loan_test.csv',sep = ';', header=TRUE)
card <- read.csv('./csv_db/card_test.csv', sep =';', header=TRUE)

clients <- read.csv('./csv_db/clients.csv',sep = ';', header=TRUE)
clients$age_range <- clients$birthday
clients$age_range <- ifelse((clients$birthday>=100000 & clients$birthday<=200000) , '10_20',clients$age_range)
clients$age_range <- ifelse((clients$birthday>=200000 & clients$birthday<=300000) , '20_30',clients$age_range)
clients$age_range <- ifelse((clients$birthday>=300000 & clients$birthday<=400000) , '30_40',clients$age_range)
clients$age_range <- ifelse((clients$birthday>=400000 & clients$birthday<=500000) , '40_50',clients$age_range)
clients$age_range <- ifelse((clients$birthday>=500000 & clients$birthday<=600000) , '50_60',clients$age_range)
clients$age_range <- ifelse((clients$birthday>=600000 & clients$birthday<=700000) , '60_70',clients$age_range)
clients$age_range <- ifelse((clients$birthday>=700000 & clients$birthday<=800000) , '70_80',clients$age_range)
clients$age_range <- ifelse((clients$birthday>=800000 & clients$birthday<=900000) , '80_90',clients$age_range)
clients$age_range <- ifelse((clients$birthday>=900000 & clients$birthday<=991231) , '90_99',clients$age_range)
clients$age_range <- as.factor(clients$age_range)
clients$age <- clients$birthday
clients$age <- (990000 - clients$birthday)/10000


all_values <- function(amount, type,METHOD){
  res <- c()
  for (i in 1:length(amount)){
    is_credit <- type[i] == 'credit'
    value <- amount[i]
    if(!is_credit){
      value <- -value
    }
    res <- c(res, value)
  }
  METHOD(res)
}

calculate_ratio <- function(coll1, coll2){
  if(coll2[1] == 0){
    if(coll1[1] != 0){
      2
    }else{
      0
    }
  }else{
    coll1[1]/coll2[1]
  }
}

filtered_values <- function(amount, type, credit, METHOD){
  res <- c()
  for (i in 1:length(amount)){
    is_credit <- type[i] == 'credit'
    value <- amount[i]
    if(!is_credit & credit){
      next
    }
    if(is_credit & !credit){
      next
    }
    res <- c(res, value)
  }
  if(length(res) > 0){
    METHOD(res)
  }else{
    0
  }
}


count_cmp <- function(coll, value){
  length(which(coll==value))
}

print('Calculating Statistics')

trans_calculations <- trans %>% 
  group_by(account_id) %>% 
  summarize(
    n_credit = count_cmp(type, "credit"),
    n_withdrawal = count_cmp(type, "withdrawal") + count_cmp(type, "withdrawal cash"),
    
    max_credit=filtered_values(amount,type, TRUE, max),
    avg_credit=filtered_values(amount,type, TRUE, mean),
    min_credit=filtered_values(amount,type, TRUE, min),
    sd_credit=filtered_values(amount,type, TRUE, sd),
    skew_credit=filtered_values(amount,type, TRUE, skew),
    kurtosi_credit=filtered_values(amount,type, TRUE, kurtosi),
    median_credit=filtered_values(amount,type, TRUE, median),
    mad_credit=filtered_values(amount,type, TRUE, mad),
    
    
    max_withdrawal=filtered_values(amount,type, FALSE, max),
    avg_withdrawal=filtered_values(amount,type, FALSE, mean),
    min_withdrawal=filtered_values(amount,type, FALSE, min),
    sd_withdrawal=filtered_values(amount,type, FALSE, sd),
    skew_withdrawal=filtered_values(amount,type, FALSE, skew),
    kurtosi_withdrawal=filtered_values(amount,type, FALSE, kurtosi),
    median_withdrawal=filtered_values(amount,type, FALSE, median),
    mad_withdrawal=filtered_values(amount,type, FALSE, mad),
    
    avg_amount=all_values(amount, type, mean),
    sd_amount=all_values(amount, type, sd),
    skew_amount=all_values(amount, type, skew),
    kurtosi_amount=all_values(amount, type, kurtosi),
    median_amount=all_values(amount, type, median),
    mad_amount=all_values(amount, type, mad),
    
    avg_balance=mean(balance),
    max_balance=max(balance),
    min_balance=min(balance),
    sd_balance=sd(balance),
    skew_balance=skew(balance),
    kurtosi_balance=kurtosi(balance),
    median_balance=median(balance),
    mad_balance=mad(balance)
    
    
  )

print('Summarizing by month')

trans_by_month <- trans %>% mutate(date_by_month=floor(date/100))
trans_by_month_sum <- trans_by_month %>% group_by(date_by_month, account_id) %>% summarize(sum_balance = mean(balance))

trans_by_month_final <- trans_by_month_sum %>% group_by(account_id) %>% summarize(avg_monthly_balance=mean(sum_balance))

print('Joining Tables')


disp_owners <- filter(disp,type=="OWNER")
clients_accounts <- subset(left_join(disp_owners, clients, by='client_id'), select=-district_id)
#account_district <- subset(left_join(account, district, by=c('district_id'='code')), select=c(account_id, district_id, name, region))
account_district <- left_join(account, district, by=c('district_id'='code'))
account_district_client <- left_join(clients_accounts, account_district, 'account_id')
trans_client_district <- left_join(trans_calculations, account_district_client, 'account_id')
trans_client_district <- left_join(trans_client_district, trans_by_month_final, 'account_id')


trans_client_district_loan <- left_join(loan, trans_client_district, 'account_id')
trans_client_district_loan$datediff <- trans_client_district_loan$date.x
trans_client_district_loan$datediff <- trans_client_district_loan$date.x - trans_client_district_loan$birthday

trans_ratio_calculations <- trans_client_district_loan %>% 
  group_by(loan_id) %>% 
  summarize(
    credit_per_witdrawal = calculate_ratio(avg_credit, avg_withdrawal),
    credit_per_payments = calculate_ratio(avg_credit, payments),
    crimes96_per_crime95 = calculate_ratio(crimes96, crimes95),
    loanAmount_per_avgAmount = calculate_ratio(amount, avg_amount)
  )

trans_client_final <- left_join(trans_client_district_loan, trans_ratio_calculations, 'loan_id')

trans_client_final_card <- left_join(trans_client_final, card, 'disp_id')

final_ratios <- trans_client_final_card

print('Deriving Extra attributes')


final_ratios$avg_amount_by_duration <- (final_ratios$avg_amount *  final_ratios$duration)
final_ratios$cover_loan <- final_ratios$amount - final_ratios$avg_balance  - final_ratios$avg_amount_by_duration
final_ratios$n_credits_vs_n_withdrawal <- final_ratios$n_credit - final_ratios$n_withdrawal
final_ratios$avg_credits_vs_avg_withdrawal <- final_ratios$avg_credit - final_ratios$avg_withdrawal
final_ratios$time_before_loan <- final_ratios$date.y - final_ratios$date.x
final_ratios$payments_by_duration <- final_ratios$payments * final_ratios$duration
final_ratios$avg_monthly_balance_vs_payments <- final_ratios$avg_monthly_balance - final_ratios$payments_by_duration + final_ratios$avg_amount_by_duration
final_ratios$frequency_numeric <- unclass(final_ratios$frequency)
final_ratios$region_numeric <- unclass(final_ratios$region)
final_ratios$gender_numeric <- unclass(final_ratios$gender)
final_ratios$card_numeric <- unclass(final_ratios$type.y)
final_ratios$card_numeric <- ifelse(is.na(final_ratios$card_numeric), 0, final_ratios$card_numeric)
final_ratios$issued <- ifelse(is.na(final_ratios$issued), 0, final_ratios$issued)
final_ratios$sd_withdrawal <- ifelse(is.na(final_ratios$sd_withdrawal), 0, final_ratios$sd_withdrawal)
final_ratios$max_balance_vs_min_balance <- final_ratios$min_balance - final_ratios$max_balance 


print('Finished')



#date.x Loan Data Date.y account_date




# group_by_region <- trans_client_district %>% 
#   group_by(region) %>% 
#   summarize(
#     avg_credit=mean(avg_credit),
#     avg_withdrawal=mean(avg_withdrawal),
#     avg_amount=mean(avg_amount),
#     avg_balance=mean(avg_balance)
#   )
# 
# group_by_gender <- trans_client_district %>% 
#   group_by(gender) %>% 
#   summarize(
#     avg_credit=mean(avg_credit),
#     avg_withdrawal=mean(avg_withdrawal),
#     avg_amount=mean(avg_amount),
#     avg_balance=mean(avg_balance)
#   )
# 
# group_by_name <- trans_client_district %>% 
#   group_by(name) %>% 
#   summarize(
#     avg_credit=mean(avg_credit),
#     avg_withdrawal=mean(avg_withdrawal),
#     avg_amount=mean(avg_amount),
#     avg_balance=mean(avg_balance)
#   )
# 
# group_by_age_range <- trans_client_district %>% 
#   group_by(age_range) %>% 
#   summarize(
#     avg_credit=mean(avg_credit),
#     avg_withdrawal=mean(avg_withdrawal),
#     avg_amount=mean(avg_amount),
#     avg_balance=mean(avg_balance)
#   )
#Approved Loans
#ggplot(trans_client_district_loan[trans_client_district_loan$status>0,], mapping=aes(x=age_range)) + geom_histogram(binwidth = 300, stat="count")
# + theme_bw() + coord_cartesian() + scale_color_gradient()
#Paid vs noy paid Loans
#ggplot(trans_client_district_loan, mapping=aes(x=status, group=status)) + geom_bar(stat="count") + xlab("Status") + ylab("Number of Loans") + ggtitle("Number of Paid vs Not Paid Loans")

#ggplot(data = group_by_age_range) + geom_bar(mapping=aes(x=age_range, y=avg_amount), stat="identity") + xlab('Decades') + ylab('Averaged Credited Amount (M.U)') + ggtitle('Averaged Credited Amount Grouped By Decades') + theme_bw() + coord_cartesian() + scale_color_gradient()
#ggplot(data = group_by_age_range) + geom_bar(mapping=aes(x=age_range, y=avg_balance), stat="identity") + xlab('Decades') + ylab('Average Balance (M.U)') + ggtitle('Average Balance Grouped By Decades') + theme_bw() + coord_cartesian() + scale_color_gradient()
#ggplot(data = trans_client_district) + geom_point(mapping=aes(x=avg_credit, y=avg_withdrawal), stat="identity") + theme_bw() + coord_cartesian() + scale_color_gradient() + geom_abline(intercept=0, slope=1, linetype="dashed", color="red", size=1.25) + xlab("Average Credit") + ylab("Average Withdrawal") + ggtitle("Comparison of the Averages Between Credit and Withdrawal (per account).") 
#print(trans_calculations)
#[1] "loan_id"               "account_id"            "date.x"                "amount"               
#[5] "duration"              "payments"              "status"                "max_credit"           
#[9] "avg_credit"            "min_credit"            "max_withdrawal"        "avg_withdrawal"       
#[13] "min_withdrawal"        "avg_amount"            "avg_balance"           "max_balance"          
#[17] "min_balance"           "disp_id"               "client_id"             "type"                 
#[21] "birthday"              "gender"                "age_range"             "district_id"          
#[25] "frequency"             "date.y"                "name"                  "region"               
#[29] "hab"                   "m_hab.499"             "m_hab500.1999"         "m_hab2000.9999"       
#[33] "m_hab.10000"           "cities"                "urban_hab"             "salary"               
#[37] "unemployment95"        "unemployment96"        "enterpreneurs_per_hab" "crimes95"             
#[41] "crimes96"
