library(tidyverse)
clients <- read.csv('./csv_db/clients.csv',sep = ';', header=TRUE)
trans <- read.csv('./csv_db/trans_train.csv',sep = ',', header=TRUE)
account <- read.csv('./csv_db/account.csv',sep = ';', header=TRUE)
district <- read.csv('./csv_db/district.csv',sep = ';', header=TRUE)
disp <- read.csv('./csv_db/disp.csv',sep = ';', header=TRUE)

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



trans_calculations <- trans %>% 
group_by(account_id) %>% 
  summarize(
    max_credit=filtered_values(amount,type, TRUE, max),
    avg_credit=filtered_values(amount,type, TRUE, mean),
    min_credit=filtered_values(amount,type, TRUE, min),
    max_withdrawal=filtered_values(amount,type, FALSE, max),
    avg_withdrawal=filtered_values(amount,type, FALSE, mean),
    min_withdrawal=filtered_values(amount,type, FALSE, min),
    avg_amount=all_values(amount, type, mean),
    avg_balance=mean(balance),
    max_balance=max(balance),
    min_balance=min(balance)
    )

disp_owners <- filter(disp,type=="OWNER")
clients_accounts <- subset(left_join(disp_owners, clients, by='client_id'), select=-district_id)
account_district <- subset(left_join(account, district, by=c('district_id'='code')), select=c(account_id, district_id, name, region))
account_district_client <- left_join(clients_accounts, account_district, 'account_id')
trans_client_district <- left_join(trans_calculations, account_district_client, 'account_id')

group_by_region <- trans_client_district %>% 
  group_by(region) %>% 
  summarize(
    avg_credit=mean(avg_credit),
    avg_withdrawal=mean(avg_withdrawal),
    avg_amount=mean(avg_amount),
    avg_balance=mean(avg_balance)
  )

group_by_gender <- trans_client_district %>% 
  group_by(gender) %>% 
  summarize(
    avg_credit=mean(avg_credit),
    avg_withdrawal=mean(avg_withdrawal),
    avg_amount=mean(avg_amount),
    avg_balance=mean(avg_balance)
  )

group_by_name <- trans_client_district %>% 
  group_by(name) %>% 
  summarize(
    avg_credit=mean(avg_credit),
    avg_withdrawal=mean(avg_withdrawal),
    avg_amount=mean(avg_amount),
    avg_balance=mean(avg_balance)
  )

group_by_age_range <- trans_client_district %>% 
  group_by(age_range) %>% 
  summarize(
    avg_credit=mean(avg_credit),
    avg_withdrawal=mean(avg_withdrawal),
    avg_amount=mean(avg_amount),
    avg_balance=mean(avg_balance)
  )

#ggplot(data = group_by_age_range) + geom_bar(mapping=aes(x=age_range, y=avg_amount), stat="identity") + xlab('Decades') + ylab('Averaged Credited Amount (M.U)') + ggtitle('Averaged Credited Amount Grouped By Decades') + theme_bw() + coord_cartesian() + scale_color_gradient()
#ggplot(data = group_by_age_range) + geom_bar(mapping=aes(x=age_range, y=avg_balance), stat="identity") + xlab('Decades') + ylab('Average Balance (M.U)') + ggtitle('Average Balance Grouped By Decades') + theme_bw() + coord_cartesian() + scale_color_gradient()
#ggplot(data = trans_client_district) + geom_point(mapping=aes(x=avg_credit, y=avg_withdrawal), stat="identity") + theme_bw() + coord_cartesian() + scale_color_gradient() + geom_abline(intercept=0, slope=1, linetype="dashed", color="red", size=1.25) + xlab("Average Credit") + ylab("Average Withdrawal") + ggtitle("Comparison of the Averages Between Credit and Withdrawal (per account).") 
print(trans_calculations)