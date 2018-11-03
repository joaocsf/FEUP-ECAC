db=sqlite:///test.db
sql2csv --db $db --query \
    " select
    account_id,
    avg(balance) as avg_balance,
    max(balance) as max_balance,
    min(balance) as min_balance,
    avg(amount) as avg_amount,
    max(amount) as max_amount,
    min(amount) as min_amout
    from trans_train
    group by account_id
    " > queries/query_trans.csv