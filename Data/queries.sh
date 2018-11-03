db=sqlite:///test.db
rm -rf queries/
mkdir queries
# Calculate the Credit Values
sql2csv --db $db --query \
    " select
    account_id,
    max(amount) as max_credit,
    avg(amount) as avg_credit,
    min(amount) as min_credit
    from trans_train
    where type like '%credit%'
    group by account_id
    " > queries/credit.csv

# Calculate the withdraw values
sql2csv --db $db --query \
    " select
    account_id,
    max(amount) as max_withdrawal,
    avg(amount) as avg_withdrawal,
    min(amount) as min_withdrawal
    from trans_train
    where not type like '%credit%'
    group by account_id
    " > queries/withdrawal.csv

# Calculate the averages balance and amount
sql2csv --db $db --query \
    " select
    account_id,
    avg(balance) as avg_balance,
    max(balance) as max_balance,
    min(balance) as min_balance,
    avg(CASE WHEN type like '%credit%' THEN amount ELSE -amount END) as avg_amount
    from trans_train
    group by account_id
    " > queries/account_balance.csv

# Join credit and withdrawal values
csvsql --query \
    " SELECT
    credit.account_id AS account_id,
    max_credit, avg_credit, min_credit,
    max_withdrawal, avg_withdrawal, min_withdrawal
    FROM credit full join withdrawal ON credit.account_id = withdrawal.account_id
    GROUP BY credit.account_id
    " queries/credit.csv queries/withdrawal.csv > queries/amounts.csv

# Join values with the previously calculated averages
csvsql --query \
    " select
    amounts.account_id AS account_id,
    max_credit, avg_credit, min_credit,
    max_withdrawal, avg_withdrawal, min_withdrawal,
    avg_amount,
    avg_balance, max_balance, min_balance
    from amounts join account_balance on account_balance.account_id = amounts.account_id
    " queries/amounts.csv queries/account_balance.csv > queries/trans.csv

echo 'Calculating Account_District_Client'

sql2csv --db $db --query \
    "
    SELECT
    client_id,
    client_disp.account_id as account_id,
    district_id,
    birthday,
    gender,
    dem_name,
    dem_region
    FROM
        (SELECT 
            account_id, 
            district_id, 
            name as dem_name, 
            region as dem_region
        FROM 
            account join district on account.district_id = district.code
        ) as account_district

        JOIN

        (SELECT
            clients.client_id as client_id,
            birthday,
            gender,
            account_id
        FROM
            clients join disp on disp.client_id = clients.client_id
        WHERE
            type like '%OWNER%'
        ) as client_disp

        ON client_disp.account_id=account_district.account_id
    " > queries/account_district_client.csv

echo 'Calculating Client_Trans'

csvsql --query \
    "
    SELECT
        account_district_client.account_id AS account_id,
        max_credit, avg_credit, min_credit,
        max_withdrawal, avg_withdrawal, min_withdrawal,
        avg_amount,
        avg_balance, max_balance, min_balance,
        district_id,
        birthday,
        gender,
        dem_name,
        dem_region
    FROM
        trans 
        JOIN
        account_district_client
        ON account_district_client.account_id=trans.account_id
    " queries/account_district_client.csv queries/trans.csv > queries/client_trans.csv
