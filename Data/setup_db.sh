db=sqlite:///test.db

echo 'Creating Database, May Take a While...'
csvsql --db $db --insert ./csv_db/*

echo 'Database Created!'