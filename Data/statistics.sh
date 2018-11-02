echo 'Processing Statistics.'
cd csv_db

for file in *.csv; do
    echo '-Processing: ' $file
    output=../stats/$file
    csvstat $file > $output
    echo '--Saved to:'$output

done