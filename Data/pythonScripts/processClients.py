import csv
import sys

file = sys.argv[1]
outFile = sys.argv[2]

with open(file, 'r') as csvfile:
    with open(outFile, 'w') as csvnew:
        reader = csv.reader(csvfile, delimiter=';' )
        fieldNames = ['birthday', 'gender']
        writer = csv.DictWriter(csvnew, fieldNames, delimiter=';')
        writer.writeheader()
        jump = True
        for row in reader:
            if jump:
                jump = False
                continue
            birthday = str(row[1]);
            print(birthday)
            print(birthday[2:4])
            month = birthday[2:4]
            print(month)
            month = int(float(month))
            gender = 'm'
            if month > 50:
                month = month - 50
                gender = 'f'

            birthday = "{0}{1:02}{2}".format(birthday[0:2], month, birthday[4:])

            writer.writerow({'birthday': birthday, 'gender': gender})



