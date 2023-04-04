#!/usr/bin/python3

# massage.py
#
# Data massager for Gun Deaths vs Presidential Preference
# project.
import csv
import re
import string

GUN_RAW_PATH = "raw_data/gun_deaths_us_1999_2019.csv"
GUN_COOKED_PATH = "gun_data.csv"
POL_RAW_PATH = "countypres_2000-2020.csv"
POL_COOKED_PATH = "pol_data.csv"
COUNTY_RE = re.compile(r"""\s+County\s*$""")

with open(GUN_COOKED_PATH, 'w', newline='') as csv_out:
    writer = csv.writer(csv_out, dialect='unix')
    with open(GUN_RAW_PATH, newline='') as csv_in:
        reader = csv.reader(csv_in)
        for row in reader:
            county = row[2]
            row[2] = COUNTY_RE.sub('', county).upper()
            writer.writerow(row)
