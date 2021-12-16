############
# This script pulls WoW AH data from my dropbox to my machine, where
# I'm building a database of this data. 
############
import requests
import os
from datetime import datetime
import pandas as pd
import dropbox
from decouple import config
import pandas
import sqlite3

DROPBOX_ACCESS = config('DROPBOX_ACCESS')
dbx = dropbox.Dropbox(DROPBOX_ACCESS)

# Download csvs from dropbox
# These go in the temp_csvs folder until they're added to the databse
# then they're deleted

db_files = dbx.files_list_folder("")
for i in db_files.entries:
    dbx.files_download_to_file('data/temp_csvs/'+i.name, i.path_lower)


# Connect to database and add csvs
conn = sqlite3.connect('data/WoWAH_db.sqlite')
ah_csvs = os.listdir('data/temp_csvs')
curs = conn.cursor() # Create cursor w/e that is
for i in ah_csvs:
    temp_file = pandas.read_csv('data/temp_csvs/' + i)
    temp_file.to_sql('auctions', conn, if_exists='append', index = False)
   

# Once files are in the database, go ahead and delete from dropbox
for i in db_files.entries:
    dbx.files_delete("/" + i.name)

# And delete  csvs
for i in ah_csvs:
    os.remove('data/temp_csvs/' + i)

conn.close()