import requests
import os
from datetime import datetime
import pandas as pd
from decouple import config
from itertools import islice
from math import ceil
import sqlite3
import time # to put a delay in the loop. Think I'm overwhelming api?

# Create access toeken
def create_access_token(client_id, client_secret, region = "us"):
   data = { 'grant_type': 'client_credentials' }
   response = requests.post('https://%s.battle.net/oauth/token' % region,
                            data = data,
                            auth = (client_id, client_secret))
   return response.json()

# Get Malf and connected realms data
def get_malfurion(search):
   search = search
   response = requests.get(search)
   return response.json()["auctions"]

CLIENT_ID_WOW = config('CLIENT_ID_WOW')
CLIENT_SECRET_WOW = config('CLIENT_SECRET_WOW')
response = create_access_token(CLIENT_ID_WOW, CLIENT_SECRET_WOW)
token = response['access_token']

# ID for Malfurion server

id = 1175
search = f"https://us.api.blizzard.com/data/wow/connected-realm/{id}/auctions?namespace=dynamic-us&locale=en_US&access_token={token}"

# Get Data
auction_data = get_malfurion(search)
# Convert data to a dataframe
auction_data = pd.DataFrame(auction_data)

auction_data = auction_data.rename(columns={"id": "item_id"})
# Expand the item column
auction_data = pd.concat([auction_data.drop(['item'], axis=1), auction_data['item'].apply(pd.Series)], axis=1)
auction_data['id'] = auction_data['id'].map(int).map(str)

item_ids = auction_data['id']
item_ids = pd.Series.tolist(item_ids)

# Get item ids already in database
# Connect to database and add table
conn = sqlite3.connect('data/WoWAH_db.sqlite')
curs = conn.cursor() # Create cursor w/e that is
curs.execute('SELECT id FROM item_id')
existing_ids = curs.fetchall()
# convert to a list
existing_ids = [item for i in existing_ids for item in i]
missing_ids = []
for i in item_ids:
    if i not in existing_ids:
        missing_ids.append(i)

# In my experimentation, the API only ever returned 50 items so I split
# the item IDs into chunks of length 50.
num_of_chunks = ceil(len(missing_ids) / 50)
length_to_split = [50] * num_of_chunks
iter_ids = iter(missing_ids)
item_chunks = [list(islice(iter_ids, elem)) for elem in length_to_split]
item_names = []
if len(item_chunks) == 1:
    print(i)
    id_char = "||".join(item_chunks[i])
    search = f"https://us.api.blizzard.com/data/wow/search/item?namespace=static-us&locale=en_US&orderby=id&&_pageSize=1000&id={id_char}&_&access_token={token}"
    response = requests.get(search).json()
    response_df = pd.json_normalize(response, record_path = ['results'])
    item_df = response_df
elif len(item_chunks) > 1:
    for i in range(1, len(item_chunks)):
        print(i)
        id_char = "||".join(item_chunks[i])
        search = f"https://us.api.blizzard.com/data/wow/search/item?namespace=static-us&locale=en_US&orderby=id&&_pageSize=1000&id={id_char}&_&access_token={token}"
        response = requests.get(search).json()
        response_df = pd.json_normalize(response, record_path = ['results'])
        item_names.append(response_df)
        time.sleep(1)
    item_df = pd.concat(item_names, ignore_index = True, axis = 0, sort = True)
else:
    exit()
sub_item_df = item_df[['data.id', 'data.is_equippable', 'data.is_stackable',
'data.level', 'data.max_count', 'data.media.id', 'data.name.en_US', 'data.purchase_price',
'data.required_level', 'data.sell_price']]
sub_item_df.columns = sub_item_df.columns.str.replace(r'data.', '') # Strip the data. prefix
sub_item_df = sub_item_df.rename(index = {'name.en_US': 'name'})

# Connect to database and add table
# Write item id df to db
sub_item_df.to_sql('item_id', conn, if_exists = 'append', index = False)
conn.close()