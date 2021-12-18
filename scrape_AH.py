#########
# This script uses access tokens for dropbox and Blizzard API keys to scrape
# data from the WoW auction house and upload the csv to drop box.
# The pull_data.py script will pull the data from dropbox, delete it once pulled
# and add it to the database. 
#########
import requests
import os
from datetime import datetime
import pandas as pd
import dropbox
from decouple import config

DROPBOX_ACCESS = config('DROPBOX_ACCESS')
dbx = dropbox.Dropbox(DROPBOX_ACCESS)

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

auction_data = auction_data.rename(columns={"id": "auction_id",})
# Expand the item column
auction_data = pd.concat([auction_data.drop(['item'], axis=1), auction_data['item'].apply(pd.Series)], axis=1)
auction_data['id'] = auction_data['id'].map(int).map(str) # Convert to int to remove trailing 0, then to string
# Unit prices are for stackable items, buyout is for unstackable. I just want a single gold cost column
auction_data['buyout'] = auction_data['buyout'].fillna(0)
auction_data['unit_price'] = auction_data['unit_price'].fillna(0)
auction_data['cost'] = auction_data['buyout'] + auction_data['unit_price']
# Cost is in copper, convert to gold
auction_data['cost_g'] = auction_data['cost'] / 10000
# Remove rows for pets. I don't care about pets
# And remove columns about pets
is_pet_mask = auction_data['pet_breed_id'].isna()
auction_data = auction_data[is_pet_mask]
auction_data.drop(columns = ['pet_breed_id', 'pet_level', 'pet_quality_id', 'pet_species_id'], inplace = True)

#   These are subgroups of an equipable item with the bonus stats (intellect agility, strength, etc)
# Make one date time column and also 4 columns for each relevant piece
auction_data['date_time'] = datetime.now()
auction_data['collection_year'] = datetime.now().strftime('%Y')
auction_data['collection_month'] = datetime.now().strftime('%m')
auction_data['collection_day'] = datetime.now().strftime('%d')
auction_data['collection_hour'] = datetime.now().strftime('%H')
filename = datetime.now().strftime('Malfurion_NA-%Y-%m-%d-%H-%M.csv')
dropbox_filename = "/" + filename
auction_data.to_csv(filename, index = False)

with open(filename, 'rb') as f:
   dbx.files_upload(f.read(), dropbox_filename)

os.remove(filename)

print("Finished " + filename)