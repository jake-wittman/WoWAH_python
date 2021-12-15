import os
# Set up python functions
import requests
from datetime import datetime
import pandas as pd
import dropbox

DROPBOX_ACCESS = os.environ.get('DROPBOX_ACCESS')
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

CLIENT_ID_WOW = os.environ.get('CLIENT_ID_WOW')
CLIENT_SECRET_WOW = os.environ.get('CLIENT_SECRET_WOW')
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
# Unit prices are for stackable items, buyout is for unstackable. I just want a single gold cost column
auction_data['buyout'] = auction_data['buyout'].fillna(0)
auction_data['unit_price'] = auction_data['unit_price'].fillna(0)
auction_data['cost'] = auction_data['buyout'] + auction_data['unit_price']
# Cost is in copper, convert to gold
auction_data['cost_g'] = auction_data['cost'] / 10000

#   These are subgroups of an equipable item with the bonus stats (intellect agility, strength, etc)
auction_data['collection_year'] = datetime.now().strftime('%Y')
auction_data['collection_month'] = datetime.now().strftime('%m')
auction_data['collection_day'] = datetime.now().strftime('%d')
auction_data['collection_hour'] = datetime.now().strftime('%H')
filename = datetime.now().strftime('Malfurion_NA-%Y-%m-%d-%H-%M.csv')
dropbox_filename = "/" + filename
auction_data.to_csv(filename, index = False)

with open(filename, 'rb') as f:
   dbx.files_upload(f.read(), dropbox_filename)