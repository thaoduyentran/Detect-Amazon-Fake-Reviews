import pandas as pd
import json 
import numpy as np

data = []
with open('/Users/thaoduyentran/Downloads/Electronics_5.json', "r") as f:   
    for l in f:
        data.append(json.loads(l.strip()))

df = pd.DataFrame.from_dict(data)    

# print all columns , 1 row 
print(df.iloc[0])
print(df.tail(100))

pd.set_option('max_columns', None)


# convert series to Pandas datetime
df['reviewTime'] = pd.to_datetime(df['reviewTime'])

# construct Boolean mask
mask = df['reviewTime'].between('2018-06-01', '2018-12-31')
mask2 = df['reviewTime'].between('2008-06-01', '2008-12-31')
# apply Boolean mask
new_df = df[mask]
new_df2 = df[mask2]

print(new_df.iloc[0])
# Subset 1000 rows
df2018 = new_df.head(1000)
df2008 = new_df2.head(1000)
print(df2)


# Check null values
df2018.isnull().values.any()
df2018.isnull().sum()
df2008.isnull().sum()

