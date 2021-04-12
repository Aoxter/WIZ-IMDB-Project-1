import pandas as pd
import numpy as np
import math

df = pd.read_csv("imdb_top_1000.csv")
new_gross = []
old_gross = df['Gross'].tolist()
old_gross = [str(x) for x in old_gross]
old_gross = ['0' if x=='nan' else x for x in old_gross]
old_gross = [x.replace(",", "") for x in old_gross]
for gross in old_gross:
    new_gross.append(int(gross))
df.drop(columns=['Gross'])
df['Gross'] = new_gross
df.to_csv('imdb.csv')
