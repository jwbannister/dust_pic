import csv
import pandas as pd
import os

path = "/home/john/Desktop/EPA AQS Data"
files = os.listdir(path)
wind_files = [fl for fl in files if 'WIND' in fl]
pm10_files = [fl for fl in files if 'WIND' not in fl]

wind_dfs = []
for i in range(len(wind_files)):
    print(i)
    wind_tmp = pd.read_csv(path + "/" + wind_files[i])
    wind_tmp = wind_tmp[(wind_tmp['County Code']==27) & \
            (wind_tmp['State Code']==6)]
    wind_dfs.append(wind_tmp)
wind_df = pd.concat(wind_dfs)

pm10_dfs = []
for i in range(len(pm10_files)):
    print(i)
    pm10_tmp = pd.read_csv(path + "/" + pm10_files[i])
    pm10_tmp = pm10_tmp[(pm10_tmp['County Code']==27) & \
            (pm10_tmp['State Code']==6)]
    pm10_dfs.append(pm10_tmp)
pm10_df = pd.concat(pm10_dfs)

wind_locs = wind_df[['Site Num', 'Latitude', 'Longitude']].drop_duplicates()
pm10_locs = pm10_df[['Site Num', 'Latitude', 'Longitude']].drop_duplicates()
site_locs = pd.concat([wind_locs, pm10_locs], join='inner', join_axes=0)

w_df = wind_df['State Code', 'County Code', 'Site Num', 'Parameter Code', \

coso_wind_2015 = wind_2015[(wind_2015['County Code']==27) & \
        (wind_2015['State Code']==6)]

pm10_2015 = pd.read_csv(path + "/" + files[0])
coso_pm10_2015 = pm10_2015[(pm10_2015['County Code']==27) & \
        (pm10_2015['State Code']==6)]


