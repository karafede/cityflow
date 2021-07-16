
import os
os.chdir('D:/Federico/CityFLows')
os.getcwd()

import numpy as np
import pandas as pd
import geopandas as gpd
from geopandas import GeoDataFrame
from shapely.geometry import Point
import folium
import osmnx as ox
import networkx as nx
import math
import momepy
from funcs_network_FK import roads_type_folium
from shapely import geometry
from shapely.geometry import Point, Polygon
import psycopg2
import db_connect
import datetime
from datetime import datetime
from datetime import date
from datetime import datetime
from geoalchemy2 import Geometry, WKTElement
from sqlalchemy import *
import sqlalchemy as sal
import geopy.distance

#### VORONOI functions
import contextily as ctx
import matplotlib.pyplot as plt
from shapely.ops import cascaded_union
import json
import datetime
import seaborn as sn

from geovoronoi.plotting import subplot_for_map, plot_voronoi_polys_with_points_in_area
from geovoronoi import voronoi_regions_from_coords, points_to_coords

os.chdir('D:/Federico/CityFLows')
os.getcwd()

## read sample json from Altran
with open('sample.json') as json_data:
    Altran = json.load(json_data)



######################################################################
##### NEW DATA FORMAT ################################################
######################################################################

i = 0
file = open("zzz_report.txt")
for x in file:
    i = i+1
    print(i)
    f = x.split(",")
    print(f)
    if i == 6:
        break


## analyzed the structure of the file and by find people ID and near neighbors
first_level = x.split("POINT_LIST")

### ---------- ####
### TIMESTSAMP ####
### ---------- ####

timestamp = first_level[0]
timestamp = timestamp.split("':")
TS = timestamp[1]
TS = TS.split(",")
TS = TS[0]
TS = TS[2:28]

date_time_str = TS
date_time_obj = datetime.datetime.strptime(date_time_str, '%Y-%m-%d %H:%M:%S.%f')
print('Date:', date_time_obj.date())
print('Time:', date_time_obj.time())
print('Date-time:', date_time_obj)


########################################
########################################

second_level = first_level[1]
detectedpeople = second_level.split('ID')
type(detectedpeople)

people_ID = [s for s in detectedpeople if "DIST_LIST" in s]
type(people_ID)

for idx, element in enumerate(detectedpeople):
    if "DIST_LIST" in element:
        print('index_people_ID:', idx, element)
    if "DIST_LIST" not in element:
        print('closed_people_ID', idx, element)

closed_people_ID = [s for s in detectedpeople if "DIST_LIST" not in s]

## get all ID...of the central point...
for idx, element in enumerate(people_ID):
    print(idx, element)
    central_ID = element.split("':")[1]
    central_ID = int(central_ID.split(',')[0])
    print('central_ID:', central_ID)
    central_ID_x = int((element.split(", 'X':")[1]).split(',')[0])
    print('central_ID_x:',  central_ID_x)
    central_ID_y = int((element.split(", 'Y':")[1]).split(',')[0])
    print('central_ID_y:', central_ID_y)



### NEW VERSION also including the closed people_ID
## get all ID...of the central point...
## define a DICTIONARY with the coordinates of all IDs for each timestamp
coords_dict = {}
for idx, element in enumerate(detectedpeople):
    if idx > 0:
        if "DIST_LIST" in element:
            print('INPUT ---> index_people_ID:', idx, element)
            # people_ID = [s for s in detectedpeople if "DIST_LIST" in s]
            # print(idx, element)
            central_ID = element.split("':")[1]
            central_ID = int(central_ID.split(',')[0])
            print('central_ID:', central_ID)
            central_ID_x = int((element.split(", 'X':")[1]).split(',')[0])
            print('central_ID_x:',  central_ID_x)
            central_ID_y = int((element.split(", 'Y':")[1]).split(',')[0])
            print('central_ID_y:', central_ID_y)
            ## build a dictionary with ID, x, Y for each ID
            list_ID = [central_ID_x, central_ID_y]
            coords_dict[central_ID] = list_ID
            coords_dict.get(central_ID)
        if "DIST_LIST" not in element:
            print('INPUT ---> ---> ---> closed_people_ID', idx, element)
            closed_ID = element.split("':")[1]
            closed_ID = int(closed_ID.split(',')[0])
            print('closed_ID:', closed_ID)
            DIST_closed_ID = element.split(", 'DIST':")[1]
            DIST_closed_ID = int(''.join(c for c in DIST_closed_ID if c.isdigit()))
            print('DIST_closed_ID:', DIST_closed_ID)




########################################################################################
########################################################################################
########################################################################################
########### ---------- put all together -------------- #################################
########################################################################################

## define a DICTIONARY with the coordinates of all IDs for each timestamp
coords_dict = {}
i = 0
ID = []
ID_X = []
ID_Y = []
CLOSE_ID = []
CLOSE_ID_xy = []
ASSOCIATE_ID =[]
DIST = []
timedate = []
timedate_CLOSE_ID = []
file = open("zzz_report.txt")
for x in file:
    i = i+1
    print(i)
    f = x.split(",")
    # print(f)
    first_level = x.split("POINT_LIST")
    #### TIMESTAMP #####################################
    timestamp = first_level[0]
    timestamp = timestamp.split("':")
    TS = timestamp[1]
    TS = TS.split(",")
    TS = TS[0]
    TS = TS[2:28]
    date_time_str = TS
    date_time_obj = datetime.datetime.strptime(date_time_str, '%Y-%m-%d %H:%M:%S.%f')
    # print('Date:', date_time_obj.date())
    # print('Time:', date_time_obj.time())
    print('Date-time:', date_time_obj)
    ## DETECTING PEOPLE #################################
    second_level = first_level[1]
    detectedpeople = second_level.split('ID')
    people_ID = [s for s in detectedpeople if "DIST_LIST" in s]
    closed_people_ID = [s for s in detectedpeople if "DIST_LIST" not in s]
    # if i == 13:
    #     break
    ## get all ID...of the central point...
    for idx, element in enumerate(detectedpeople):
        if idx > 0:
            if "DIST_LIST" in element:
                print('INPUT ---> index_people_ID:', idx, element)
                # people_ID = [s for s in detectedpeople if "DIST_LIST" in s]
                # print(idx, element)
                central_ID = element.split("':")[1]
                central_ID = int(central_ID.split(',')[0])
                print('central_ID:', central_ID)
                central_ID_x = int((element.split(", 'X':")[1]).split(',')[0])
                print('central_ID_x:', central_ID_x)
                central_ID_y = int((element.split(", 'Y':")[1]).split(',')[0])
                print('central_ID_y:', central_ID_y)
                ## BUILD list of (X,Y)
                ID.append(central_ID)
                ID_X.append(central_ID_x)
                ID_Y.append(central_ID_y)
                timedate.append(TS)
                ## build a DICTIONARY with ID, x, Y for each ID
                list_ID = [central_ID_x, central_ID_y]
                coords_dict[central_ID] = list_ID
    for idx, element in enumerate(detectedpeople):
        if idx > 0:
            if "DIST_LIST" in element:
                central_ID = element.split("':")[1]
                central_ID = int(central_ID.split(',')[0])
        if idx > 0:
            if "DIST_LIST" not in element:
                print('INPUT ---> ---> ---> closed_people_ID', idx, element)
                ### keep track of the CENTRAL ID...
                ASSOCIATE_ID.append(central_ID)
                closed_ID = element.split("':")[1]
                closed_ID = int(closed_ID.split(',')[0])
                print('closed_ID:', closed_ID)
                DIST_closed_ID = element.split(", 'DIST':")[1]
                DIST_closed_ID = int(''.join(c for c in DIST_closed_ID if c.isdigit()))
                print('DIST_closed_ID:', DIST_closed_ID)
                ## get coordinates for the close_ID (different length compared to the central IDs)
                CLOSE_ID_xy.append(coords_dict.get(closed_ID))
                # if idx == 2:
                #    break
                ## populate list of all CLOSE IDs
                CLOSE_ID.append(closed_ID)
                ## populate list of all distancece of CLOSE IDs from the CENTRAL IDs
                DIST.append(DIST_closed_ID)
                timedate_CLOSE_ID.append(TS)


##-------- build dataframe for CENTRAL points --------############
list_of_X_Y = list(zip(ID_X, ID_Y))
df_X_Y = pd.DataFrame(list_of_X_Y, columns=['X', 'Y'])
df_X_Y['timedate'] = timedate
df_X_Y['ID'] = ID
## order columns...
df_X_Y = df_X_Y[['timedate', 'ID', 'X', 'Y']]
# sort by 'timedate'
df_X_Y.sort_values(['timedate'],ascending=True, inplace=True)


##------ build dataframe for CLOSE points (NEAR NEIGHBORS) ---####
## replace None vales with a [0,0] list
res = [[0,0] if v is None else v for v in CLOSE_ID_xy]
## transform list of lists into list of tuples
res = [tuple(l) for l in res]
df_X_Y_CLOSE_ID = pd.DataFrame(res, columns=['CLOSE_X', 'CLOSE_Y'])
## add other columns to the dataframe
df_X_Y_CLOSE_ID['timedate'] = timedate_CLOSE_ID
df_X_Y_CLOSE_ID['CLOSE_ID'] = CLOSE_ID
df_X_Y_CLOSE_ID['ID'] = ASSOCIATE_ID
df_X_Y_CLOSE_ID['DIST'] = DIST
## order columns...
df_X_Y_CLOSE_ID = df_X_Y_CLOSE_ID[['timedate', 'ID', 'CLOSE_ID', 'CLOSE_X', 'CLOSE_Y', 'DIST']]
# sort by 'timedate'
df_X_Y_CLOSE_ID.sort_values(['timedate'],ascending=True, inplace=True)
## set 0 values to blank
df_X_Y_CLOSE_ID[df_X_Y_CLOSE_ID[['CLOSE_X', 'CLOSE_Y']].eq(0)] = np.nan



## merge CENTRAL_IDs with CLOSE_IDs (near neighbors)
DF_people = pd.merge(df_X_Y, df_X_Y_CLOSE_ID, on=['timedate', 'ID'], how='left')
# DF_people = DF_people.replace(np.nan, '', regex=True)
### remove rows with duplicated values in 'CLOSE_ID' and 'ID'
DF_people = DF_people[DF_people['ID'] != DF_people['CLOSE_ID']]

DF_people['timedate'] = DF_people['timedate'].astype('datetime64[ns]')
## define "date" only
DF_people['date'] = DF_people['timedate'].apply(lambda x: x.strftime("%Y-%m-%d"))
# define "hour"
DF_people['hour'] = DF_people['timedate'].apply(lambda x: x.hour)
# define minutes
DF_people['minute'] = DF_people['timedate'].apply(lambda x: x.minute)
# define seconds
DF_people['seconds'] = DF_people['timedate'].apply(lambda x: x.second)
## set the index of the timeseries
# DF_people = DF_people.set_index('date')

# sort by 'timedate'
DF_people.sort_values(['timedate'],ascending=True, inplace=True)

type(DF_people)
# DF_people.dtypes



###########################################################################
#####------- plot TIMESERIES of people COUNTS ----- #######################
############# CENTRAL PEOPLE ID ###########################################

### https://www.dataquest.io/blog/tutorial-time-series-analysis-with-pandas/
### get number of counts for each timestamp ###
timestamps_counts = DF_people.groupby(DF_people[['timedate']].columns.tolist(), sort=False).size().reset_index().rename(columns={0:'counts'})
## timestamps with more registrations
max_timestamp = timestamps_counts['timedate'][timestamps_counts.counts == max(timestamps_counts.counts)]
timestamps_counts.drop_duplicates(['timedate'], inplace=True)
LIST_TIMESTAMPS = list(timestamps_counts.timedate.unique())

timestamps_counts = DF_people.groupby(DF_people[['timedate', 'date', 'hour', 'minute', 'seconds']].columns.tolist(), sort=False).size().reset_index().rename(columns={0:'counts'})

## set the INDEX of the timeseries
timestamps_counts = timestamps_counts.set_index('timedate')


####------ make a resampling by minutes......
## https://pandas.pydata.org/pandas-docs/stable/user_guide/timeseries.html
# Specify the data columns we want to include (i.e. exclude Year, Month, Weekday Name)
data_columns = ['counts']
# Resample to minutely frequency, aggregating with mean
timestamps_counts_MIN_mean = timestamps_counts[data_columns].resample('min').mean()
timestamps_counts_SECs_mean = timestamps_counts[data_columns].resample('S').mean()   ### 1 seconds
timestamps_counts_SECs_mean = timestamps_counts[data_columns].resample('2S').mean()  ### 5 seconds


# Start and end of the date range to extract
start, end = '2021-07-07', '2021-07-07'
# Plot daily and weekly resampled time series together
fig = plt.figure(figsize=(10,8))
ax = fig.add_subplot(111)
ax.plot(timestamps_counts.loc[start:end, 'counts'], marker='', linestyle='dotted', linewidth=0.5, label='by timestamp',  color='gray')
# ax.plot(timestamps_counts_SECs_mean.loc[start:end, 'counts'], marker='', linestyle='-', linewidth=0.5, label='by timestamp',  color='gray')
ax.plot(timestamps_counts_MIN_mean.loc[start:end, 'counts'], marker='o', markersize=6, linestyle='solid', label='minute mean resample', color='red')
ax.set_ylabel('number of people')
ax.legend()
ax.title.set_text('timeseries IDs and CLOSE IDs (2021-07-07)')
plt.savefig("timeseries_people_ID_ALTRAN.png")
plt.close()


#### DISTANCES of CLOSE people from CENTRAL people
np.mean(DF_people['DIST'][~np.isnan(DF_people['DIST'])].astype(np.int64))
np.max(DF_people['CLOSE_X'][~np.isnan(DF_people['CLOSE_X'])].astype(np.int64))
np.max(DF_people['X'][~np.isnan(DF_people['X'])].astype(np.int64))
np.max(DF_people['CLOSE_Y'][~np.isnan(DF_people['CLOSE_Y'])].astype(np.int64))
np.max(DF_people['Y'][~np.isnan(DF_people['Y'])].astype(np.int64))

################################################
##### SPATIAL PLOT #############################

# ## plot all data
# sn.set_style('whitegrid')
# sn.lmplot('CLOSE_X','CLOSE_Y',data = DF_people,
#        palette=' blue',size=10,fit_reg=False)
# sn.lmplot('X','Y',data = DF_people,
#        palette='red',size=10,fit_reg=False)
# plt.show()


## https://stackoverflow.com/questions/14827650/pyplot-scatter-plot-marker-size


fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)
plt.scatter(DF_people.X, DF_people.Y, c='blue', marker='+', s=(10 * 72. / fig.dpi) ** 2, label='central_people')
plt.scatter(DF_people.CLOSE_X, DF_people.CLOSE_Y, c='red', marker='s', s=15, label='CLOSE_people', alpha=0.2)
plt.legend(loc='upper left')
plt.grid(color='gray', linestyle='-', linewidth=1)
plt.show()
plt.savefig("map_people_ID_ALTRAN.png")
plt.close()


'''
fig = plt.figure()
ax1 = fig.add_subplot(111)
### make a loop to visualize time series
for idx, timestamp in enumerate(LIST_TIMESTAMPS):
    # fig = plt.figure()
    # ax1 = fig.add_subplot(111)
    ## filter data by selected 'timestamp'
    selected_DF_people = DF_people[DF_people.timedate == timestamp]
    # print(selected_DF_people)
    plt.scatter(selected_DF_people.X, selected_DF_people.Y, c='blue', marker='+', s=(10 * 72. / fig.dpi) ** 2, label='central_people')
    plt.scatter(selected_DF_people.CLOSE_X, selected_DF_people.CLOSE_Y, c='red', marker='s', s=15, label='CLOSE_people', alpha=0.3)
    plt.legend(loc='upper left')
    plt.show()
'''

