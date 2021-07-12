
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
    if i == 1865:
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
timedate = []
timedate_CLOSE_ID = []
file = open("zzz_report.txt")
for x in file:
    i = i+1
    print(i)
    f = x.split(",")
    # print(f)
    # if i == 1865:
    #    break
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
                ## build a dictionary with ID, x, Y for each ID
                list_ID = [central_ID_x, central_ID_y]
                coords_dict[central_ID] = list_ID
                # coords_dict.get(central_ID)
            if "DIST_LIST" not in element:
                print('INPUT ---> ---> ---> closed_people_ID', idx, element)
                closed_ID = element.split("':")[1]
                closed_ID = int(closed_ID.split(',')[0])
                print('closed_ID:', closed_ID)
                DIST_closed_ID = element.split(", 'DIST':")[1]
                DIST_closed_ID = int(''.join(c for c in DIST_closed_ID if c.isdigit()))
                print('DIST_closed_ID:', DIST_closed_ID)
                ## get coordinates for the close_ID (there have different lenght compared to the central IDs)
                CLOSE_ID.append(closed_ID)
                CLOSE_ID_xy.append(coords_dict.get(closed_ID))
                timedate_CLOSE_ID.append(TS)


## build dataframe for central points
list_of_X_Y = list(zip(ID_X, ID_Y))
df_X_Y = pd.DataFrame(list_of_X_Y, columns=['X', 'Y'])
df_X_Y['timedate'] = timedate
df_X_Y['ID'] = ID
## order columns...
df_X_Y = df_X_Y[['timedate', 'ID', 'X', 'Y']]
# sort by 'timedate'
df_X_Y.sort_values(['timedate'],ascending=True, inplace=True)

## build dataframe for CLOSE points (NEAR NEIGHBORS)
## replace None vales with a [0,0] list
res = [[0,0] if v is None else v for v in CLOSE_ID_xy]
## transform list of lists into list of tuples
res = [tuple(l) for l in res]
df_X_Y_CLOSE_ID = pd.DataFrame(res, columns=['CLOSE_X', 'CLOSE_Y'])
df_X_Y_CLOSE_ID['timedate'] = timedate_CLOSE_ID
df_X_Y_CLOSE_ID['ID'] = CLOSE_ID
## order columns...
df_X_Y_CLOSE_ID = df_X_Y_CLOSE_ID[['timedate', 'ID', 'CLOSE_X', 'CLOSE_Y']]
# sort by 'timedate'
df_X_Y_CLOSE_ID.sort_values(['timedate'],ascending=True, inplace=True)
## set 0 values to blank
df_X_Y_CLOSE_ID[df_X_Y_CLOSE_ID[['CLOSE_X', 'CLOSE_Y']].eq(0)] = np.nan
# df_X_Y_CLOSE_ID['CLOSE_X'][~np.isnan(df_X_Y_CLOSE_ID['CLOSE_X'])].astype(np.int64)
# df_X_Y_CLOSE_ID['CLOSE_X'][~np.isnan(df_X_Y_CLOSE_ID['CLOSE_X'])].astype({"CLOSE_X": int})



## merge CENTRAL_IDs with CLOSE_IDs (near neighbors)
DF_people = pd.merge(df_X_Y, df_X_Y_CLOSE_ID, on=['timedate', 'ID'], how='left')
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
## set 0 values to blank


type(DF_people)
DF_people.dtypes


### https://www.dataquest.io/blog/tutorial-time-series-analysis-with-pandas/
### get number of counts for each timestamp ###
timestamps_counts = DF_people.groupby(DF_people[['timedate']].columns.tolist(), sort=False).size().reset_index().rename(columns={0:'counts'})
## timestamps with more registrations
max=timestamp = timestamps_counts['timedate'][timestamps_counts.counts == max(timestamps_counts.counts)]
timestamps_counts.drop_duplicates(['timedate'], inplace=True)
LIST_TIMESTAMPS = list(timestamps_counts.timedate.unique())


timestamps_counts = DF_people.groupby(DF_people[['timedate', 'date', 'hour', 'minute', 'seconds']].columns.tolist(), sort=False).size().reset_index().rename(columns={0:'counts'})


## set the INDEX of the timeseries
timestamps_counts = timestamps_counts.set_index('timedate')
## plot a distribution of counts by timestamps
sn.set(rc={'figure.figsize':(11, 4)})
timestamps_counts['counts'].plot(linewidth=0.5)

# better plots....
ax = timestamps_counts.loc['2021-07-07', 'counts'].plot()
ax.set_ylabel('number of people')
#### resample by minutes
##  zoom in further and look at 16:30 and 17:00
# timestamps_counts = DF_people.groupby(DF_people[['timedate', 'date', 'hour', 'minute', 'seconds']].columns.tolist(), sort=False).size().reset_index().rename(columns={0:'counts'})
# timestamps_counts = timestamps_counts.set_index('minute')
# ax = timestamps_counts.loc['30':'57', 'counts'].plot()

## make a resampling by minutes......
## https://pandas.pydata.org/pandas-docs/stable/user_guide/timeseries.html
# Specify the data columns we want to include (i.e. exclude Year, Month, Weekday Name)
data_columns = ['counts']
# Resample to minutely frequency, aggregating with mean
timestamps_counts_mean = timestamps_counts[data_columns].resample('min').mean()
timestamps_counts_mean.head(3)

# Start and end of the date range to extract
start, end = '2021-07-07', '2021-07-07'
# Plot daily and weekly resampled time series together
fig, ax = plt.subplots()
ax.plot(timestamps_counts.loc[start:end, 'counts'], marker='', linestyle='-', linewidth=0.5, label='by seconds')
ax.plot(timestamps_counts_mean.loc[start:end, 'counts'], marker='o', markersize=8, linestyle='-', label='Minute Mean Resample')
ax.set_ylabel('number of people')
ax.legend()


################################################
##### SPATIAL PLOT #############################(

# ## plot all data
# sn.set_style('whitegrid')
# sn.lmplot('CLOSE_X','CLOSE_Y',data = DF_people,
#        palette=' blue',size=10,fit_reg=False)
# sn.lmplot('X','Y',data = DF_people,
#        palette='red',size=10,fit_reg=False)
# plt.show()


## https://stackoverflow.com/questions/14827650/pyplot-scatter-plot-marker-size

'''
fig = plt.figure()
ax1 = fig.add_subplot(111)
plt.scatter(DF_people.CLOSE_X, DF_people.CLOSE_Y, c='b', marker='s',s=30, label='CLOSE_people')
plt.scatter(DF_people.X, DF_people.Y, c='r', marker='+', s=(10*72./fig.dpi)**2, label='central_people')
plt.legend(loc='upper left')
plt.show()
'''

fig = plt.figure()
ax1 = fig.add_subplot(111)
### make a loop to visualize time series
for idx, timestamp in enumerate(LIST_TIMESTAMPS[0:2000]):
    # fig = plt.figure()
    # ax1 = fig.add_subplot(111)
    ## filter data by selected 'timestamp'
    selected_DF_people = DF_people[DF_people.timedate == timestamp]
    # print(selected_DF_people)
    plt.scatter(selected_DF_people.X, selected_DF_people.Y, c='blue', marker='+', s=(10 * 72. / fig.dpi) ** 2, label='central_people')
    plt.scatter(selected_DF_people.CLOSE_X, selected_DF_people.CLOSE_Y, c='gray', marker='s', s=15, label='CLOSE_people', alpha=0.4)
plt.show()




############################################################################
############################################################################
############################################################################
############################################################################
####### OLD scripts ########################################################

## get al keys...
keys = Altran.keys()


for key in Altran.keys():
    track = Altran.get(key)
    print(track)


# if two lists of the adjacency list are identical, then take only the last one...

for key, value in Altran.items():
    Altran[key] = value
    print(key, value)


df = pd.DataFrame(list(Altran.items()),columns = ['column1','column2'])

people = Altran.get('detectedPeople')
for person, attr in people.items():
    print(person)
    print(attr["boundingBox"])
    print(attr["closeId"])
    print(attr["detectionConfidence"])


timestamp = Altran.get('timestamp')

for timestamp in [ Altran.get('timestamp') ]:
    print(timestamp)

for detectedpeople in Altran.get('detectedPeople'):
    print(detectedpeople)

## ge out some values (bound boxes)
for timestamp in [Altran.get('timestamp')]:
    print(timestamp)


##########################################################
#### bounding box of the image ###########################

from photutils.aperture import BoundingBox
from matplotlib import patches, text, patheffects
import matplotlib.patches as pac


## get bounding box assigned to each person
people = Altran.get('detectedPeople')
for person, attr in people.items():
       boundingBox = attr["boundingBox"]
       bbox = BoundingBox(boundingBox[0], boundingBox[1], boundingBox[2], boundingBox[3])
       print(bbox)
       print('shape:', bbox.shape)
       print('extent:', bbox.extent)
       print('center:', bbox.center)
       plt.axes()
       rectangle = pac.Rectangle((boundingBox[0], 0), bbox.shape[1], bbox.shape[0], fill=False, edgecolor='red', lw=2)
       plt.gca().add_patch(rectangle)
       plt.axis('scaled')
       plt.show()
       # plt.close()



