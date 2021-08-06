
import os
os.chdir('D:/Federico/CityFLows')
# os.chdir(('C:\\Users\\Federico\\ownCloud\\CityFLows'))
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
# os.chdir('C:\\Users\\Federico\\ownCloud\\CityFLows')
os.getcwd()



## read sample json from Altran
with open('sample2.json') as json_data:
    Altran = json.load(json_data)
people = Altran.get('detectedPeople')

########################################################################################
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
# file = open("zzz_report.txt")
for x in people:
    print(x)
    i = i+1
    print(i)
    ## transform into a string
    x = str(x)
    f = x.split(",")
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
## save dato to .csv
DF_people[['timedate', 'ID', 'X','Y', 'CLOSE_ID', 'CLOSE_X', 'CLOSE_Y', 'DIST']].to_csv('DF_people_ALTRAN_07July2021.txt')

type(DF_people)
# DF_people.dtypes



###########################################################################
#####------- plot TIMESERIES of people COUNTS ----- #######################
############# CENTRAL PEOPLE ID ###########################################

### https://www.dataquest.io/blog/tutorial-time-series-analysis-with-pandas/
### get number of counts for each timestamp
DF = DF_people.drop_duplicates(['timedate', 'ID'],keep= 'last')
timestamps_counts = DF.groupby(DF[['timedate']].columns.tolist(), sort=False).size().reset_index().rename(columns={0:'counts'})
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

fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)
plt.scatter(DF_people.X, DF_people.Y, c='blue', marker='+', s=(10 * 72. / fig.dpi) ** 2, label='central_people')
# plt.scatter(DF_people.CLOSE_X, DF_people.CLOSE_Y, c='red', marker='s', s=15, label='CLOSE_people', alpha=0.2)
plt.legend(loc='upper left')
plt.grid(color='gray', linestyle='-', linewidth=1)
plt.show()
plt.savefig("map_CENTRAL_ID_ALTRAN.png")
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


#################################################################################################
#### ------------------- DRAW TRAJECTORIES ---------------------#################################
#################################################################################################


### groupby ID and get counts for each ID
ID_counts = DF.groupby(DF[['ID']].columns.tolist(), sort=False).size().reset_index().rename(columns={0:'counts'})
## summary statistics
ID_counts.describe()

fig = plt.hist(ID_counts['counts'], bins= 200)   # bins = 24, color = 'gold'
plt.title('Distribution of Counts (for all IDs)')
plt.xlabel("counts")
plt.ylabel("Frequency")
plt.savefig("counts_all_IDs.png")
plt.close()

## filter ID with counts < 15
ID_counts = ID_counts[(ID_counts.counts <= 10)  & (ID_counts.counts >= 5)]

### --- PLOT all TRAJECTORIES ---- ####################################

fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)
# LIST_IDs = list(DF.ID.unique())
LIST_IDs = list(ID_counts.ID.unique())

for idx, id in enumerate(LIST_IDs):
    # print(id)
    selected_ID = DF[['timedate', 'ID', 'X', 'Y']][DF.ID == id]
    ## remove duplicated with same X & Y
    selected_ID = selected_ID.drop_duplicates(['X', 'Y'], keep='last')
    # sort by 'timedate'
    selected_ID.sort_values(['timedate'], ascending=True, inplace=True)
    ## reset index
    selected_ID = selected_ID.reset_index()
    len_ID = len(selected_ID)
    # print(len_ID)
    if len_ID >= 2:  #32
        selected_ID.plot('X','Y',ax=ax1, legend=None, linestyle='solid', linewidth=2)
        print(id)
plt.savefig("trajectories_ALL_ID_ALTRAN.png")
plt.close()


### --- highlight ORIGIN and DESTINATION for each trajectory -- ####################

fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)


for idx, id in enumerate(LIST_IDs):
    # print(id)
    selected_ID = DF[['timedate', 'ID', 'X', 'Y']][DF.ID == id]
    ## remove duplicated with same X & Y
    selected_ID = selected_ID.drop_duplicates(['X', 'Y'], keep='last')
    # sort by 'timedate'
    selected_ID.sort_values(['timedate'], ascending=True, inplace=True)
    ## reset index
    selected_ID = selected_ID.reset_index()
    len_ID = len(selected_ID)
    # print(len_ID)
    if len_ID >= 3:  #32
        selected_ID.plot('X','Y',ax=ax1, legend=None, linestyle='dotted', linewidth=0.8)  # color='red'  linestyle='dotted'
        print(id)
        for k, v in selected_ID[['X','Y']].iterrows():
            if (k == 0):
                # print(k, v)
                ax1.annotate('' + 'O', v, fontsize=10, color = 'red')  ### ORIGIN of the trajectory
            if (k ==len_ID-1):
                ax1.annotate('' + 'D', v, fontsize=10, color = 'blue')  ### DESTINATION of the trajectory

plt.savefig("trajectories_OD_ID_ALTRAN.png")
plt.close()



#### LONG TRAJECTORIES ########

fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)
LIST_IDs = list(DF.ID.unique())

for idx, id in enumerate(LIST_IDs):
    # print(id)
    selected_ID = DF[['timedate', 'ID', 'X', 'Y']][DF.ID == id]
    ## remove duplicated with same X & Y
    selected_ID = selected_ID.drop_duplicates(['X', 'Y'], keep='last')
    # sort by 'timedate'
    selected_ID.sort_values(['timedate'], ascending=True, inplace=True)
    ## reset index
    selected_ID = selected_ID.reset_index()
    len_ID = len(selected_ID)
    # print(len_ID)
    if len_ID > 32:
        selected_ID.plot('X','Y',ax=ax1, legend=None, linestyle='solid', linewidth=1)  # color='red'  linestyle='dotted'
        print(id)
        for k, v in selected_ID[['X','Y']].iterrows():
            if (k == 0):
                # print(k, v)
                ax1.annotate('' + 'O', v, fontsize=10, color = 'red')  ### ORIGIN of the trajectory
            if (k ==len_ID-1):
                ax1.annotate('' + 'D', v, fontsize=10, color = 'blue')  ### DESTINATION of the trajectory
plt.savefig("trajectories_LONG_PATHs_ALTRAN.png")
plt.close()


# for k, v in selected_DF[['X','Y']].iterrows():
# ax.annotate(''+str(k+1), v)
#    plt.show()

## ---- clustering ---------- #########################
## k-means partitions a list of observations into k clusters, where each point belongs to the cluster with the nearest mean

# import required modules
import numpy as np
from scipy.cluster.vq import kmeans2
from sklearn.preprocessing import MinMaxScaler
from sklearn.cluster import KMeans


### --- determine the number of clusters --- ########################################################################
### use the INERTIA_ATTRIBUTE  method

### https://blog.cambridgespark.com/how-to-determine-the-optimal-number-of-clusters-for-k-means-clustering-14f27070048f
## https://towardsdatascience.com/cheat-sheet-to-implementing-7-methods-for-selecting-optimal-number-of-clusters-in-python-898241e1d6ad
## https://jakevdp.github.io/PythonDataScienceHandbook/05.11-k-means.html

#### normalize data.....

mms = MinMaxScaler()
mms.fit(DF[['X', 'Y']])
data_transformed = mms.transform(DF[['X', 'Y']])

## ....basically For each k value, we will initialise k-means and use the inertia attribute to identify
## the sum of squared distances of samples to the nearest cluster centre.

Sum_of_squared_distances = []
K = range(1,15)
for k in K:
    km = KMeans(n_clusters=k)
    km = km.fit(data_transformed)
    Sum_of_squared_distances.append(km.inertia_)

## the ELBOW of the plot represent the OPTIMAL number of CLUSTERs

plt.plot(K, Sum_of_squared_distances, 'bx-')
plt.xlabel('k')
plt.ylabel('Sum of squared distances')
plt.title('Elbow Method for Optimal number of Clusters')
plt.show()
plt.savefig("cluster_optimization_ALTRAN.png")
plt.close()


#################################################################
####------ CLUSTERING the points using k-means ---###############
#################################################################


points = DF[['X', 'Y']].to_numpy(dtype='float32')

# plt.scatter(DF.X, DF.Y, s=5)

from sklearn.cluster import KMeans
kmeans = KMeans(n_clusters=6)  ### use the number of clusters found above
kmeans.fit(points)
y_kmeans = kmeans.predict(points)


fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)
plt.scatter(DF.X, DF.Y, c=y_kmeans, s=5, cmap='viridis', label='central_people')

centroids = kmeans.cluster_centers_
plt.scatter(centroids[:, 0], centroids[:, 1], c='blue', s=200, alpha=0.6, label='cluster centroid')
plt.legend(loc='upper left')
plt.grid(color='gray', linestyle='-', linewidth=1)
plt.savefig("map_CLUSTERING_CENTRAL_ID_ALTRAN.png")
plt.close()


##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################

###----- convert lat/lon to UTM (metric system) zone of Milano 32T ---######
from pyproj import Proj
import pyproj

## define projection zone...
## from EPSD:4326 ----> EPSG: 32632
myProj = pyproj.Proj(proj='utm', zone=32, ellps='WGS84')
ST_X,ST_Y = myProj(9.203477137061604, 45.4850803014196)
print(ST_X,ST_Y)
### go back to degrees..(from metric to degrees)

print(myProj(ST_X,ST_Y,inverse=True))

### convert all the X,Y points in real metric coordinates
### !!! transform all cordinates into meters (they are in cm)
DF['X'] = DF[['X']]/10 + ST_X
DF['Y'] = DF[['Y']]/10 + ST_Y

## make two empty lists for the X,Y coordnates in degrees
transformed_X = []
transformed_Y = []
### convert all the metric coordinates into degrees
zipped_X_Y = zip(list(DF['X']), list(DF['Y']))
for (x, y) in zipped_X_Y:
    print(x,y)
    transformed = myProj(x, y, inverse=True)
    transformed_X.append(transformed[0])
    transformed_Y.append(transformed[1])

## replace metric coordinates with degree coordinates
DF['X'] = transformed_X
DF['Y'] = transformed_Y



##### define distance between two coordinates
def haversine(coord1, coord2):
    # Coordinates in decimal degrees (e.g. 43.60, -79.49)
    lon1, lat1 = coord1
    lon2, lat2 = coord2
    R = 6371000  # radius of Earth in meters
    phi_1 = np.radians(lat1)
    phi_2 = np.radians(lat2)
    delta_phi = np.radians(lat2 - lat1)
    delta_lambda = np.radians(lon2 - lon1)
    a = np.sin(delta_phi / 2.0) ** 2 + np.cos(phi_1) * np.cos(phi_2) * np.sin(delta_lambda / 2.0) ** 2
    c = 2 * np.arctan2(np.sqrt(a), np.sqrt(1 - a))
    meters = R * c  # output distance in meters
    km = meters / 1000.0  # output distance in kilometers
    meters = round(meters)
    km = round(km, 3)
    # print(f"Distance: {meters} m")
    # print(f"Distance: {km} km")
    return meters


# define the extension of the EDGE file
xmin, ymin, xmax, ymax = min(DF.X), min(DF.Y), max(DF.X), max(DF.Y)
# East-West extent
EW = haversine((xmin, ymin), (xmax, ymin))
# North-South extent
NS = haversine((xmin, ymin), (xmin, ymax))
# diamter of each hexagon in the grid = 900 metres
d = 1  # 1 meter
# horizontal width of hexagon = w = d* sin(60)
w = d * np.sin(np.pi / 3)
# Approximate number of hexagons per row = EW/w
n_cols = int(EW / w) + 1
# Approximate number of hexagons per column = NS/d
n_rows = int(NS / d) + 10

# Make a hexagonal grid to cover the entire area
from matplotlib.patches import RegularPolygon

w = (xmax - xmin) / n_cols  # width of hexagon
d = w / np.sin(np.pi / 3)  # diameter of hexagon
array_of_hexes = []
for rows in range(0, n_rows):
    hcoord = np.arange(xmin, xmax, w) + (rows % 2) * w / 2
    vcoord = [ymax - rows * d * 0.75] * n_cols
    for x, y in zip(hcoord, vcoord):  # , colors):
        hexes = RegularPolygon((x, y), numVertices=6, radius=d / 2, alpha=0.2, edgecolor='k')
        verts = hexes.get_path().vertices
        trans = hexes.get_patch_transform()
        points = trans.transform(verts)
        array_of_hexes.append(Polygon(points))

hex_grid = gpd.GeoDataFrame({'geometry': array_of_hexes}, crs={'init': 'epsg:4326'})
# hex_grid.plot()


###--- make a geodataframe with all ther IDs points ---######
array_of_points = []
## transform all cordinates into meters (they are in cm)
for x, y in zip(DF.X, DF.Y):
    points = Point((x, y))
    array_of_points.append(points)

### make a goedataframe containing the POINTS
points_gdf = gpd.GeoDataFrame(array_of_points,columns=['geometry'])
points_gdf.plot()
points_gdf.geometry.to_file(filename='ID_POINTS.geojson', driver='GeoJSON')


## ---- INTERSECT all POINTS with the HEXAGONAL GRID ----- ####

# get hexagons containing the points
points_hex = gpd.sjoin(hex_grid, points_gdf, how='inner', op='intersects')
points_hex.plot()

points_hex.geometry.to_file(filename='points_hex.geojson', driver='GeoJSON')
hex_grid.geometry.to_file(filename='hex_grid_10m.geojson', driver='GeoJSON')

### reset indexes....
points_hex.reset_index(level=0, inplace=True)
hex_grid.reset_index(inplace=True)

style_hex = {'fillColor': '#00000000', 'color': '#00FFFFFF'}
style_edges = {'fillColor': '#00000000', 'color': '#000000'}
style_egdes_hex = {'fillColor': '#00000000', 'color': '#ff0000'}


#############################################################################################
# create basemap around Catania
ave_LAT = 45.4850803014196
ave_LON = 9.203477137061604
my_map = folium.Map([ave_LAT, ave_LON], zoom_start=28, tiles='cartodbpositron')

## add ID points
# folium.GeoJson('ID_POINTS.geojson', style_function=lambda x: style_edges).add_to((my_map))

# add all ID points as highligths for each point (in blue)
folium.GeoJson(
    # data to plot
    hex_grid[['index', 'geometry']].to_json(),
    show=True,
    style_function=lambda x: style_hex,
    highlight_function=lambda x: {'weight': 1,
                                  'color': 'yellow',
                                  'fillOpacity': 0.2
                                  },
    # fields to show
    tooltip=folium.features.GeoJsonTooltip(
        fields=['index']
    ),
).add_to(my_map)


folium.GeoJson(
    # data to plot
    points_hex[['index', 'geometry']].to_json(),
    show=True,
    style_function=lambda x: style_egdes_hex,
    highlight_function=lambda x: {'weight': 1,
                                  'color': 'yellow',
                                  'fillOpacity': 0.2
                                  },
    # fields to show
    tooltip=folium.features.GeoJsonTooltip(
        fields=['index']
    ),
).add_to(my_map)

my_map.save("hex_grid_1m.html")



##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
#### --- divide the study area with hexagonal grids ---###########################

# define the EXTENSION of the STUDY AREA file
xmin, ymin, xmax, ymax = min(DF.X), min(DF.Y), max(DF.X), max(DF.Y)
## transform all cordinates into meters (they are in cm)
xmin, ymin, xmax, ymax = xmin/10, ymin/10, xmax/10, ymax/10
# East-West extent of the study area
# EW = math.hypot(xmin - ymin, xmax - ymin)
EW = xmax - xmin
# North-South extent of the study area
# NS = math.hypot(xmin - ymin, xmin - ymax)
NS = ymax-ymin
# diamter of each hexagon in the grid = 900 metres
d = 1  ## meters
# horizontal width of hexagon = w = d* sin(60)
w = d * np.sin(np.pi / 3)
# Approximate number of hexagons per row = EW/w
n_cols = int(EW / w) + 1
# Approximate number of hexagons per column = NS/d
n_rows = int(NS / d) + 10

# Make a hexagonal grid to cover the entire area
from matplotlib.patches import RegularPolygon
import matplotlib.patches as pac

fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)
w = (xmax - xmin) / n_cols  # width of hexagon
d = w / np.sin(np.pi / 3)  # diameter of hexagon
array_of_hexes = []
for rows in range(0, n_rows):
    hcoord = np.arange(xmin, xmax, w) + (rows % 2) * w / 2
    vcoord = [ymax - rows * d * 0.75]  * n_cols
    # print(vcoord)
    for x, y in zip(hcoord, vcoord):  # , colors):
        hexes = RegularPolygon((x, y), numVertices=6, radius=d / 2, alpha=0.2, edgecolor='k')
        verts = hexes.get_path().vertices
        trans = hexes.get_patch_transform()
        points = trans.transform(verts)
        array_of_hexes.append(Polygon(points))
        ax1.add_patch(hexes)
plt.axis('scaled')
ax1.set_xlim([xmin, xmax])
ax1.set_ylim([ymin, ymax])


'''
fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)
rectangle = pac.Rectangle((xmin,ymin), xmax, ymax,ec="red", fill=False, edgecolor='red')
ax1.set_xlim([xmin, xmax])
ax1.set_ylim([ymin, ymax])
ax1.add_patch(rectangle)
plt.axis('scaled')
'''

###--- make a geodataframe with all ther IDs points ---######
array_of_points = []
## transform all cordinates into meters (they are in cm)
df_X = (DF.X)/10
df_Y = (DF.Y)/10
for x, y in zip(df_X, df_Y):
    points = Point((x, y))
    array_of_points.append(points)

### make a goedataframe containing the POINTS
points_gdf = gpd.GeoDataFrame(array_of_points,columns=['geometry'])
points_gdf.plot()
points_gdf.geometry.to_file(filename='ID_POINTS.geojson', driver='GeoJSON')

### make a geodataframe
hex_grid = gpd.GeoDataFrame(array_of_hexes,columns=['geometry'])
hex_grid.plot()

## ---- INTERSECT all POINTS with the HEXAGONAL GRID ----- ####

# get hexagons containing the points
points_hex = gpd.sjoin(hex_grid, points_gdf, how='inner', op='intersects')
points_hex.plot()

points_hex.geometry.to_file(filename='points_hex.geojson', driver='GeoJSON')
hex_grid.geometry.to_file(filename='hex_grid_10m.geojson', driver='GeoJSON')

### reset all indices
points_hex.reset_index(level=0, inplace=True)
hex_grid.reset_index(inplace=True)

style_hex = {'fillColor': '#00000000', 'color': '#00FFFFFF'}
style_edges = {'fillColor': '#00000000', 'color': '#000000'}
style_egdes_hex = {'fillColor': '#00000000', 'color': '#ff0000'}


#############################################################################################
# create basemap around Catania
ave_LAT = 37.53988692816245
ave_LON = 15.044971594798902
my_map = folium.Map([ave_LAT, ave_LON], zoom_start=11, tiles='cartodbpositron')

## ---- add ID points to the basemap ---#########
#################################################

for i in range(len(df_X)):
    folium.CircleMarker(location=[df_Y.iloc[i], df_X.iloc[i]],
                        radius=1,
                        color="black",
                        fill=True,
                        fill_color="black",
                        fill_opacity=1).add_to(my_map)

# add 'u' and 'v' as highligths for each edge (in blue)
folium.GeoJson(
    # data to plot
    hex_grid[['index', 'geometry']].to_json(),
    show=True,
    style_function=lambda x: style_hex,
    highlight_function=lambda x: {'weight': 1,
                                  'color': 'yellow',
                                  'fillOpacity': 0.2
                                  },
    # fields to show
    tooltip=folium.features.GeoJsonTooltip(
        fields=['index']
    ),
).add_to(my_map)


folium.GeoJson(
    # data to plot
    points_hex[['index', 'geometry']].to_json(),
    show=True,
    style_function=lambda x: style_egdes_hex,
    highlight_function=lambda x: {'weight': 1,
                                  'color': 'yellow',
                                  'fillOpacity': 0.2
                                  },
    # fields to show
    tooltip=folium.features.GeoJsonTooltip(
        fields=['index']
    ),
).add_to(my_map)

my_map.save("hex_grid_1m.html")


