

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
import itertools
import glob

#### VORONOI functions
import contextily as ctx
import matplotlib.pyplot as plt
from shapely.ops import cascaded_union
import json
import datetime
import seaborn as sn
import requests

from geovoronoi.plotting import subplot_for_map, plot_voronoi_polys_with_points_in_area
from geovoronoi import voronoi_regions_from_coords, points_to_coords

input_dir = 'D:/Federico/CityFLows/input_files/'
output_dir = 'D:/Federico/CityFLows/output_files/'
os.chdir('D:/Federico/CityFLows')
# os.chdir('C:\\Users\\Federico\\ownCloud\\CityFLows')
os.getcwd()


######################################################
###--------- load bulk of files ----##################
######################################################

## build an empty DataFram to fill with all the single dataframe frm the output files "DF_people"
all_PEOPLE = pd.DataFrame([])

### upload Viasat data for the month of November 2019
extension = 'json'
# os.chdir('D:/Federico/CityFLows/new_input_files')

###---> all input files from Piazza Duca D'Aosta Milano Centrale Camera 3
# os.chdir('D:\\Federico\\CityFLows\\new_input_files\\UEFA_event_milano\\Camera3_DUCA_AOSTA\\xxxx')
os.chdir('D:\\Federico\\CityFLows\\new_input_files\\UEFA_event_milano\\Camera3_DUCA_AOSTA\\new_elaborations')

###---> all input files from Lato Taxi Milano Centrale Camera 2
# os.chdir('D:\\Federico\\CityFLows\\new_input_files\\UEFA_event_milano\\Camera2_Lato_Taxi\\xxxx')
# os.chdir('D:\\Federico\\CityFLows\\new_input_files\\UEFA_event_milano\\Camera2_Lato_Taxi\\new_elaborations')

###---> all input files from Lato Taxi Milano Centrale Camera 1
# os.chdir('D:\\Federico\\CityFLows\\new_input_files\\UEFA_event_milano\\Camera1_Lato_Biciclette\\xxxx')


## list all JSON files in the folder
filenames = glob.glob('*.{}'.format(extension))
# filenames = ['2021_09_29_16_02.json']

## loop over all the .csv file with the raw VIASAT data
for json_file in filenames:
    print(json_file)
    ## read sample json from Altran
    with open(json_file) as json_data:
        Altran = json.load(json_data)
    people = Altran.get('report')
    ts_start = Altran.get('ts_start')
    ts_to = Altran.get('ts_to')
    ## get ID Camera and its coordinates
    sensor = Altran.get('sensor')
    gps_lat = Altran.get('gps_lat')
    gps_lon = Altran.get('gps_lon')
    ## get the link to the report
    url_report = Altran.get('link')
    # print(people)

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
        first_level = x.split("frame")
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
        second_level = first_level[2]
        detectedpeople = second_level.split('id_persona')
        ## get ID persona
        people_ID = [s for s in detectedpeople if 'Cx_2D_cm' in s]
        # people_ID = [s for s in detectedpeople if 'Cx3D_px' in s]
        # if i == 13:
        #     break

        ## get all ID...of the central point...
        for idx, element in enumerate(people_ID):
            if idx > 0:
                print('INPUT ---> index_people_ID:', idx, element)
                print(idx, element)
                central_ID = element.split("':")[1]
                central_ID = int(central_ID.split(',')[0])
                print('central_ID:', central_ID)
                central_ID_x = int((element.split(", 'Cx_2D_cm':")[1]).split(',')[0])
                # central_ID_x = int((element.split(", 'Cx3D_px':")[1]).split(',')[0])
                print('central_ID_x:', central_ID_x)
                central_ID_y = int((element.split(", 'Cy_2D_cm':")[1]).split(',')[0])
                # central_ID_y = int((element.split(", 'Cy3D_px':")[1]).split('}')[0])
                print('central_ID_y:', central_ID_y)
                ## BUILD list of (X,Y)
                ID.append(central_ID)
                ID_X.append(central_ID_x)
                ID_Y.append(central_ID_y)
                timedate.append(TS)
                ## build a DICTIONARY with ID, x, Y for each ID
                list_ID = [central_ID_x, central_ID_y]
                coords_dict[central_ID] = list_ID
        for idx, element in enumerate(people_ID):
            if idx > 0:
                central_ID = element.split("':")[1]
                central_ID = int(central_ID.split(',')[0])

    ##-----build dataframe for CENTRAL points ('ID persona')-----#######
    list_of_X_Y = list(zip(ID_X, ID_Y))
    DF_people = pd.DataFrame(list_of_X_Y, columns=['X', 'Y'])
    DF_people['timedate'] = timedate
    DF_people['ID'] = ID
    ## order columns...
    DF_people = DF_people[['timedate', 'ID', 'X', 'Y']]
    # sort by 'timedate'
    DF_people.sort_values(['timedate'],ascending=True, inplace=True)


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
    all_PEOPLE = all_PEOPLE.append(DF_people)
    ###---> drop duplicates
    all_PEOPLE = all_PEOPLE.drop_duplicates(['timedate', 'ID', 'X', 'Y'], keep='last')

###---> drop duplicates
DF = all_PEOPLE.drop_duplicates(['timedate', 'ID', 'X', 'Y'],keep= 'last')

min_DF_people = DF[(DF.date == min(DF['date']))]
max_DF_people = DF[(DF.date == max(DF['date']))]

## save dato to .csv
DF[['timedate', 'ID', 'X','Y']].to_csv(output_dir + sensor.replace(" ", "") + '_from_' +
                                               min(min_DF_people.date) + '__' + str(min(min_DF_people.hour)) + 'h' +
                                               str(min(min_DF_people.minute)) + 'min_to_' +
                                               max(max_DF_people.date) + '__' + str(max(max_DF_people.hour)) + 'h' +
                                               str(max(max_DF_people.minute)) + 'min.txt')


######################################################################################################################################################
#######----------------------------------------------------------------------------------------------------###########################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################

###########################################################################
#####------- plot TIMESERIES of people COUNTS ----- #######################
############# CENTRAL PEOPLE ID ###########################################

del(all_PEOPLE)

### https://www.dataquest.io/blog/tutorial-time-series-analysis-with-pandas/
### get number of counts for each timestamp

'''
## Milano Centrale CAMERA 3
DF_a = pd.read_csv(output_dir + "DF_people_ALTRAN_MILANOCENTRALE3_2021-10-06_from_14_00_to_19_40.txt")
DF_b = pd.read_csv(output_dir + "DF_people_ALTRAN_MILANOCENTRALE3_2021-10-06_from_17_00_to_22_40.txt")
DF_c = pd.read_csv(output_dir + "DF_people_ALTRAN_MILANOCENTRALE3_2021-10-07_from_22_40_to_09_40.txt")
DF_d = pd.read_csv(output_dir + "DF_people_ALTRAN_MILANOCENTRALE3_2021-10-07_from_05_10_to_10_30.txt")
DF_e = pd.read_csv(output_dir + "DF_people_ALTRAN_MILANOCENTRALE3_2021-10-07_from_13_10_to_18_40.txt")
## concatenate files
DF = pd.concat([DF_a, DF_b, DF_c, DF_d, DF_e])
'''

##---> load data from Milano Centrale 3, Piazza Duca d'Aosta
# DF = pd.read_csv(output_dir + "MILANOCENTRALE3_from_2021-10-06__14h0min_to_2021-10-12__23h59min.txt")
# DF = pd.read_csv(output_dir + "MILANO_3_from_2021-10-15__3h0min_to_2021-10-21__12h59min.txt")
DF = pd.read_csv(output_dir + "MILANO_3_from_2021-10-10__17h0min_to_2021-10-10__18h59min.txt")
##---> load data from Milano Centrale 2, Lato Taxi
# DF = pd.read_csv(output_dir + "MILANOCENTRALE2_from_2021-10-06__13h0min_to_2021-10-12__22h41min.txt")
# DF = pd.read_csv(output_dir + "MILANO_2_TAXI_from_2021-10-15__16h0min_to_2021-10-21__0h30min.txt")
##---> load data from Milano Centrale 1, Lato Biciclette
# DF = pd.read_csv(output_dir + "MILANOCENTRALE1_from_2021-10-06__14h0min_to_2021-10-12__23h59min.txt")
# DF = pd.read_csv(output_dir + "MILANO_1_BICI_from_2021-10-15__3h0min_to_2021-10-21__12h59min.txt")


## sort by 'timedate'
DF.sort_values(['timedate'], ascending=True, inplace=True)
# DF = DF_people.drop_duplicates(['timedate', 'ID'],keep= 'last')
DF = DF.drop_duplicates(['timedate', 'ID', 'X', 'Y'],keep= 'last')

timestamps_counts = DF.groupby(DF[['timedate']].columns.tolist(), sort=False).size().reset_index().rename(columns={0:'counts'})
# timestamps_counts[(timestamps_counts.counts == max(timestamps_counts['counts']))]

outlier = max(timestamps_counts['counts'])
timestamps_counts = timestamps_counts[timestamps_counts.counts < outlier]


## timestamps with more registrations
max_timestamp = timestamps_counts['timedate'][timestamps_counts.counts == max(timestamps_counts.counts)]
timestamps_counts.drop_duplicates(['timedate'], inplace=True)
LIST_TIMESTAMPS = list(timestamps_counts.timedate.unique())

# timestamps_counts = DF_people.groupby(DF_people[['timedate', 'date', 'hour', 'minute', 'seconds']].columns.tolist(), sort=False).size().reset_index().rename(columns={0:'counts'})
DF['timedate'] = DF['timedate'].astype('datetime64[ns]')
## define "date" only
DF['date'] = DF['timedate'].apply(lambda x: x.strftime("%Y-%m-%d"))
# define "hour"
DF['hour'] = DF['timedate'].apply(lambda x: x.hour)
# define minutes
DF['minute'] = DF['timedate'].apply(lambda x: x.minute)
# define seconds
DF['seconds'] = DF['timedate'].apply(lambda x: x.second)
timestamps_counts = DF.groupby(DF[['timedate', 'date', 'hour', 'minute', 'seconds']].columns.tolist(), sort=False).size().reset_index().rename(columns={0:'counts'})
timestamps_counts = timestamps_counts[timestamps_counts.counts < 50]

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
start_date, end_date = min(DF['date']), max(DF['date'] )
# Plot daily and weekly resampled time series together
fig = plt.figure(figsize=(10,8))
ax = fig.add_subplot(111)
ax.plot(timestamps_counts.loc[start_date:end_date, 'counts'], marker='', linestyle='dotted', linewidth=0.3, label='by timestamp',  color='gray')
# ax.plot(timestamps_counts_SECs_mean.loc[start:end, 'counts'], marker='', linestyle='-', linewidth=0.5, label='by timestamp',  color='gray')
ax.plot(timestamps_counts_MIN_mean.loc[start_date:end_date, 'counts'], marker='o', markersize=4, linestyle='solid', linewidth=0.5, label='minute mean resample', color='red')
ax.set_ylabel('number of people')
ax.legend()
ax.title.set_text('timeseries IDs and CLOSE IDs' + start_date)
plt.savefig(output_dir + "timeseries_people_MILANO_CENTRALE3_DUCA_AOSTA_17h20_to18h20-10Oct2021.png")
# plt.savefig(output_dir + "timeseries_people_MILANO_CENTRALE2_LATO_TAXI.png")
# plt.savefig(output_dir + "timeseries_people_MILANO_CENTRALE2_LATO_BICICLETTE.png")
plt.close()

################################################
##### SPATIAL PLOT #############################

## https://stackoverflow.com/questions/14827650/pyplot-scatter-plot-marker-size

fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)
plt.scatter(DF.X, DF.Y, c='blue', marker='+', s=(10 * 72. / fig.dpi) ** 2, label='central_people')
plt.legend(loc='upper left')
plt.grid(color='gray', linestyle='-', linewidth=1)
# flip y axis
# plt.gca().invert_yaxis()
# plt.show()
plt.savefig(output_dir + "map_CENTRAL_ID_ALTRAN" + start_date + ".png")
plt.close()


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
plt.savefig(output_dir + "counts_all_IDs.png")
plt.close()

## filter ID with counts < 15
# ID_counts = ID_counts[(ID_counts.counts <= 10)  & (ID_counts.counts >= 5)]

### --- PLOT all TRAJECTORIES ---- ####################################

''' 

fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)
# plt.gca().invert_yaxis()

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
    if len_ID >= 2:  
        selected_ID.plot('X','Y',ax=ax1, legend=None, linestyle='solid', linewidth=2)
        print(id)
plt.savefig("trajectories_Lato_Biciclette.png")
plt.close()

'''

### --- highlight ORIGIN and DESTINATION for each trajectory -- ####################

# LIST_IDs = list(DF.ID.unique())
LIST_IDs = list(ID_counts.ID.unique())

fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)
# flip y axis
# plt.gca().invert_yaxis()


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
    print(len_ID)  ## how many times I see the ID
    if len_ID >= 0:
        selected_ID.plot('X','Y',ax=ax1, legend=None, linestyle='dotted', linewidth=0.8)  # color='red'  linestyle='dotted'
        print(id)
        for k, v in selected_ID[['X','Y']].iterrows():
            if (k == 0):
                # print(k, v)
                ax1.annotate('' + 'O', v, fontsize=10, color = 'red')  ### ORIGIN of the trajectory
            if (k ==len_ID-1):
                ax1.annotate('' + 'D', v, fontsize=10, color = 'blue')  ### DESTINATION of the trajectory

plt.savefig(output_dir + "trajectories_OD_ID_Lato_Biciclette.png")
plt.close()


'''

#### LONG TRAJECTORIES ########

fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)
## flip y axis
# plt.gca().invert_yaxis()
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
    if len_ID > 10:
        selected_ID.plot('X','Y',ax=ax1, legend=None, linestyle='solid', linewidth=1)  # color='red'  linestyle='dotted'
        print(id)
        for k, v in selected_ID[['X','Y']].iterrows():
            if (k == 0):
                # print(k, v)
                ax1.annotate('' + 'O', v, fontsize=10, color = 'red')  ### ORIGIN of the trajectory
            if (k ==len_ID-1):
                ax1.annotate('' + 'D', v, fontsize=10, color = 'blue')  ### DESTINATION of the trajectory
                
plt.savefig("trajectories_LONG_PATHs_Lato_Biciclette.png")
plt.close()

'''

# for k, v in selected_DF[['X','Y']].iterrows():
# ax.annotate(''+str(k+1), v)
#    plt.show()


########################################################################################################################
########################################################################################################################
########################################################################################################################
## ---- clustering ---------- ##########################################################################################
## k-means partitions a list of observations into k clusters, where each point belongs to the cluster with the nearest mean

# import required modules
import numpy as np
from scipy.cluster.vq import kmeans2
from sklearn.preprocessing import MinMaxScaler
from sklearn.cluster import KMeans

## load speed & directions summary data:
# speed_directions = pd.read_csv(output_dir + "all_ID_speeds_lato_taxi_angles__17h20_to_18h20_10Oct2021.csv")
speed_directions = pd.read_csv(output_dir + "all_ID_speeds_duca_aosta_angles.csv")

## remove rows with NAs values in the column "speed"
DF = speed_directions[speed_directions['speed'].notna()]
## filter speed data
DF = DF[(DF['X'] >= 20) & (DF['Y'] <=80)]
DF = DF[(DF['speed'] >= 0.3) & (DF['speed'] <=2.5)]

## add hours and minutes
DF['timedate'] = DF['timedate'].astype('datetime64[ns]')
## define "date" only
DF['date'] = DF['timedate'].apply(lambda x: x.strftime("%Y-%m-%d"))
# define "hour"
DF['hour'] = DF['timedate'].apply(lambda x: x.hour)
# define minutes
DF['minute'] = DF['timedate'].apply(lambda x: x.minute)
# define seconds
DF['seconds'] = DF['timedate'].apply(lambda x: x.second)

### set time of day. morning or evening
# time_of_day = "morning"
# time_of_day = "evening"
time_of_day = "all_day"

if  time_of_day == "morning":
    DF = DF[(DF.hour >= 6) & (DF.hour < 10)]
elif time_of_day == "evening":
    DF = DF[(DF.hour >= 17) & (DF.hour < 19)]
elif time_of_day == "all_day":
    DF = DF




### --- determine the number of clusters --- ########################################################################
### use the INERTIA_ATTRIBUTE  method

### https://blog.cambridgespark.com/how-to-determine-the-optimal-number-of-clusters-for-k-means-clustering-14f27070048f
## https://towardsdatascience.com/cheat-sheet-to-implementing-7-methods-for-selecting-optimal-number-of-clusters-in-python-898241e1d6ad
## https://jakevdp.github.io/PythonDataScienceHandbook/05.11-k-means.html

#### normalize data.....

mms = MinMaxScaler()
mms.fit(DF[['X', 'Y']])
data_transformed = mms.transform(DF[['X', 'Y']])
data_transformed = np.nan_to_num(data_transformed)

## ....basically For each k value, we will initialise k-means and use the inertia attribute to identify
## the sum of squared distances of samples to the nearest cluster centre.

Sum_of_squared_distances = []
K = range(1,15)
for k in K:
    km = KMeans(n_clusters=k, random_state = 42)
    km = km.fit(data_transformed)
    Sum_of_squared_distances.append(km.inertia_)

## the ELBOW of the plot represent the OPTIMAL number of CLUSTERs

plt.plot(K, Sum_of_squared_distances, 'bx-')
plt.xlabel('k')
plt.ylabel('Sum of squared distances')
plt.title('Elbow Method for Optimal number of Clusters')
# plt.show()
plt.savefig(output_dir + time_of_day + "_cluster_optimization_all_speeds_from_06_to_24_Oct2021.png")
plt.close()


#################################################################
####------ CLUSTERING the points using k-means ---###############
#################################################################


points = DF[['X', 'Y']].to_numpy(dtype='float32')
points = np.nan_to_num(points)

# plt.scatter(DF.X, DF.Y, s=5)

from sklearn.cluster import KMeans
kmeans = KMeans(n_clusters=3, random_state = 42)  ### use the number of clusters found above
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
## flip y axis
# plt.gca().invert_yaxis()
plt.grid(color='gray', linestyle='-', linewidth=1)
plt.savefig(output_dir + time_of_day + "_map_CLUSTERING_from_06_to_24_Oct2021.png")
plt.close()

##################################################################################
########---------------------------------------###################################


###########################################################################
####------ CLUSTERING speeds and diections using k-means ---###############
###########################################################################

#############################
####---- SPEED---- ##########
#############################

#### normalize data.....
mms = MinMaxScaler()
mms.fit(DF[['speed']])
data_transformed = mms.transform(DF[['speed']])
data_transformed = np.nan_to_num(data_transformed)

## ....basically For each k value, we will initialise k-means and use the inertia attribute to identify
## the sum of squared distances of samples to the nearest cluster centre.

Sum_of_squared_distances = []
K = range(1,15)
for k in K:
    km = KMeans(n_clusters=k, random_state = 42)
    km = km.fit(data_transformed)
    Sum_of_squared_distances.append(km.inertia_)

## the ELBOW of the plot represent the OPTIMAL number of CLUSTERs

plt.plot(K, Sum_of_squared_distances, 'bx-')
plt.xlabel('k')
plt.ylabel('Sum of squared distances')
plt.title(time_of_day + '_Elbow Method for Optimal number of Clusters')
plt.close()


#################################################################
####------ CLUSTERING the points using k-means ---###############
#################################################################


points = DF[['speed']].to_numpy(dtype='float32')
points = np.nan_to_num(points)

from sklearn.cluster import KMeans
kmeans = KMeans(n_clusters=3, random_state = 42)  ### use the number of clusters found above
kmeans.fit(points)
speed_kmeans = kmeans.predict(points)


fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)
## colour points based on the owing cluster..
plt.scatter(DF.X, DF.Y, c=speed_kmeans, s=5, cmap='viridis', label='central_people')
plt.close()


## get three of clusteres speeds.....
centroids = kmeans.cluster_centers_
centroids = pd.DataFrame(centroids)
centroids['ID'] = centroids.index
centroids = centroids.rename(columns={0: 'speed'})


speed_kmeans = pd.DataFrame(speed_kmeans)
speed_kmeans = speed_kmeans.rename(columns={0: 'ID'})
speed_kmeans_counts = speed_kmeans.groupby(speed_kmeans[['ID']].columns.tolist(), sort=False).size().reset_index().rename(columns={0:'counts'})
speed_kmeans_counts = pd.merge(speed_kmeans_counts, centroids, on=['ID'], how='left')

## save dato to .csv
speed_kmeans_counts.to_csv(output_dir +  time_of_day + '_clustered_speeds_from_06_to_24_Oct2021.csv')


##################################
####---- DIRECTIONS---- ##########
##################################


mms = MinMaxScaler()
mms.fit(DF[['angle_deg']])
data_transformed = mms.transform(DF[['speed']])
data_transformed = np.nan_to_num(data_transformed)

## ....basically For each k value, we will initialise k-means and use the inertia attribute to identify
## the sum of squared distances of samples to the nearest cluster centre.

Sum_of_squared_distances = []
K = range(1,15)
for k in K:
    km = KMeans(n_clusters=k, random_state = 42)
    km = km.fit(data_transformed)
    Sum_of_squared_distances.append(km.inertia_)

## the ELBOW of the plot represent the OPTIMAL number of CLUSTERs

plt.plot(K, Sum_of_squared_distances, 'bx-')
plt.xlabel('k')
plt.ylabel('Sum of squared distances')
plt.title(time_of_day + '_Elbow Method for Optimal number of Clusters')
plt.close()


#################################################################
####------ CLUSTERING the points using k-means ---###############
#################################################################


points = DF[['angle_deg']].to_numpy(dtype='float32')
points = np.nan_to_num(points)

from sklearn.cluster import KMeans
kmeans = KMeans(n_clusters=3, random_state = 42)  ### use the number of clusters found above
kmeans.fit(points)
direction_kmeans = kmeans.predict(points)


fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(111)
ax1.patch.set_facecolor('white')
ax1.patch.set_alpha(0.5)
## colour points based on the owing cluster..
plt.scatter(DF.X, DF.Y, c=direction_kmeans, s=5, cmap='viridis', label='central_people')
plt.close()


## get three of clusteres speeds.....
centroids = kmeans.cluster_centers_
centroids = pd.DataFrame(centroids)
centroids['ID'] = centroids.index
centroids = centroids.rename(columns={0: 'angle_deg'})


direction_kmeans = pd.DataFrame(direction_kmeans)
direction_kmeans = direction_kmeans.rename(columns={0: 'ID'})
direction_kmeans_counts = speed_kmeans.groupby(direction_kmeans[['ID']].columns.tolist(), sort=False).size().reset_index().rename(columns={0:'counts'})
direction_kmeans_counts = pd.merge(direction_kmeans_counts, centroids, on=['ID'], how='left')

## save dato to .csv
direction_kmeans_counts.to_csv(output_dir + time_of_day + '_clustered_directions_from_06_to_24_Oct2021.csv')




