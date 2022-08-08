

from modelsAI import *
import utilsAI
import os
os.getcwd()
from time import sleep
# output_path = "D:/Federico/CityFLows/objectdetection/output/prova_piazza_IDs/"
# output_path = "D:/Federico/CityFLows/objectdetection/output/prova_piazza_IDs/may14_2022/"
output_path = "D:/Federico/CityFLows/objectdetection/output/prova_piazza_IDs/april23_2022/"


import os, sys, time, datetime, random
import torch
from torch.utils.data import DataLoader
from torchvision import datasets, transforms
from torch.autograd import Variable
from collections import OrderedDict
import matplotlib.pyplot as plt


import matplotlib.patches as patches
from PIL import Image
from datetime import timedelta
import pandas as pd
from keras.utils import plot_model


config_path='config/yolov3.cfg'
weights_path='config/yolov3.weights'
class_path='config/coco.names'
img_size=416
conf_thres=0.3  # 0.8    0.3
nms_thres=0.2   # 0.4    0.2


# Load model and weights
model = Darknet(config_path, img_size=img_size)
# plot_model(model, to_file="Darknet53.png", show_shapes=True)
model.load_weights(weights_path)
# model.cuda()
model.eval()
classes = utilsAI.load_classes(class_path)
# Tensor = torch.cuda.FloatTensor
Tensor = torch.FloatTensor


def detect_image(img):
    # scale and pad image
    ratio = min(img_size/img.size[0], img_size/img.size[1])
    imw = round(img.size[0] * ratio)
    imh = round(img.size[1] * ratio)
    img_transforms = transforms.Compose([ transforms.Resize((imh, imw)),
         transforms.Pad((max(int((imh-imw)/2),0), max(int((imw-imh)/2),0), max(int((imh-imw)/2),0), max(int((imw-imh)/2),0)),
                        (128,128,128)),
         transforms.ToTensor(),
         ])
    # convert image to Tensor
    image_tensor = img_transforms(img).float()
    image_tensor = image_tensor.unsqueeze_(0)
    input_img = Variable(image_tensor.type(Tensor))
    # run inference on the model and get detections
    with torch.no_grad():
        detections = model(input_img)
        detections = utilsAI.non_max_suppression(detections, 80, conf_thres, nms_thres)
    return detections [0]



## load an .mp4 video
# videopath = 'D:/Federico/CityFLows/objectdetection/input/videos_piazza_stazione_centrale/19April2022/19April_07_10am.mp4'

# figure = plt.figure()
## pip3 install opencv-python
## pip install scikit-image
import cv2
from IPython.display import clear_output

cmap = plt.get_cmap('tab20b')
colors = [cmap(i)[:3] for i in np.linspace(0, 1, 20)]

# initialize Sort object and video capture
from sort import *
# vid = cv2.VideoCapture(videopath)
## to get the tmestamp
# fps = vid.get(cv2.CAP_PROP_FPS)   # frames per seconds
# frames = vid.get(cv2.CAP_PROP_FRAME_COUNT)   # number of frames
# print(frames)
# print(fps)
# calculate duration of the video
# seconds = int(frames / fps)
# print("duration in hours:", seconds/3600)
# mot_tracker = Sort()

all_IDs = pd.DataFrame([])
time0 = pd.date_range('2022-04-23 07:00:00', periods=1)
times = time0



os.chdir('D:/Federico/CityFLows/objectdetection/input/videos_piazza_stazione_centrale/23April2022')
# os.chdir('D:/Federico/CityFLows/objectdetection/input/videos_piazza_stazione_centrale/14May2022/14May2022_14_40')

## list all .mp4 files in the folder
filenames = glob.glob('*.{}'.format('mp4'))


## loop over all the .mp4 file with the raw VIASAT data
# for mp4_file in range(len(filenames)):
for n in range(len(filenames)):
    print(n)
    ## read file
    vid = cv2.VideoCapture(filenames[n])
    ## to get the tmestamp
    fps = vid.get(cv2.CAP_PROP_FPS)  # frames per seconds
    frames = vid.get(cv2.CAP_PROP_FRAME_COUNT)  # number of frames
    # print(frames)
    # print(fps)
    mot_tracker = Sort()

    for ii in range(int(frames/1)):
    # for ii in range(int(100)):
        ID_X = []
        ID_Y = []
        ID = []
        FRAME = []
        print("---------------------------------------- frame number:", str(ii) + "_" + str(n))
        ret, frame = vid.read()
        try:
            ## get two frames each second
            if ii in range(0, int(frames), 10):
                frame = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
                pilimg = Image.fromarray(frame)
                detections = detect_image(pilimg)

                img = np.array(pilimg)
                pad_x = max(img.shape[0] - img.shape[1], 0) * (img_size / max(img.shape))
                pad_y = max(img.shape[1] - img.shape[0], 0) * (img_size / max(img.shape))
                unpad_h = img_size - pad_y
                unpad_w = img_size - pad_x

                if detections is not None:
                    tracked_objects = mot_tracker.update(detections.cpu())  ## --> multiple object tracker..
                    ## get two frames each second (write timestamp)
                    times = times + 500 * timedelta(microseconds=1000)  # 10 milli second
                    print("******************** timestamp:", times[0])
                    unique_labels = detections[:, -1].cpu().unique()
                    n_cls_preds = len(unique_labels)
                    OBJECT_CONF = []
                    CLASS_SCORE = []  # how much we can trust the object (person or car, has been correctly assigned)


                    ######################################################################################################
                    ########### ----------- procedure to calculate IoU (Intersection over Union (IOU)) -------- ##########
                    # is a metric to evaluate the object detector’s accuracy.
                    # It is calculated between two bounding boxes’ (Ground truth & Predicted bounding boxes) overlap area with union area.
                    box1 = []
                    box2 = []
                    matched = 0
                    unmatched = 0
                    ## detected persons (GROUND TRUTH) bounding boxes
                    for xx1, yy1, xx2, yy2, object_conf, class_score, class_pred in (reversed(detections.numpy())):
                        # print(round(object_conf, 2), round(class_score, 2), class_pred)
                        # print("------DETECTED------->>>>", a,b,c,d, object_conf, class_score, class_pred ,"\n")
                        ## tracked/predicted persons (GROUND TRUTH) bounding boxes
                        for x1, y1, x2, y2, obj_id, cls_pred in tracked_objects:
                            # print("-----TRACKED----->", index, x1, y1, x2, y2, obj_id)
                            # print(xx1-x1, yy1-y1, xx2-x2, yy2-y2)
                            if -0.1 <= np.average([xx1-x1, yy1-y1, xx2-x2, yy2-y2]) <= 1:
                                matched += 1
                                # print(index-1 , "--------------gotta-------------------")
                                # print(xx1 - x1, yy1 - y1, xx2 - x2, yy2 - y2)
                                # print(tracked_objects[index-1])
                                # print("box1:", x1,y1, x2, y2, ";   box2:", xx1,yy1,xx2,yy2)
                                ## put all boxes togegher......
                                box1.append([x1, y1, x2, y2])
                                box2.append([xx1, yy1, xx2, yy2])
                            else:
                                unmatched +=1
                    box1 = np.array(box1)
                    box2 = np.array(box2)
                    # print("matched: ", matched)
                    # print("unmatched: ", unmatched)
                    try:
                        # area = (box1[:, 2] - box1[:, 0]) * (box1[:, 3] - box1[:, 1])
                        # print("AREA:", area)
                        IoU = utilsAI.bbox_iou_numpy(box1, box2)
                        # print("----- IoU ------:", IoU)
                        IoU = list( IoU.diagonal())
                        IoU_frame = np.mean(IoU)
                        # print("IoU frame: ", IoU_frame)

                    except IndexError:
                        pass
                    ######################################################################################################
                    ######################################################################################################


                    for a, b, c, d, object_conf, class_score, class_pred in (reversed(detections.numpy())):
                        # print(round(object_conf, 2), round(class_score, 2), class_pred)
                        # print("------DETECTED------->>>>", a,b,c,d, object_conf, class_score, class_pred ,"\n")
                        OBJECT_CONF.append(round(object_conf, 1))
                        CLASS_SCORE.append(round(class_score, 1))
                        OBJECT_CONF_mean = sum(OBJECT_CONF) / len(OBJECT_CONF)
                        CLASS_SCORE_mean = sum(CLASS_SCORE) / len(CLASS_SCORE)


                    for x1, y1, x2, y2, obj_id, cls_pred in tracked_objects:
                            ## detect ONLY persons
                            if cls_pred == 0:
                                periods = len(tracked_objects)
                                box_h = int(((y2 - y1) / unpad_h) * img.shape[0])
                                box_w = int(((x2 - x1) / unpad_w) * img.shape[1])
                                y1 = int(((y1 - pad_y // 2) / unpad_h) * img.shape[0])
                                x1 = int(((x1 - pad_x // 2) / unpad_w) * img.shape[1])
                                # print("centroid X, Y, ID, label:", x1, y1, int(obj_id), int(cls_pred))
                                ID_X.append(x1)
                                ID_Y.append(y1)

                                ID.append(int(obj_id))
                                # FRAME.append(ii)
                                list_of_X_Y = list(zip(ID_X, ID_Y))
                                DF_people = pd.DataFrame(list_of_X_Y, columns=['X', 'Y'])
                                DF_people['ID'] = ID
                                DF_people['timestamp'] = str(times[0])
                                frame_number = str(ii) + "_" + str(n)
                                DF_people['frame_number'] = frame_number
                                ## ratio between the number of the DETECTED pedestrians and the effectivelty TRACKED persons in each frame
                                accuracy = round((len(DF_people)/len(OBJECT_CONF))*100, 2)
                                ## FALSE NEGATIVES
                                FN = len(OBJECT_CONF) - len(DF_people)
                                ## FALSE POSITIVES
                                FP = 0
                                DF_people['accuracy'] = accuracy
                                DF_people['FN'] = FN
                                DF_people['FP'] = FP
                                ## number of mismatched IDS
                                DF_people['IDS'] = 0
                                ## Number of detected people (Ground Truth)
                                DF_people['GT'] = len(OBJECT_CONF)
                                DF_people['frame_confidence'] = round(OBJECT_CONF_mean, 2)
                                ## Intersection over Union (,ust be closer to 1
                                DF_people['frame_IoU'] = round(IoU_frame, 2)
                                # calculate Multiple Object Tracking Precision (must be close to zero as much as possible)
                                DF_people['MOTP'] = (1 - DF_people.frame_IoU) / DF_people.GT

                                ## save frame with tracking boxes......

                                color = colors[int(obj_id) % len(colors)]
                                color = [i * 255 for i in color]
                                cls = classes[int(cls_pred)]
                                cv2.rectangle(frame, (x1, y1), (x1 + box_w, y1 + box_h), color, 4)
                                cv2.rectangle(frame, (x1, y1 - 35), (x1 + len(cls) * 19 + 60, y1), color, -1)
                                cv2.putText(frame, cls + "-" + str(int(obj_id)), (x1, y1 - 10), cv2.FONT_HERSHEY_SIMPLEX, 1,
                                            (255, 255, 255), 3)
    
                                # save image
                                fig = plt.figure(figsize=(12, 8))
                                fig = plt.imshow(frame)
                                # plt.axis('off')
    
                                plt.savefig(output_path + str(ii) + ".jpg",
                                            bbox_inches='tight', pad_inches=0.0)
                                plt.close()


                #### concatenate all IDs by frame
                all_IDs = pd.concat([all_IDs, DF_people])

                ID_list = list(OrderedDict.fromkeys(list(all_IDs.frame_number)))
                ## filter element of a given frame  number
                ## get IDs in the current frame
                current_frame = all_IDs[(all_IDs.frame_number == frame_number)]
                idx = ID_list.index(frame_number)
                ## get IDs in the  previous frame
                previous_frame = all_IDs[(all_IDs.frame_number == ID_list[idx - 1])]
                ## get the number of the mismatched IDs
                IDS = len(list(set(current_frame.ID) - set(previous_frame.ID)))
                all_IDs.IDS[all_IDs.frame_number == frame_number] = IDS

        except cv2.error:
            pass



# sort by 'timedate'
# DF_people.sort_values(['timedate'], ascending=True, inplace=True)
##----- calculate Multiple Object Tracking Accuracy (MOTA)-----###
all_IDs["MOTA"] = 1 - (all_IDs.FN + all_IDs.FP + all_IDs.IDS) / all_IDs.GT
### save .csv file
# all_IDs.to_csv('D:\\Federico\\CityFLows\\objectdetection\\23April2022_PiazzaDucaAosta.csv')


# all_IDs = pd.read_csv('D:\\Federico\\CityFLows\\objectdetection\\19April2022_PiazzaDucaAosta_pixels.csv')
# all_IDs = all_IDs[1:4000]

'''
df_IDs = pd.DataFrame([])
# ID_list = np.unique(list(all_IDs.frame_number))
ID_list = list(OrderedDict.fromkeys(list(all_IDs.frame_number)))
for idx, frame in enumerate(ID_list):
    print(idx, frame)
    ## filter element of a given frame  number
    ## get IDs in the current frame
    current_frame = all_IDs[(all_IDs.frame_number == frame)]
    ## get IDs in the  previous frame
    previous_frame = all_IDs[(all_IDs.frame_number == ID_list[idx-1])]
    ## get the number of the mismatched IDs
    IDS = len(list(set(current_frame.ID) - set(previous_frame.ID)))

    # previous_frame.rename(columns={'ID': 'ID_previous'}, inplace=True)
    df_IDs_sequence = pd.concat([current_frame["ID"], current_frame["frame_number"]], axis=1, ignore_index=True)
    df_IDs_sequence.columns =['ID', 'frame_number']

    # print("MISMATCH ERROR(IDSt): ", IDS)
    df_IDs_sequence['IDS'] = IDS
    ## concatenate all results...
    df_IDs = pd.concat([df_IDs, df_IDs_sequence])
    ## drop rows with NAs values
    # df_IDs.dropna(subset=["diff_IDs"], inplace=True)
    ## merge "IDS" coulumn with all_IDs
    df_IDs = pd.merge(all_IDs, df_IDs[['frame_number', 'IDS']], on=['frame_number'], how='left')
    ##remove duplicates
    df_IDs = df_IDs.drop_duplicates(['timestamp' ,'ID', 'X', 'Y'], keep='last')
'''

# frame in ID_list
# idx = ID_list.index(frame)
# ID_list[idx -1]