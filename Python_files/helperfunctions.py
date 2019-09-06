import tensorflow as tf
import numpy as np
from netCDF4 import Dataset
import time
import math
import pandas as pd

#For creating locality for individual state variable
def locality_creator(init_dataset, locality_range, xlocal):
    
    output_dataset = np.zeros((init_dataset.shape[0], init_dataset.shape[1], locality_range))
    radius = int(locality_range / 2)
    
    locality = np.linspace(-radius, radius, locality_range)
    locality = np.true_divide(locality, xlocal)
    locality = np.power(locality, 2)
    locality = np.exp((-1/2) * locality)
    
    for i in range(init_dataset.shape[1]):
        start = i - radius
        stop = i + radius
        index = np.linspace(start,stop,locality_range, dtype='int')
        if stop >= init_dataset.shape[1]:
            stop2 = (stop + 1)%init_dataset.shape[1]
            index[:-stop2] = np.linspace(start,init_dataset.shape[1]-1,init_dataset.shape[1]-start, dtype='int')
            index[-stop2:] = np.arange(0,stop2,1,dtype='int')
        output_dataset[:,i,:] = init_dataset[:,index]

    return np.multiply(np.transpose(output_dataset,(1,0,2)), locality).astype('float32') 

#For creating the truth label
def truth_label_creator(init_dataset):
    output_dataset = init_dataset[:]
    output_dataset = np.expand_dims(output_dataset, axis=0)
    return np.transpose(output_dataset.astype('float32'))

#Creating time data splits
def split_sequences(sequences, n_steps):
    X = list()
    for i in range(len(sequences)):
        # find the end of this pattern
        end_ix = i*n_steps + n_steps
        # check if we are beyond the dataset
        if end_ix > len(sequences):
            break
        # gather input and output parts of the pattern
        seq_x = sequences[i*n_steps:end_ix, :]
        X.append(seq_x)
    return np.array(X)

#For serializing the tensor to a string for TFRecord
def _serialize_tensor(value):
    return tf.io.serialize_tensor(value)

#For writing data to the TFRecord file
def write_TFRecord(filename, dataset, time_splits):
    with tf.io.TFRecordWriter(filename) as writer:
        for i in range(dataset.shape[0]):
            dataset_splits = split_sequences(dataset[i], time_splits)
            for j in range(dataset_splits.shape[0]):
                data = dataset_splits[j]
                serial_string = _serialize_tensor(data)
                writer.write(serial_string.numpy())
    writer.close()

#For reading the TFRecord File
def read_TFRecord(filename):
    return tf.data.TFRecordDataset(filename)

#For parsing the value from string to float32
def _parse_tensor(value):
    return tf.io.parse_tensor(value, out_type=tf.float32)

#For creating Train and Validation datasets
def train_val_creator(dataset, val_size):
    val_dataset = dataset.take(val_size)
    train_dataset = dataset.skip(val_size)
    return train_dataset, val_dataset

def read_dataframe(filename):
    return pd.read_csv(filename).to_dict(orient= 'records')[0]

def write_dataframe(dataframe, filename):   
    dataframe.to_csv(filename)

def tfrecord(parameter_list):
    #Getting the NetCDF files
    root_grp = Dataset(parameter_list['netCDf_loc'], "r", format="NETCDF4")

    #Extrating the datasets
    analysis_init = root_grp["vam"]
    forecast_init = root_grp["vfm"]

    analysis_dataset = helpfunc.truth_label_creator(analysis_init)
    forecast_dataset = helpfunc.locality_creator(forecast_init, parameter_list['locality'], parameter_list['xlocal'])

    helpfunc.write_TFRecord(parameter_list['tfrecord_analysis'], analysis_dataset, parameter_list['time_splits'])
    helpfunc.write_TFRecord(parameter_list['tfrecord_forecast'], forecast_dataset, parameter_list['time_splits'])

def write_to_json(loc, model):
    with open(loc, 'w') as json_file:
        json_file.write(model)

def read_json(loc):
    json_file = open(loc, 'r')
    content = json_file.read()
    json_file.close()
    return content