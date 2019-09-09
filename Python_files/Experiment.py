import copy
import random
import numpy as np
import pandas as pd
import os
import sys

import helperfunctions as helpfunc
import training_test as tntt

#Parameter List
parameter_list = {}

#Dataset and directories related settings
parameter_list['netCDf_loc'] = "../lorenz96_multi/DATA_sample/X40F18/all_10/nocorr_I20/assim.nc"
parameter_list['xlocal'] = 3
parameter_list['locality'] = 19
parameter_list['time_splits'] = 30
parameter_list['tfrecord_analysis'] = './analysis.tfrecord'
parameter_list['tfrecord_forecast'] = './forecast.tfrecord'
parameter_list['experiment_dir'] = "./exper"
parameter_list['checkpoint_dir'] = parameter_list['experiment_dir'] + '/checkpoint'
parameter_list['log_dir'] = parameter_list['experiment_dir'] + '/log'
parameter_list['model_loc'] = parameter_list['experiment_dir'] + '/model.json'

if not(os.path.isfile(parameter_list['tfrecord_analysis']) and os.path.isfile(parameter_list['tfrecord_forecast'])):
    if not(os.path.isfile(parameter_list['netCDf_loc'])):
        print("\nNetCDF file doesn't exist for tfrecord creation, terminating....\n")
        sys.exit()
    else:
        print("\nCreating TFrecord.\n")
        helpfunc.tfrecord(parameter_list)
else:
    print('\nTFrecord file exists. Using them for training.\n')

csv_name = parameter_list['checkpoint_dir'] + '/params.csv'

if not os.path.exists(parameter_list['experiment_dir']):
    os.makedirs(parameter_list['log_dir'])
    os.mkdir(parameter_list['checkpoint_dir'])

    #Network related settings
    parameter_list['LSTM_output'] = 100
    parameter_list['net_output'] = 1
    parameter_list['activation'] = 'tanh'
    parameter_list['rec_activation'] = 'hard_sigmoid'
    parameter_list['l2_regu'] = 0.0
    parameter_list['l1_regu'] = 0.0
    parameter_list['lstm_dropout'] = 0.0
    parameter_list['rec_lstm_dropout'] = 0.0
    parameter_list['unroll_lstm'] = False

    #Training related settings
    parameter_list['learning_rate'] = 1e-3
    parameter_list['max_checkpoint_keep'] = 1
    parameter_list['epochs'] = 50
    parameter_list['log_freq'] = 5
    parameter_list['early_stop_patience'] = 5
    parameter_list['num_epochs_checkpoint'] = 1
    parameter_list['summery_freq'] = 1
    parameter_list['global_epoch'] = 0
    parameter_list['batch_size'] = 250
    parameter_list['val_size'] = 2

else:
    if os.path.isfile(csv_name):
        parameter_list = helpfunc.read_dataframe(csv_name)
    else:
        print('\nNo CSV file exists at {}. Exiting....\n'.format(csv_name))
        sys.exit()

parameter_list['global_epoch'] = tntt.traintest(copy.deepcopy(parameter_list))
params_dataframe = pd.DataFrame(parameter_list, index=[1])

helpfunc.write_dataframe(params_dataframe, csv_name)

