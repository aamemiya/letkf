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
parameter_list['netCDf_loc'] = "/home/mshlok/letkf/DATA_sample_long/X40F18/all_10/nocorr_I20/assim.nc"
parameter_list['xlocal'] = 3
parameter_list['locality'] = 19
parameter_list['num_timesteps'] = 30000
parameter_list['time_splits'] = 30
parameter_list['tfrecord_analysis'] = './tfrecord/X40F18_I20_analysis.tfrecord'
parameter_list['tfrecord_forecast'] = './tfrecord/X40F18_I20_forecast.tfrecord'
parameter_list['experiment_dir'] = "./exper"
parameter_list['checkpoint_dir'] = parameter_list['experiment_dir'] + '/checkpoint'
parameter_list['log_dir'] = parameter_list['experiment_dir'] + '/log'
parameter_list['model_loc'] = parameter_list['experiment_dir'] + '/model.json'

csv_name = parameter_list['checkpoint_dir'] + '/params.csv'

if not os.path.exists(parameter_list['experiment_dir']):
    os.makedirs(parameter_list['log_dir'])
    os.mkdir(parameter_list['checkpoint_dir'])

    #Network related settings
    parameter_list['num_lstm_layers'] = 1
    parameter_list['num_dense_layers'] = 2
    parameter_list['dense_output'] = 50
    parameter_list['LSTM_output'] = 100
    parameter_list['net_output'] = 1
    parameter_list['activation'] = 'tanh'
    parameter_list['rec_activation'] = 'hard_sigmoid'
    parameter_list['l2_regu'] = 0.0
    parameter_list['l1_regu'] = 0.0
    parameter_list['lstm_dropout'] = 0.0
    parameter_list['rec_lstm_dropout'] = 0.0
    parameter_list['unroll_lstm'] = False
    parameter_list['new_forecast'] = True

    #Training related settings
    parameter_list['max_checkpoint_keep'] = 1
    parameter_list['log_freq'] = 5
    parameter_list['early_stop_patience'] = 5
    parameter_list['num_epochs_checkpoint'] = 1
    parameter_list['summery_freq'] = 1
    parameter_list['global_epoch'] = 0
    parameter_list['global_batch_size'] = 2048 * 4
    parameter_list['val_size'] = 2
    parameter_list['lr_decay_steps'] = 30000
    parameter_list['learning_rate'] = 1e-3 * parameter_list['global_batch_size'] / 256

else:
    if os.path.isfile(csv_name):
        parameter_list = helpfunc.read_dataframe(csv_name)
    else:
        print('\nNo CSV file exists at {}. Exiting....\n'.format(csv_name))
        sys.exit()

parameter_list['epochs'] = 4

parameter_list['global_epoch'] = tntt.traintest(copy.deepcopy(parameter_list))
params_dataframe = pd.DataFrame(parameter_list, index=[1])

helpfunc.write_dataframe(params_dataframe, csv_name)
