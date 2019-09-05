#Parameter List
parameter_list = {}

parameter_list['netCDf_loc'] = "../lorenz96_multi/DATA_sample/X40F18/all_10/nocorr_I20/assim.nc"
parameter_list['xlocal'] = 3
parameter_list['locality'] = 19
parameter_list['time_splits'] = 30
parameter_list['batch_size'] = 250
parameter_list['val_size'] = 2
parameter_list['LSTM_output'] = 5
parameter_list['net_output'] = 1
parameter_list['learning_rate'] = 1e-3
parameter_list['log_dir'] = './log'
parameter_list['checkpoint_dir'] = './checkpoint'
parameter_list['max_checkpoint_keep'] = 4
parameter_list['epochs'] = 5
parameter_list['log_freq'] = 5
parameter_list['early_stop_patience'] = 5
parameter_list['num_epochs_checkpoint'] = 2
parameter_list['summery_freq'] = 1

parameter_list['tfrecord_analysis'] = './analysis.tfrecord'
parameter_list['tfrecord_forecast'] = './forecast'