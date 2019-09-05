import tensorflow as tf
import numpy as np
from netCDF4 import Dataset

import params
import helperfunctions as helpfunc

parameter_list = params.parameter_list

#Getting the NetCDF files
root_grp = Dataset(parameter_list['netCDf_loc'], "r", format="NETCDF4")

#Extrating the datasets
analysis_init = root_grp["vam"]
forecast_init = root_grp["vfm"]

analysis_dataset = helpfunc.truth_label_creator(analysis_init)
forecast_dataset = helpfunc.locality_creator(forecast_init, parameter_list['locality'], parameter_list['xlocal'])

helpfunc.write_TFRecord('analysis.tfrecord', analysis_dataset, parameter_list['time_splits'])
helpfunc.write_TFRecord('forecast.tfrecord', forecast_dataset, parameter_list['time_splits'])