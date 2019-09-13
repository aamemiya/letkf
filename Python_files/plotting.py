from netCDF4 import Dataset
import matplotlib.pyplot as plt
import numpy as np
import pickle
import helperfunctions as helpfunc

experiment_name = 'L15_D10_8'

pickle_fileloc = './n_experiments/' + experiment_name + '/checkpoint' + '/params.pickle' 

parameter_list = helpfunc.read_dataframe(pickle_fileloc)

root_grp = Dataset(parameter_list['netCDf_loc'], "a", format="NETCDF4")

#Extrating the datasets
analysis_init = root_grp["vam"]
forecast_init = root_grp["vfm"]
model_forecast_init = root_grp[parameter_list['experiment_name'] + '_vfm']

#Randomly select five variables for plotting
random_variables = np.random.randint(low = 0, high=40, size=5)

plot_analysis = analysis_init[10:parameter_list['test_num_timesteps'], random_variables]
plot_forecat = forecast_init[10:parameter_list['test_num_timesteps'], random_variables]
plot_model_forecast = model_forecast_init[10:parameter_list['test_num_timesteps'], random_variables]

def scatter_plot(x, y, variable_num):

    fig, ax = plt.subplots()
    ax.set_title('Variable {}'.format(variable_num))
    ax.set_ylabel('y_label')
    ax.set_xlabel('x_label')
    ax.scatter(x, y, marker='.')
    img_name = 'scatter_plot_variable_{}'.format(variable_num)
    fig.savefig(img_name, format= 'png', dpi = 1200)

def line_plot(plot_variable, variable_num):

    analysis, forecast, model_forecast = plot_variable
    time = np.arange(len(analysis)) + 1
    
    fig, ax = plt.subplots()
    ax.set_title('Variable {}'.format(variable_num))
    ax.set_xlabel('Time')
    ax.plot(analysis, time, linestyle = '-', label = 'Analysis')
    ax.plot(forecast, time, linestyle = '--', label = 'Forecast')
    ax.plot(model_forecast, time, linestyle = '-.', label = 'Model forecast')
    ax.legend()
    img_name = 'line_plot_variable_{}'.format(variable_num)
    fig.savefig(img_name, format= 'png', dpi = 1200)