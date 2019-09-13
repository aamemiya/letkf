from netCDF4 import Dataset
import matplotlib.pyplot as plt
import numpy as np
import pickle
import helperfunctions as helpfunc
import os

experiment_name = 'L15_D10_5'

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
plot_model_forecast = model_forecast_init[:,random_variables]

def scatter_plot(plot_variable, variable_num, x1_label, x2_label, y_label, directory):

    x1, x2, y = plot_variable
    fig, ax = plt.subplots(1,3, sharey=True)
    fig.suptitle('Variable {}'.format(variable_num))

    print(len(x1), len(x2), len(y))
    ax[0].set_ylabel(y_label)
    ax[0].set_xlabel(x1_label)
    ax[0].scatter(x1, y, s = 15, marker='*')
    
    ax[1].set_xlabel(x2_label)
    ax[1].scatter(x2, y, s = 15, marker='o')

    ax[2].set_xlabel('Together')
    ax[2].scatter(x2, y, s=20, marker='o', label=x2_label)
    ax[2].scatter(x1, y, s=8, marker='*', label=x1_label)

    img_name = directory + '/scatter_plot_variable_{}.png'.format(variable_num)
    print('Saving image file: {}'.format(img_name))
    fig.savefig(img_name, format= 'png', dpi = 1200)

def line_plot(plot_variable, variable_num, directory):

    model_forecast, forecast,  analysis = plot_variable
    time = np.arange(len(analysis)) + 1
    
    fig, ax = plt.subplots()
    ax.set_title('Variable {}'.format(variable_num))
    ax.set_xlabel('Time')
    ax.plot(time, analysis, 'g-',  label = 'Analysis')
    ax.plot(time, forecast, 'y--',  label = 'Forecast')
    ax.plot(time, model_forecast, 'k:',  label = 'Model forecast')
    ax.legend()
    img_name = directory + '/line_plot_variable_{}.png'.format(variable_num)
    print('Saving image file: {}'.format(img_name))
    fig.savefig(img_name, format= 'png', dpi = 1200)

image_dir = (parameter_list['experiment_dir'] + '/images')
if not(os.path.exists(image_dir)):
    os.mkdir(image_dir)

for i in range(len(random_variables)):
    
    scatter_plot((plot_model_forecast[:,i],
                  plot_forecat[:,i],
                  plot_analysis[:,i]),
                  random_variables[i],
                  'Model forecast',
                  'Forecast',
                  'Analysis',
                  image_dir)
    

    line_plot((plot_model_forecast[:,i],
                  plot_forecat[:,i],
                  plot_analysis[:,i]),
                  random_variables[i],
                  image_dir)
