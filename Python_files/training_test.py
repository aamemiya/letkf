import tensorflow as tf
import numpy as np
from netCDF4 import Dataset
import time
import math
import os
import sys

import helperfunctions as helpfunc
import network_arch as net

mirrored_strategy = tf.distribute.MirroredStrategy()

def train(parameter_list, model, checkpoint, manager, summary_writer, optimizer):
    
    print("\nCreating Dataset\n")

    forecast_dataset, analysis_dataset = helpfunc.createdataset(parameter_list)
    if parameter_list['make_recurrent']:
        analysis_split = helpfunc.split_sequences(analysis_dataset[:,100:,:], parameter_list['time_splits'])
        analysis_split = np.transpose(analysis_split, (1,0,2,3))

        forecast_split = helpfunc.split_sequences(forecast_dataset[:,100:,:], parameter_list['time_splits'])
        forecast_split = np.transpose(forecast_split, (1,0,2,3))

        analysis_dataset = np.reshape(analysis_split, (analysis_split.shape[0]*analysis_split.shape[1],
                                parameter_list['time_splits'], 1))
        forecast_dataset = np.reshape(forecast_split, (forecast_split.shape[0]*forecast_split.shape[1],
                                parameter_list['time_splits'], parameter_list['locality']))

    else:
        parameter_list['time_splits'] = 1
        analysis_dataset = np.reshape(analysis_dataset, (analysis_dataset.shape[0]*analysis_dataset.shape[1],
                                      1))
        forecast_dataset = np.reshape(forecast_dataset, (forecast_dataset.shape[0]*forecast_dataset.shape[1],
                                      parameter_list['locality']))


    tfdataset_analysis = helpfunc.create_tfdataset(analysis_dataset)
    tfdataset_forecast = helpfunc.create_tfdataset(forecast_dataset)

    #Zipping the files
    dataset = tf.data.Dataset.zip((tfdataset_forecast, tfdataset_analysis))

    #Shuffling the dataset
    dataset = dataset.shuffle(1000000)
    dataset = dataset.batch(batch_size=parameter_list['global_batch_size'])

    #Creating Train and Validation datasets
    train_dataset, val_dataset = helpfunc.train_val_creator(dataset, parameter_list['val_size'])

    #Distributing the dataset
    train_dataset_dist = mirrored_strategy.experimental_distribute_dataset(train_dataset)
    val_dataset_dist = mirrored_strategy.experimental_distribute_dataset(val_dataset)

    #Loss and metric
    with mirrored_strategy.scope():
        
        loss_func = tf.keras.losses.MeanSquaredError(reduction = tf.keras.losses.Reduction.SUM,
                                                    name='LossMSE')
        def compute_loss(labels, predictions):
            per_example_loss = loss_func(labels, predictions)
            return per_example_loss * (1.0 / (parameter_list['global_batch_size'] * parameter_list['time_splits']))
        
        metric_train = tf.keras.metrics.RootMeanSquaredError(name='T_RMSE')
        metric_val = tf.keras.metrics.RootMeanSquaredError(name='V_RMSE')

        def train_step(inputs):
            with tf.GradientTape() as tape:
                
                local_forecast, analysis = inputs
                pred_analysis = model(local_forecast)

                #Calculating relative loss
                loss = compute_loss(analysis, pred_analysis)

            gradients = tape.gradient(loss, model.trainable_variables)
            optimizer.apply_gradients(zip(gradients, model.trainable_weights))

            metric_train(analysis, pred_analysis)

            return loss

        def val_step(inputs):
            local_forecast_val, analysis_val = inputs
            pred_analysis_val = model(local_forecast_val, training = False)

            val_loss = compute_loss(analysis_val, pred_analysis_val)
            metric_val(analysis_val, pred_analysis_val)

            return val_loss

        @tf.function
        def distributed_train_step(inputs):
            per_replica_losses = mirrored_strategy.experimental_run_v2(train_step,
                                                    args=(inputs,))
            return mirrored_strategy.reduce(tf.distribute.ReduceOp.SUM, per_replica_losses,
                                                    axis=None)
        @tf.function
        def distributed_val_step(inputs):
            per_replica_losses = mirrored_strategy.experimental_run_v2(val_step, args=(inputs,))
            return mirrored_strategy.reduce(tf.distribute.ReduceOp.SUM, per_replica_losses,
                    axis=None)
        
        #Initialing training variables
        global_step = 0
        global_step_val = 0
        val_min = 0
        val_loss_min = 100

        #Starting training
        with summary_writer.as_default():

            epochs = parameter_list['epochs']

            for epoch in range(epochs):

                start_time = time.time()

                parameter_list['global_epoch'] += 1

                print('\nStart of epoch %d' %(parameter_list['global_epoch']))
            
                # Iterate over the batches of the dataset.
                for step, inputs in enumerate(train_dataset_dist):
                
                    global_step += 1

                    # Open a GradientTape to record the operations run
                    # during the forward pass, which enables autodifferentiation.
                    loss = distributed_train_step(inputs)

                    # Log of validation results  
                    if (step % parameter_list['log_freq']) == 0:
                        print('Training loss (for one batch) at step %s: %s' % (step+1, float(loss)))
                        
                # Display metrics at the end of each epoch.
                train_acc = metric_train.result()
                print('\nTraining loss at epoch end {}'.format(loss))
                print('Training acc over epoch: %s ' % (float(train_acc)))
                print('Seen so far: %s samples\n' % ((global_step) * parameter_list['global_batch_size']))

                if not(epoch % parameter_list['summery_freq']):
                    tf.summary.scalar('Loss_total', loss, step= parameter_list['global_epoch'])
                    tf.summary.scalar('Train_RMSE', train_acc, step= (parameter_list['global_epoch']))

                # Reset training metrics at the end of each epoch
                metric_train.reset_states()

                #Code for validation at the end of each epoch
                for step_val, inputs in enumerate(val_dataset_dist):

                    global_step_val += 1

                    val_loss = distributed_val_step(inputs)

                    if (step_val % parameter_list['log_freq']) == 0:
                        print('Validation loss (for one batch) at step {}: {}'.format(step_val, val_loss))
                        
                val_acc = metric_val.result()
                print('Validation acc over epoch: %s \n' % (float(val_acc)))
                
                if not(epoch % parameter_list['summery_freq']):
                    tf.summary.scalar('Loss_total_val', val_loss, step= (parameter_list['global_epoch']))
                    tf.summary.scalar('Val_RMSE', metric_val.result(), step= (parameter_list['global_epoch']))
                    
                # Reset training metrics at the end of each epoch
                metric_val.reset_states()

                if val_loss_min > val_loss:
                    val_loss_min = val_loss
                    checkpoint.epoch.assign_add(1)
                    if int(checkpoint.epoch + 1) % parameter_list['num_epochs_checkpoint'] == 0:
                        save_path = manager.save()
                        print("Saved checkpoint for epoch {}: {}".format(checkpoint.epoch, save_path))
                        print("loss {:1.2f}".format(loss.numpy()))

                if math.isnan(val_acc):
                    print('Breaking out as the validation loss is nan')
                    break                

                if (epoch > 19):
                    if not (epoch % parameter_list['early_stop_patience']):
                        if not (val_min):
                            val_min = val_acc
                        else:
                            if val_min > val_acc:
                                val_min = val_acc
                            else:
                                print('Breaking loop as validation accuracy not improving')
                                print("loss {:1.2f}".format(loss.numpy()))
                                break

                print('Time for epoch (in minutes): %s' %((time.time() - start_time)/60))

    if not(os.path.exists(parameter_list['model_loc'])):
        model_json = model.to_json()
        helpfunc.write_to_json(parameter_list['model_loc'], model_json)

    return parameter_list['global_epoch']

def test(parameter_list, model):

    root_grp = Dataset(parameter_list['netCDf_loc'], "a", format="NETCDF4")

    #Extrating the datasets
    analysis_init = root_grp["vam"]
    forecast_init = root_grp["vfm"]

    analysis_dataset = truth_label_creator(analysis_init[10:parameter_list['test_num_timesteps']])
    forecast_dataset = forecast_dataset = locality_creator(forecast_init[10:parameter_list['test_num_timesteps']],
                                                            parameter_list['locality'],
                                                            parameter_list['xlocal'])
    
    new_forecast = np.zeros_like(analysis_dataset, dtype='float32')

    if parameter_list['make_recurrent']:
        for i in range(forecast_dataset.shape[1]):
            forecast = np.expand_dims(forecast_dataset[:,i,:], axis=1)
            new_forecast[:,i,:] = model(forecast)
        new_forecast = np.transpose(np.squeeze(new_forecast))
    else:
        for j in range(forecast_dataset.shape[0]):             
                forecast = forecast_dataset[j,:,:]
                new_forecast[j,:,:] = model(forecast)
        new_forecast = np.transpose(np.squeeze(new_forecast))

    test_time = root_grp.createDimension('tt', forecast_dataset.shape[1])
    v_test_time = root_grp.createVariable('v_test_time', 'i', ('tt',))
    model_vfm = root_grp.createVariable(parameter_list['experiment_name'] + '_vfm', 'f4', ('tt','x',))
    model_vfm[:] = new_forecast
    root_grp.close()
    
def traintest(parameter_list, flag='train'):

    print('\nGPU Available: {}\n'.format(tf.test.is_gpu_available()))

    #Get the Model
    with mirrored_strategy.scope():
        if os.path.exists(parameter_list['model_loc']):
            print('\nLoading saved model...\n')
            j_string = helpfunc.read_json(parameter_list['model_loc'])
            model = tf.keras.models.model_from_json(j_string)
        else:
            model = net.rnn_model(parameter_list)

        #Defining Model compiling parameters
        learningrate_schedule = tf.keras.optimizers.schedules.ExponentialDecay(parameter_list['learning_rate'],
                                                                      decay_steps = parameter_list['lr_decay_steps'],
                                                                      decay_rate = parameter_list['lr_decay_rate'],
                                                                      staircase = True)
        learning_rate = learningrate_schedule 
        optimizer = tf.keras.optimizers.Adam(learning_rate=learning_rate)

        #Defining the checkpoint instance
        checkpoint = tf.train.Checkpoint(epoch = tf.Variable(0), model = model)

    #Creating summary writer
    summary_writer = tf.summary.create_file_writer(logdir= parameter_list['log_dir'])

    #Creating checkpoint instance
    save_directory = parameter_list['checkpoint_dir']
    manager = tf.train.CheckpointManager(checkpoint, directory= save_directory, 
                                        max_to_keep= parameter_list['max_checkpoint_keep'])
    checkpoint.restore(manager.latest_checkpoint).expect_partial()

    #Checking if previous checkpoint exists
    if manager.latest_checkpoint:
        print("Restored from {}".format(manager.latest_checkpoint))

        if flag == 'test':
            print('Starting testing...')
            return test(parameter_list, model)

        if flag == 'train':
            print('Starting training for a restored point... \n')
            return train(parameter_list, model, checkpoint, manager, summary_writer, optimizer)
        
    else:
        print("No checkpoint exists.")
        
        # if flag == 'test':
        #     print('Cannot test as no checkpoint exists. Exiting...')
        #     return parameter_list
        
        if flag == 'train':
            print('Initializing from scratch... \n')
            parameter_list = train(parameter_list, model, checkpoint, manager, summary_writer, optimizer)
            return parameter_list

    print(learning_rate)