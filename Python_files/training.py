import tensorflow as tf
import numpy as np
from netCDF4 import Dataset
import time
import math

import helperfunctions as helpfunc
import network_arch as net

def training(parameter_list):

    #Reading the TFRecord files
    anal_file = helpfunc.read_TFRecord(parameter_list['tfrecord_analysis'])
    fore_file = helpfunc.read_TFRecord(parameter_list['tfrecord_forecast'])

    #Parsing the dataset
    anal_file = anal_file.map(helpfunc._parse_tensor)
    fore_file = fore_file.map(helpfunc._parse_tensor)

    #Zipping the files
    dataset = tf.data.Dataset.zip((fore_file, anal_file))

    #Shuffling the dataset
    dataset = dataset.shuffle(100000)
    dataset = dataset.batch(batch_size=parameter_list['batch_size'])

    #Creating Train and Validation datasets
    train_dataset, val_dataset = helpfunc.train_val_creator(dataset, parameter_list['val_size'])

    #Get the Model
    model = net.rnn_model(parameter_list)

    #Defining Model compiling parameters
    learning_rate = parameter_list['learning_rate']
    optimizer = tf.keras.optimizers.Adam(learning_rate=learning_rate, amsgrad=True)

    #Loss and metric
    loss_func = tf.keras.losses.MeanSquaredError(name='Loss: MSE')
    metric_train = tf.keras.metrics.RootMeanSquaredError(name='T_RMSE')
    metric_val = tf.keras.metrics.RootMeanSquaredError(name='V_RMSE')

    #Creating summary writer
    summary_writer = tf.summary.create_file_writer(logdir= parameter_list['log_dir'])

    #Creating checkpoint instance
    checkpoint = tf.train.Checkpoint(epoch = tf.Variable(0), optimizer = optimizer, model = model)
    save_directory = parameter_list['checkpoint_dir']
    manager = tf.train.CheckpointManager(checkpoint, directory= save_directory, 
                                        max_to_keep= parameter_list['max_checkpoint_keep'])
    checkpoint.restore(manager.latest_checkpoint)

    #Checking if previous checkpoint exists
    if manager.latest_checkpoint:
        print("Restored from {}".format(manager.latest_checkpoint))
    else:
        print("Initializing from scratch.")
        
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

            print('\nStart of epoch %d' %(epoch+1))

            # Iterate over the batches of the dataset.
            for step, (local_forecast, analysis) in enumerate(train_dataset):

                global_step += 1

                # Open a GradientTape to record the operations run
                # during the forward pass, which enables autodifferentiation.
                with tf.GradientTape() as tape:

                    pred_analysis = model(local_forecast)

                    #Calculating relative loss
                    loss = loss_func(analysis, pred_analysis)

                gradients = tape.gradient(loss, model.trainable_variables)
                optimizer.apply_gradients(zip(gradients, model.trainable_weights))

                metric_train(analysis, pred_analysis)

                # Log of validation results  
                if (step % parameter_list['log_freq']) == 0:
                    print('Training loss (for one batch) at step %s: %s' % (step+1, float(loss)))
                    print('Seen so far: %s samples' % ((global_step) * parameter_list['batch_size']))
                    
            # Display metrics at the end of each epoch.
            train_acc = metric_train.result()
            print('\nTraining loss at epoch end {}'.format(loss))
            print('Training acc over epoch: %s \n' % (float(train_acc)))

            if not(epoch % parameter_list['summery_freq']):
                tf.summary.scalar('Loss_total', loss, step= (epoch + 1))
                tf.summary.scalar('Train_RMSE', train_acc, step= (epoch + 1))

            # Reset training metrics at the end of each epoch
            metric_train.reset_states()

            #Code for validation at the end of each epoch
            for step_val, (local_forecast_val, analysis_val) in enumerate(val_dataset):

                global_step_val += 1

                pred_analysis_val = model(local_forecast_val)

                val_loss = loss_func(analysis_val, pred_analysis_val)
                metric_val(analysis_val, pred_analysis_val)

                if (step_val % parameter_list['log_freq']) == 0:
                    print('Validation loss (for one batch) at step %s: %s' % (step_val, float(val_loss)))
                    
            val_acc = metric_val.result()
            print('Validation acc over epoch: %s \n' % (float(val_acc)))
            print('Seen so far: %s samples \n' % ((step + 1) * parameter_list['batch_size']))
            
            if not(epoch % parameter_list['summery_freq']):
                tf.summary.scalar('Loss_total_val', val_loss, step= (epoch + 1))
                tf.summary.scalar('Val_RMSE', metric_val.result(), step= (epoch + 1))
                
            # Reset training metrics at the end of each epoch
            metric_val.reset_states()

            if val_loss_min > val_loss:
                val_loss_min = val_loss
                checkpoint.epoch.assign_add(1)
                if int(checkpoint.epoch + 1) % parameter_list['num_epochs_checkpoint'] == 0:
                    save_path = manager.save()
                    print("Saved checkpoint for epoch {}: {}".format(int(checkpoint.epoch), save_path))
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
                            # save_path = manager.save()
                            # print("Saved checkpoint for epoch {}: {}".format(int(checkpoint.epoch), save_path))
                            print("loss {:1.2f}".format(loss.numpy()))
                            break

            print('Time for epoch (in minutes): %s' %((time.time() - start_time)/60))

    return (epoch + 1)