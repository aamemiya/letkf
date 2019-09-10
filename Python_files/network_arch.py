import numpy as np
import tensorflow as tf

#Model defination
def rnn_model(parameter_list):

    x = tf.keras.Input(shape=(None,
                            parameter_list['locality']),
                            name='INPUT')

    inputs = x

    for i in range(parameter_list['num_lstm_layers']):
        
        x = tf.keras.layers.LSTM(units=parameter_list['LSTM_output'],
                            activation = parameter_list['activation'],
                            recurrent_activation = parameter_list['rec_activation'],
                            kernel_regularizer = tf.keras.regularizers.l2(parameter_list['l2_regu']),
                            activity_regularizer = tf.keras.regularizers.l1(parameter_list['l1_regu']),
                            dropout = parameter_list['lstm_dropout'],
                            recurrent_dropout = parameter_list['rec_lstm_dropout'],
                            unroll = parameter_list['unroll_lstm'],
                            return_sequences=True,
                            name = 'LSTM_{}'.format(i+1))(x)

    for i in range(parameter_list['num_dense_layers']-1):

        x = tf.keras.layers.Dense(units=parameter_list['dense_output'],
                                kernel_regularizer = tf.keras.regularizers.l2(parameter_list['l2_regu']),
                                activation = None,
                                name = 'DENSE_{}'.format(i+1))(x)
        x = tf.keras.layers.ELU(1.5, name='ELU_{}'.format(i+1))(x)

    if parameter_list['new_forecast']:

        x = tf.keras.layers.Dense(units=parameter_list['net_output'],
                                kernel_regularizer = tf.keras.regularizers.l2(parameter_list['l2_regu']),
                                activation = None,
                                name = 'DENSE_OUTPUT')(x)
        x = tf.keras.layers.ELU(1.5, name = 'ELU_output')(x)
    
    else:

        x = inputs + tf.keras.layers.Dense(units=parameter_list['net_output'],
                                kernel_regularizer = tf.keras.regularizers.l2(parameter_list['l2_regu']),
                                activation = None,
                                name = 'DENSE_OUTPUT')(x)
        x = tf.keras.layers.ELU(1.5, name = 'ELU_output')(x)
    
    outputs = x
    return tf.keras.Model(inputs, outputs, name='RNN')