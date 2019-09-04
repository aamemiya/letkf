import numpy as np
import tensorflow as tf

#Model defination
def rnn_model(parameter_list):
    net_input = tf.keras.Input(shape=(parameter_list['time_splits'], parameter_list['locality']), name='INPUT')
    x = tf.keras.layers.LSTM(units=parameter_list['LSTM_output'], return_sequences=True)(net_input)
    output = tf.keras.layers.Dense(units=parameter_list['net_output'], activation=tf.keras.activations.relu)(x)
    return tf.keras.Model(net_input, output, name='RNN')