from __future__ import absolute_import, division, print_function, unicode_literals

import sys
import pathlib

import numpy as np

import pandas as pd

import tensorflow as tf
from tensorflow.python import keras
from tensorflow.python.keras import layers

dim=int(sys.argv[1])


def build_model():
      model = keras.Sequential([
            layers.Dense(12, activation=tf.nn.relu, input_shape=[dim]),
            layers.Dense(12, activation=tf.nn.relu),
      layers.Dense(dim)
      ])
      optimizer = tf.keras.optimizers.RMSprop(0.001)
      
      model.compile(loss='mean_squared_error',
                    optimizer=optimizer,
                    metrics=['mean_absolute_error', 'mean_squared_error'])
      return model


model = build_model()
model.summary()
model.save('model.h5')

