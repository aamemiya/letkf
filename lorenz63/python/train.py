from __future__ import absolute_import, division, print_function, unicode_literals

import sys
import pathlib
import struct

import numpy as np

import pandas as pd

import tensorflow as tf
from tensorflow.python import keras
from tensorflow.python.keras import layers

Ndim=8

Nrec=int(sys.argv[1])

file_ms=open('xfm_mean_sdv.dat','rb')

file_in=open('xfm_train.dat','rb')
file_out=open('xam_train.dat','rb')

file_ms.read(4) ### padding
xfm_mean=np.array(struct.unpack('f'*Ndim,file_ms.read(4*Ndim)))
file_ms.read(4) ### padding
file_ms.read(4) ### padding
xfm_sdv=np.array(struct.unpack('f'*Ndim,file_ms.read(4*Ndim)))


def norm(x):
      return (x - xfm_mean) / xfm_sdv

train_data_in        = np.array([[None for column in range(Ndim)] for row in range(Nrec)])
normed_train_data_in = np.array([[None for column in range(Ndim)] for row in range(Nrec)])
train_data_out       = np.array([[None for column in range(Ndim)] for row in range(Nrec)])

for irec in range(Nrec):
    file_in.read(4) ### padding
    file_out.read(4)
    train_data_in[irec]=np.array(struct.unpack('f'*Ndim,file_in.read(4*Ndim)))
    train_data_out[irec]=np.array(struct.unpack('f'*Ndim,file_out.read(4*Ndim)))
    file_in.read(4) ### padding
    file_out.read(4)
    normed_train_data_in[irec] = norm(train_data_in[irec])

model = keras.models.load_model('model.h5')

EPOCHS = 3

history = model.fit(
  normed_train_data_in, train_data_out,
  epochs=EPOCHS, validation_split = 0.2, verbose=0)

model.save('model.h5')


