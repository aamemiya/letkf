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

file_ms=open('xfm_mean_sdv.dat','rb')

file_ms.read(4) ### padding
xfm_mean=np.array(struct.unpack('f'*Ndim,file_ms.read(4*Ndim)))
file_ms.read(4) ### padding
file_ms.read(4) ### padding
xfm_sdv=np.array(struct.unpack('f'*Ndim,file_ms.read(4*Ndim)))
file_ms.read(4) ### padding


file_in=open('xfm_temp.dat','rb')
file_out=open('xam_temp.dat','wb')

dumy=file_in.read(4) ### padding

x_in=np.array(struct.unpack('f'*Ndim,file_in.read(4*Ndim)))

def norm(x):
 return (x - xfm_mean) / xfm_sdv


normed_x_in=norm(x_in)

model= keras.models.load_model('model.h5')

x_out = model.predict(np.array([normed_x_in]))
x_out1= x_out[0]

file_out.write(dumy)
file_out.write(struct.pack('f'*Ndim,*x_out1))
file_out.write(dumy)



      
