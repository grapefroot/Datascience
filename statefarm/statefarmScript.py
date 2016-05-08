import os
import glob
import math
import time
import random
import numpy as np
import pandas as pd
from lasagne import layers
from lasagne.updates import nesterov_momentum
from nolearn.lasagne import NeuralNet

from sklearn.utils import shuffle
from sklearn.preprocessing import LabelEncoder
from sklearn.cross_validation import train_test_split

'''
Loading data functions
'''
PIXELS = 96
imageSize = PIXELS * PIXELS
num_features = imageSize


def load_train_cv(encoder):
  X_train = []
  y_train = []
  print('Reading train images')
  for j in range(10):
    print('Loading folder c{}'.format(j))
    path = os.path.join('.', 'imgs', 'train', 'c' + str(j), '*.jpg')
    files = glob.glob(path)
    for file in files:
      img = cv2.imread(file, 0)
      img = cv2.resize(img, (PIXELS, PIXELS))
      img = np.reshape(img, (1, num_features))
      X_train.append(img)
      y_train.append(j)

  X_train = np.array(X_train)
  y_train = np.array(y_train)

  y_train = encoder.fit_transform(y_train).astype('int32')

  X_train, y_train = shuffle(X_train, y_train)

  X_train, X_test, y_train, y_test = train_test_split(X_train, y_train,
                                                      test_size=0.1)
  X_train = X_train.reshape(X_train.shape[0], 1, PIXELS, PIXELS).astype(
    'float32') / 255.
  X_test = X_test.reshape(X_test.shape[0], 1, PIXELS, PIXELS).astype(
    'float32') / 255.

  return X_train, y_train, X_test, y_test, encoder


def load_test():
  print('Reading test images')
  path = os.path.join('.', 'imgs', 'test', '*.jpg')
  files = glob.glob(path)
  X_test = []
  X_test_id = []
  total = 0
  thr = math.floor(len(files) / 10)
  for file in files:
    filebase = os.path.basename(file)
    img = cv2.imread(file, 0)
    img = cv2.imread(img, (PIXELS, PIXELS))
    img = np.reshape(img, (1, num_features))
    X_test.append(img)
    X_test_id.append(filebase)
    total += 1
    if total % thr == 0:
      print('Read {} images from {}'.format(total, len(files)))
  X_test = np.array(X_test)
  X_test_id = np.array(X_test_id)

  X_test = X_test.reshape(X_test.shape[0], 1, PIXELS, PIXELS).astype(
    'float32') / 255.
  return X_test, X_test_id


def construct_net1():
  net1 = NeuralNet(
    layers=[
      ('input', layers.InputLayer),
      ('hidden', layers.DenseLayer),
      ('output', layers.DenseLayer),
    ],
    input_shape=(None, 9216),
    hidden_num_units=100,
    output_nonlinearity=None,
    output_num_units=9,
    update=nesterov_momentum,
    update_learning_rate=0.01,
    update_momentum=0.9,
    max_epochs=400,
    verbose=1,
  )
  return net1


def main():
  enc = LabelEncoder()
  X, y, _, _, _ = load_train_cv(enc)
  print
  X.shape
  print
  y.shape
  estimator = construct_net1()
  estimator.fit(X, y)


if __name__ == '__main__':
  main()
