import pandas as pd
import numpy as np
import tensorflow as tf

from sklearn.preprocessing import MinMaxScaler
from tensorflow.keras.layers import Dense, Dropout, LSTM
from tensorflow.keras.models import Sequential

training_data = pd.read_csv("../eurusd-3month-1h-DL.csv")
test_data = pd.read_csv("../eurusd-45days-1h-test-DL.csv")

scaler = MinMaxScaler(feature_range=(0, 1))
data_training_array = scaler.fit_transform(training_data)
data_test_array = scaler.fit_transform(test_data)

x_train = []
y_train = []
for i in range(100, data_training_array.shape[0]):
    x_train.append(data_training_array[i - 100 : i])
    y_train.append(data_training_array[i, 0])
x_train, y_train = np.array(x_train), np.array(y_train)

x_test = []
y_test = []
for i in range(100, data_test_array.shape[0]):
    x_test.append(data_test_array[i - 100 : i])
    y_test.append(data_test_array[i, 0])
x_test, y_test = np.array(x_test), np.array(y_test)


def new_model(x_train):
    model = Sequential()
    model.add(
        LSTM(
            units=50,
            activation="relu",
            return_sequences=True,
            input_shape=(x_train.shape[1], 1),
        )
    )
    model.add(Dropout(0.2))

    model.add(LSTM(units=60, activation="relu", return_sequences=True))
    model.add(Dropout(0.3))

    model.add(LSTM(units=80, activation="relu", return_sequences=True))
    model.add(Dropout(0.4))

    model.add(LSTM(units=120, activation="relu"))
    model.add(Dropout(0.5))

    model.add(Dense(units=1))
    model.compile(
        optimizer=tf.keras.optimizers.legacy.Adam(),
        loss="mean_squared_error",
        metrics=[tf.keras.metrics.MeanAbsoluteError()],
    )
    return model


def load_existing_model():
    return tf.keras.models.load_model("keras_model.h5")


def train_model(model):
    with tf.device("/CPU:0"):
        model.fit(x_train, y_train, epochs=100)
        model.save("keras_model.h5")


model = load_existing_model()
y_pred = model.predict(x_test)

y_pred = scaler.inverse_transform(y_pred)
y_test = scaler.inverse_transform(y_test.reshape(-1, 1))
