import pandas as pd
import numpy as np
import tensorflow as tf

from sklearn.preprocessing import MinMaxScaler
from sklearn.linear_model import LinearRegression
from sklearn.metrics import confusion_matrix, accuracy_score
from tensorflow.keras.layers import Dense, Dropout, LSTM
from tensorflow.keras.models import Sequential

# implementation from: https://github.com/034adarsh/Stock-Price-Prediction-Using-LSTM/blob/main/LSTM_Improved_model(diff_dataset).ipynb

training_data = pd.read_csv("../eurusd-3month-1h-DL.csv", sep=";")
test_data = pd.read_csv("../eurusd-45days-1h-test-DL.csv", sep=";")


def dl_data_prep():
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
    y_test = scaler.inverse_transform(y_test.reshape(-1, 1))
    return x_train, y_train, x_test, y_test


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


def predict_using_lstm(x_test, model):
    model = load_existing_model()
    y_pred = model.predict(x_test)
    y_pred = scaler.inverse_transform(y_pred)
    return y_pred


def regression():
    x_train = training_data.drop(["close"], axis=1)
    y_train = training_data.drop(["open", "low", "high"], axis=1)
    x_test = test_data.drop(["close"], axis=1)
    y_test = test_data.drop(["open", "low", "high"], axis=1)

    regressor = LinearRegression()
    regressor.fit(x_train, y_train)
    y_pred = regressor.predict(x_test)
    print(regressor.score(x_test, y_test))
    return y_pred, y_test


def calculate_total_profit(y_pred, y_test):
    commission = 5
    capital = 1000
    leverage = 100
    transactions = []
    current_position = None
    future_position = None
    open_price = None

    for i in range(0, len(y_pred)):
        if i == len(y_pred) - 1:
            if current_position != None:
                close_price = y_test[i]
                profit = (close_price - open_price) * (capital / open_price) * leverage
                if current_position == "SHORT":
                    profit = -profit
                transactions.append(profit)
                break

        if y_pred[i + 1] > y_test[i]:
            future_position = "LONG"
        elif y_pred[i + 1] < y_test[i]:
            future_position = "SHORT"

        if current_position != future_position:
            if current_position == None:
                open_price = y_test[i + 1]
                current_position = future_position
            else:
                close_price = y_test[i + 1]
                profit = (close_price - open_price) * (capital / open_price) * leverage
                if current_position == "SHORT":
                    profit = -profit

                transactions.append(profit)
                open_price = y_test[i + 1]
                current_position = future_position

    return sum(transactions) - commission * len(transactions)


y_pred, y_test = regression()
y_pred, y_test = np.array(y_pred), np.array(y_test)
total_profit = calculate_total_profit(y_pred, y_test)
