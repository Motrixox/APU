# Importowanie bibliotek
import numpy as np
import keras
import matplotlib.pyplot as plt
from keras import layers

# Parametry modelu i danych
num_classes = 10
input_shape = (28, 28, 1)

# Wczytanie obrazów i podział na treningowe i testowe
(x_train, y_train), (x_test, y_test) = keras.datasets.mnist.load_data()

# Normalizacja obrazów
x_train = x_train.astype("float32") / 255
x_test = x_test.astype("float32") / 255

# Dostosowanie rozmiaru obrazów
x_train = np.expand_dims(x_train,-1)
x_test = np.expand_dims(x_test,-1)
print("x_train shape:", x_train.shape)
print(x_train.shape[0], "train samples")
print(x_test.shape[0], "test samples")

# Konwersja klasyfikacji danych do kategorii
y_train = keras.utils.to_categorical(y_train, num_classes)
y_test = keras.utils.to_categorical(y_test, num_classes)

# Stworzenie modelu
model = keras.Sequential(
[
keras.Input(shape=input_shape),
layers.Conv2D(32, kernel_size=(3, 3), activation="relu"),
layers.MaxPooling2D(pool_size=(2, 2)),
layers.Conv2D(64, kernel_size=(3, 3), activation="relu"),
layers.MaxPooling2D(pool_size=(2, 2)),
layers.Flatten(),
layers.Dropout(0.5),
layers.Dense(num_classes, activation="softmax"),
]
)
model.summary()

model.compile(loss="categorical_crossentropy", optimizer="adam", metrics=["accuracy"])

# Trening modelu
batch_size = 128
epochs = 1
model.fit(x_train, y_train, batch_size=batch_size, epochs=epochs,
validation_split=0.1)

# Ewaluacja wyników treningu
score = model.evaluate(x_test, y_test, verbose=0)
print("Test loss:", score[0])
print("Test accuracy:", score[1])

# Przykładowa predykcja
prediction = model.predict(x_test)
print(prediction)

img = plt.imshow(1-x_test[1])
img.set_cmap('gray')
plt.axis('off')
plt.show()