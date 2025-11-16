# red_neuronal_ajedrez.R
# Hecho por:
# Anthonny Flores (C32875)
# Andres Zuñiga Mora (C38733)
# Amy Chen Wu (C32203)
# Leonardo Vega Aragon (C38313)

# Libreria
library(keras3)

# Definir bloque residual
residual.block <- function(x, filters) {
  shortcut = x
  x = x |>
    layer_conv_2d(filters = filters, kernel_size = c(3, 3), activation = "relu", padding = "same") |>
    layer_batch_normalization() |>
    layer_conv_2d(filters = filters, kernel_size = c(3, 3), activation = "relu", padding = "same") |>
    layer_batch_normalization()
  x = layer_add(inputs = list(shortcut, x))
  x = layer_activation(activation = "relu")(x)
  return(x)
}

# Funcion para hacer el modelo
generate.model <- function() { # "Hatchet1"
  # Modificacion: Cambiar policy units a 4672 (estandar AlphaZero 64x73 move types)
  
  # Definir modelo
  inputs = layer_input(shape = c(8, 8, 12)) # Entrada tablero 8x8x12 (tensor)
  x = inputs |>
    layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu", padding = "same") |>
    layer_batch_normalization()
  
  # 20 bloques residuales 
  for (i in 1:20) {
    x = residual.block(x, 256)
  }
  
  # Cabeza de valor
  value.head = x |>
    layer_flatten() |>
    layer_dense(units = 512, activation = "relu", kernel_regularizer = regularizer_l2(0.01)) |>
    layer_dropout(rate = 0.4) |>
    layer_dense(units = 256, activation = "relu", kernel_regularizer = regularizer_l2(0.01)) |>
    layer_dropout(rate = 0.3) |>
    layer_dense(units = 1, activation = "tanh", name = "value")
  
  # Cabeza de politica (AlphaZero)
  policy.head = x |>
    layer_flatten() |>
    layer_dense(units = 512, activation = "relu", kernel_regularizer = regularizer_l2(0.01)) |>
    layer_dropout(rate = 0.4) |>
    layer_dense(units = 4672, activation = "softmax", name = "policy")
  
  # Crear modelo
  model = keras_model(inputs = inputs, outputs = list(value = value.head, policy = policy.head)) |>
    compile(
      optimizer = optimizer_adam(learning_rate = 0.0001),
      loss = list(value = "mean_squared_error", policy = "categorical_crossentropy"),
      loss_weights = list(value = 1.0, policy = 1.0)
    )
  
  # Datos de modelo
  summary(model)
  return(model)
}

# Funcion para guardar el modelo
save.model <- function(model) {
  # Ruta del archivo en formato .keras
  filepath <- "../resultados/Modelos/modelo_ajedrez_hatchett1.keras"
  
  # Guardar el modelo en formato nativo Keras
  save_model(model, filepath = filepath, overwrite = TRUE)
  
  cat("Modelo guardado correctamente en: ", normalizePath(filepath), "\n")
}

# Funcion para cargar el modelo
call.model <- function() {
  filepath <- "../resultados/Modelos/modelo_ajedrez_hatchett1.keras"
  # Verificar si el archivo existe
  if (!file.exists(filepath)) {
    return(generate.model())
  }
  
  # Cargar el modelo desde el archivo HDF5
  model <- load_model(filepath)
  cat("Modelo cargado desde: ", normalizePath(filepath), "\n")
  return(model)
}