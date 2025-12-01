# red_neuronal_ajedrez.R
# Hecho por:
# Anthonny Flores (C32875)
# Andres Zuniga Mora (C38733)
# Amy Chen Wu (C32203)
# Leonardo Vega Aragon (C38313)

# Libreria
library(keras3)

# Funcion para definir bloque residual
residual.block <- function(x, filters) {
  shortcut = x
  x = x |>
    layer_conv_2d(filters = filters, kernel_size = c(3, 3), padding = "same", use_bias = FALSE) |>
    layer_batch_normalization() |>
    layer_activation(activation = "relu") |>
    layer_conv_2d(filters = filters, kernel_size = c(3, 3), padding = "same", use_bias = FALSE) |>
    layer_batch_normalization()
  
  x = layer_add(inputs = list(shortcut, x))
  x = layer_activation(activation = "relu")(x)
  return(x)
}

# Funcion para hacer el modelo
generate.model <- function(num.residual.blocks = 20, conv.filters = 256) { # Modelo "Hatchet1"
  # $ Se asume que 4672 es la salida de movimientos legales (64x73)
  num.policy.units = 4672
  
  # Definir modelo
  inputs = layer_input(shape = c(8, 8, 18))
  
  # Capa Inicial
  x = inputs |>
    layer_conv_2d(filters = conv.filters, kernel_size = c(3, 3), padding = "same", use_bias = FALSE, kernel_regularizer = regularizer_l2(0.0001)) |>
    layer_batch_normalization() |>
    layer_activation(activation = "relu")
  
  # Bloques residuales
  for (i in 1:num.residual.blocks) {
    x = residual.block(x, conv.filters)
  }
  
  # Cabeza de valor
  value.head = x |>
    # Capa de Compresion
    layer_conv_2d(filters = 32, kernel_size = c(1, 1), use_bias = FALSE) |>
    layer_batch_normalization() |>
    layer_activation(activation = "relu") |>
    layer_flatten() |>
    
    # Capa Densa Final
    layer_dense(units = 128, activation = "relu", kernel_regularizer = regularizer_l2(0.0001)) |> #* Capa densa intermedia
    layer_dense(units = 1, activation = "tanh", name = "value")
  
  # Cabeza de politica
  policy.head = x |>
    # Capa de Compresion 1 (Aumentada)
    layer_conv_2d(filters = conv.filters, kernel_size = c(3, 3), padding = "same", use_bias = FALSE) |>
    layer_batch_normalization() |>
    layer_activation(activation = "relu") |>
    
    # Capa de Compresion 2 (Ajustada a 73)
    layer_conv_2d(filters = 73, kernel_size = c(1, 1), use_bias = FALSE) |> 
    layer_flatten() |>
    
    # Capa Densa Final
    layer_dense(units = num.policy.units, activation = "softmax", name = "policy")
  
  # Crear modelo y compilar
  model = keras_model(inputs = inputs, outputs = list(value = value.head, policy = policy.head)) |>
    compile(
      optimizer = optimizer_adam(learning_rate = 0.0001), #* Optimizador Adam
      loss = list(value = "mean_squared_error", policy = "categorical_crossentropy"),
      loss_weights = list(value = 1.0, policy = 1.0)
    )
  
  summary(model)
  return(model)
}

# Funcion para guardar el modelo
save.model <- function(model) {
  filepath <- "../resultados/Modelos/modelo_ajedrez_hatchett1.keras" # Ruta de archivo
  save_model(model, filepath = filepath, overwrite = TRUE) # Guardar modelo
  cat("Modelo guardado correctamente en: ", normalizePath(filepath), "\n") # Confirmacion
}

# Funcion para cargar el modelo
call.model <- function() {
  filepath <- "../resultados/Modelos/modelo_ajedrez_hatchett1.keras" # Ruta de archivo
  if (!file.exists(filepath)) { # Si no existe
    return(generate.model()) # Generar modelo nuevo
  }
  model <- load_model(filepath) # Cargar modelo
  cat("Modelo cargado desde: ", normalizePath(filepath), "\n") # Confirmacion
  return(model)
}