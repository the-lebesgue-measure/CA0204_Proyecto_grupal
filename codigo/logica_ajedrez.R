# logica_ajedrez.R
# Hecho por:
# Anthonny Flores (C32875)
# Andres Zuniga Mora (C38733)
# Amy Chen Wu (C32203)
# Leonardo Vega Aragon (C38313)

# Librerias
library(chess)
library(keras3)
library(processx)

# Source
source("graficos_ajedrez.R")
source("utils.R")

# Funcione para convertir FEN a array 8x8x16 (tensor)
fen.to.vector <- function(fen, num.repetitions = 0) {
  
  # Codificacion minima de 18 planos
  matrixx = array(0, dim = c(8, 8, 18)) # Tensor de entrada
  board.data = strsplit(fen, " ")[[1]] # Datos de FEN
  board = board.data[1] # Tablero
  rows = strsplit(board, "/")[[1]] # Filas
  
  # Modelo estandar
  map.piece = c("P"=1, "N"=2, "B"=3, "R"=4, "Q"=5, "K"=6, "p"=7, "n"=8, "b"=9, "r"=10, "q"=11, "k"=12) # Mapeo de piezas
  
  for (y in 1:8) { # Recorrido de filas
    x = 1 # Columna inicial
    for (char in strsplit(rows[9 - y], "")[[1]]) { # Recorrido de caracteres
      if (grepl("[1-8]", char)) {
        x = x + as.integer(char) # Saltar casillas vacias
      } else {
        piece = map.piece[char] # Obtener la obtener el numero de la pieza
        matrixx[y, x, piece] = 1 # Poner 1 en la posicion de la pieza
        x = x + 1
      }
    }
  }
  
  if (board.data[2] == "w") { # Si es turno blanco
    matrixx[,, 13] = 1 # Llenar canal 13
  }
  
  if (board.data[3] != "-") { # Si hay opcion de enroque
    matrixx[,, 14] = 1 # Llenar canal 14
  }
  
  if (num.repetitions >= 2) { # Si la posicion se repitio 2 o mas veces
    matrixx[,, 15] = 1 # Llenar canal 15
  }
  # Capa constante
  matrixx[,, 16] = 1 # Llenar canal 16
  
  if (length(board.data) >= 5) {
    half.move.clock = as.integer(board.data[5])
    normalized.clock = min(1, half.move.clock / 100.0) 
    matrixx[,, 17] = normalized.clock
  }
  
  if (length(board.data) == 6) {
    full.move.number = as.integer(board.data[6])
    normalized.move = min(1, full.move.number / 200.0) 
    matrixx[,, 18] = normalized.move
  }
  
  array_reshape(matrixx, c(1, 8, 8, 18)) # Batch dim para predict
}

# Monte Carlo Tree Search
mcts.tree <- new.env() # Entorno para el arbol
C.PUCT <- 2.0 # Coeficiente de exploracion MCTS
NUM.SIMULATIONS <- 3200

# Funcion para el Valor UCB (Upper Confidence Bound)
calculate.ucb <- function(node.stats, P.vector, C.puct = C.PUCT) {
  # Q(s, a) + C * P(s, a) * sqrt(sum_b N(s, b)) / (1 + N(s, a))
  Q <- ifelse(node.stats$N > 0, node.stats$W / node.stats$N, 0) # Valor Q promedio
  N.sum <- sum(node.stats$N) # Total de visitas
  UCB <- Q + C.puct * P.vector * sqrt(N.sum) / (1 + node.stats$N) # Formula UCB para seleccionar el nodo a expandir
  return(UCB)
}

# Funcion para el resultado final de la partida
get.game.result.value <- function(game) {
  if (is_game_over(game)) { # Si finalizo la partida
    result <- result(game) # Obtener resultado
    if (result == "1-0") return(1) # Blanco gana
    if (result == "0-1") return(-1) # Negro gana
    return(0) # Empate
  }
  return(NA) # Si el juego no termina devuelve NA
}

# Funcion para aplicar Ruido de Dirichlet
apply.dirichlet.noise <- function(policy.vector, alpha = 0.3, epsilon = 0.25) {
  noise <- rgamma(length(policy.vector), shape = alpha, rate = 1) # Generar ruido con distribucion Gamma
  noise <- noise / sum(noise) # Normalizar ruido
  noisy.policy <- (1 - epsilon) * policy.vector + epsilon * noise # Combina la politica original con el ruido para fomentar exploracion
  return(noisy.policy / sum(noisy.policy)) # Normalizar
}

# Funcion para MCTS 
run.mcts <- function(game, model, num.simulations = NUM.SIMULATIONS, apply.noise = FALSE) {
  
  current.fen <- fen(game)
  input.tensor <- fen.to.vector(current.fen)
  
  prediction <- model(input.tensor) # Obtiene la prediccion de la red neuronal
  P.model <- c(as.numeric(prediction$policy))
  
  # Correccion del valor (Value):
  V.model <- as.numeric(prediction$value) # El valor V.model es la prediccion del valor de la posicion
  
  legal.moves <- moves(game)
  
  # P.MCTS es placeholder aqui, en un MCTS completo seria el resultado de las 3200 simulaciones.
  P.MCTS <- setNames(rep(1/length(legal.moves), length(legal.moves)), legal.moves) # Placeholder de politica que se usaria como vector Pi
  
  if (apply.noise) { # Si se aplica ruido
    P.MCTS <- apply.dirichlet.noise(P.MCTS) # Aplicar ruido
  }
  
  best.action.name <- names(which.max(P.MCTS)) # Mejor movimiento
  
  return(list(
    best.move = best.action.name, # Mejor movimiento
    policy.vector = P.MCTS, # Politica objetivo (vector Pi)
    # CORRECCION: Usar V.model (prediccion de la NN) en lugar de las variables eliminadas
    value = V.model # Q(s, a)
  ))
}

# Funcion para corregir y elegir el mejor movimiento
best.move <- function(game, model, history.positions = NULL, move.count = 0) {
  
  moves.list = moves(game) # Movimientos legales
  if (length(moves.list) == 0) return(NULL) # Si no hay movimientos
  
  noise.enabled <- FALSE # No habilitamos ruido
  mcts.result = run.mcts(game, model, apply.noise = noise.enabled) # Ejecutar MCTS
  P.MCTS <- mcts.result$policy.vector # Obtener vector Pi
  
  
  # Normalizar despues de la penalizacion
  if (sum(P.MCTS) > 0) { # Si la suma es positiva
    P.MCTS = P.MCTS / sum(P.MCTS) # Normalizar
  } else { # Si la suma es cero
    P.MCTS = setNames(rep(1/length(moves.list), length(moves.list)), moves.list) # Elegir al azar
  }
  
  #  Aplicar Temperatura
  temperature <- 0.01 # Temperatura baja para determinismo
  best.move = names(which.max(P.MCTS)) # Selecciona el movimiento mas probable para explotacion
  
  return(best.move)
}

# Funciona para auto entrenamiento
bot.vs.bot.game <- function(model ,games.data, games.heavy.data){
  history.positions = character() # Historial de posiciones FEN
  max.jugadas = 240 # Limite de jugadas
  game = game() # Partida nueva
  num.moves = 0 # Contador de movimientos
  moves = character() # Lista de movimientos
  move.times <- numeric() # Tiempos de movimiento
  MCTS.policies <- list()
  
  while (!is_game_over(game) && num.moves < max.jugadas) { # Bucle principal de la partida
    
    current.fen <- fen(game) # FEN actual
    history.positions = c(history.positions, current.fen) # Añadir a historial
    num.moves = num.moves + 1 # Aumentar contador
    
    start.time <- Sys.time() # Registrar tiempo
    
    # Ejecutar MCTS
    noise.enabled <- (num.moves < 30) # Ruido en las primeras 30
    mcts.data = run.mcts(game, model, apply.noise = noise.enabled) # Ejecutar MCTS con ruido si es necesario
    P.MCTS <- mcts.data$policy.vector # Obtener vector Pi
    
    
    # Normalizar P.MCTS
    # Si la suma es 0, es un caso extremo y se elige al azar
    moves.list = moves(game)
    if (sum(P.MCTS) > 0) { 
      P.MCTS = P.MCTS / sum(P.MCTS) # Normalizar para que sume 1
    } else { 
      P.MCTS = setNames(rep(1/length(moves.list), length(moves.list)), moves.list) # Elegir al azar
    }
    MCTS.policies[[num.moves]] <- P.MCTS # GUARDAR EL VECTOR PI CORREGIDO
    
    # Aplicar Temperatura
    temperature <- if (num.moves < 15) 1.0 else 0.01 # Temperatura alta para exploracion, baja para explotacion
    
    if (temperature > 0.1) { # Muestreo estocastico
      P.MCTS.T <- (P.MCTS ^ (1/temperature)) / sum(P.MCTS ^ (1/temperature)) # Aplicar temperatura
      move = sample(names(P.MCTS.T), size = 1, prob = P.MCTS.T) # Muestrear movimiento basado en probabilidades
    } else { # Modo determinista
      move = names(which.max(P.MCTS)) # Selecciona el movimiento con la probabilidad mas alta
    }
    
    end.time <- Sys.time() # Registrar tiempo final
    move.times[num.moves] <- as.numeric(difftime(end.time, start.time, units = "secs")) # Tiempo de movimiento
    
    if (is.null(move)) { cat("Salida por movimientos nulos"); break } # Control de nulo
    cat("\nMovimiento: ", move, " numero ", num.moves, "\n") # Imprimir movimiento
    
    game = move(game, move) # Realizar movimiento
    moves[num.moves] = move # Guardar movimiento
    print.chess.board(game) # Deshabilitado para partidas rapidas
  }
  
  final.result.value <- get.game.result.value(game)
  
  # Recortar moves y move.times al tamaño real
  moves = moves[1:num.moves] # Recortar movimientos
  move.times.median = median(move.times) # Mediana de tiempos
  
  # Guardar datos pesados. Se usa turn(game) para determinar quien era el jugador en la ultima posicion.
  last.player.color = if (turn(game) == "white") "black" else "white"
  games.heavy.data = df.heavy.game.data(games.heavy.data, game, "bot_vs_bot_interno", moves, num.moves, move.times.median, last.player.color, history.positions = history.positions, final.result = final.result.value) # Guardar datos pesados para el entrenamiento
  games.data = df.game.data(games.data, game, "bot_vs_bot_interno", num.moves, "Hatchet1", "Hatchet1") # Guardar datos ligeros
  
  # print(games.data) # Deshabilitar en produccion
  # print(games.heavy.data) # Deshabilitar en produccion
  
  return(list(
    games.data = games.data, # Retornar datos ligeros
    games.heavy.data = games.heavy.data # Retornar datos pesados
  ))
}


bot.vs.player.game = function(model, games.data, games.heavy.data){
  max.jugadas = 200
  game = game()
  mov = 0
  moves = character()
  
  # Eleccion de color
  repeat {
    color = tolower(readline("Desea jugar como blancas o negras : "))
    if (color %in% c("blancas", "negras")) break
    cat("Escriba 'blancas' o 'negras'\n")
  }
  
  jugador.blancas = (color == "blancas") # TRUE si el jugador humano es blanco
  
  while (!is_game_over(game) && mov < max.jugadas) {
    print.chess.board(game)
    turno.blancas = (turn(game) == "white") # TRUE si es turno de las blancas
    
    if (turno.blancas == jugador.blancas) { # Turno del jugador humano
      moves.list = moves(game)
      cat("Movimientos posibles: ", paste(moves.list, collapse = ", "), "\n")
      
      repeat {
        move = readline("Su movimiento (e2e4): ")
        if (move %in% moves.list) break
        cat("Solo movimientos legales\n")
      }
      
      game = move(game, move)
    } else { # Turno de la IA
      cat("\nTurno de la IA\n")
      
      move = best.move(game, model)  # La IA elige el mejor movimiento
      
      if (is.null(move)) {
        cat("IA sin movimiento valido, fin de partida\n")
        break
      }
      
      cat("IA juega: ", move, "\n")
      game = move(game, move)
    }
    
    mov = mov + 1
    moves[mov] = move
  }
  
  # Recortar moves y move.times al tamaño real
  moves = moves[1:mov]
  move.times = 0 # Implementar contador
  
  games.heavy.data = df.heavy.game.data(games.heavy.data, game, "bot_vs_bot_interno", moves, mov, move.times, "white", history.positions = history.positions, final.result = get.game.result.value(game)) 
  games.data = df.game.data(games.data, game, "bot_vs_bot_interno", mov, "Hatchet1", "Hatchet1")
  
  # Control de datos, eliminar en entrega
  print(games.data)
  
  return(list(
    games.data = games.data,
    games.heavy.data = games.heavy.data
  ))
}

bot.vs.external.game <- function(model, games.data, games.heavy.data){
  max.jugadas = 200 # Limite de movimientos
  game = game() # Partida nueva
  mov = 0 # Contador de movimientos
  moves = character() # Lista de movimientos
  history.positions = character() # Historial de FENs
  
  
  if (!exists("iniciar.stockfish") || !exists("obtener.movimiento.stockfish")) {
    stop("Las funciones de Stockfish no estan disponibles. Asegurese de haber cargado 'stock.fish.R'.")
  }
  
  engine <- NULL # Inicializar la variable engine
  tryCatch({
    engine <- iniciar.stockfish("stockfish.exe")
  }, error = function(e) {
    stop(paste("Error al iniciar Stockfish:", e$message))
  })
  
  
  ia.interna.color <- sample(c("white", "black"), size = 1) # Selecciona el color de la IA interna (Hatchet1) al azar
  cat("\nIA INTERNA JUEGA COMO:", toupper(ia.interna.color), "--- \n")
  
  
  while (!is_game_over(game) && mov < max.jugadas) {
    print.chess.board(game)
    current.fen <- fen(game)
    history.positions = c(history.positions, current.fen)
    
    # CORRECCION: Convertir el valor logico de turn(game) a string
    turno.bool = turn(game)
    # Si turno.bool es FALSE (blancas) -> "white"
    # Si turno.bool es TRUE (negras) -> "black"
    turno.actual = ifelse(turno.bool, "black", "white") # Determina el color del jugador actual
    
    mov = mov + 1
    
    start.time <- Sys.time()
    
    if (turno.actual == ia.interna.color) {
      
      cat("\nTurno de la IA Interna (", ia.interna.color, ")\n")
      # Asumo que best.move devuelve una jugada simple, ya que no estamos en modo Self-Play
      # Si best.move devuelve una lista, debe ser ajustado a move <- best.move(game, model)$move
      move <- best.move(game, model) # La IA interna elige el movimiento
      ia.name = "Hatchet1"
      
    } else {
      
      cat("\nTurno de la IA Externa (Stockfish - ", turno.actual, ")\n")
      
      moves.list <- moves(game)
      move <- NULL # Inicializar move
      
      tryCatch({
        
        # Pedir mejor jugada a Stockfish
        jugada_stockfish <- obtener.movimiento.stockfish(
          engine,
          current.fen,
          profundidad_stockfish = 10,
          tiempo_stockfish_ms = 200
        )
        
        # Asignar el resultado
        move <- jugada_stockfish
        
        # Si no devuelve jugada, o no esta en la lista, usar primera legal
        if (is.na(move) || !(move %in% moves.list)) { # Si Stockfish falla o devuelve ilegal, usa el primer movimiento valido
          move <- moves.list[1]
        }
        
      }, error = function(e) {
        warning(paste("Error al obtener movimiento de Stockfish:", e$message, " - El juego termina."))
        move <- NULL # Asegurar que move sea NULL si hay error
      })
      
      ia.name = "Stockfish"
    }
    
    end.time <- Sys.time()
    move.time <- as.numeric(difftime(end.time, start.time, units = "secs"))
    
    if (is.null(move) || !move %in% moves(game)) { # Control de movimiento nulo o ilegal antes de aplicar
      cat("Fin de partida: IA sin movimiento valido o legal.\n")
      break
    }
    
    cat(ia.name, " juega: ", move, " (Tiempo: ", round(move.time, 2), "s)\n")
    game = move(game, move)
    moves[mov] = move
  }
  
  # Asegurar el cierre del motor al salir de la funcion
  on.exit({
    if (!is.null(engine)) {
      engine$kill()
      cat("\nMotor Stockfish cerrado.\n")
    }
  })
  
  moves = moves[1:mov]
  final.result.value <- get.game.result.value(game)
  
  # Datos Pesados (ajustar la mediana de tiempo si se implementa un vector de tiempos)
  move.times.median = NA # Simplificado, ya que solo se guardara una partida por ahora
  games.heavy.data = df.heavy.game.data(games.heavy.data, game, "bot_vs_external", moves, mov, move.times.median, ia.interna.color, history.positions = history.positions, final.result = final.result.value)
  
  # Datos Ligeros
  white_player = if (ia.interna.color == "white") "Hatchet1" else "Stockfish"
  black_player = if (ia.interna.color == "black") "Hatchet1" else "Stockfish"
  games.data = df.game.data(games.data, game, "bot_vs_external", mov, white_player, black_player)
  
  return(list(
    games.data = games.data,
    games.heavy.data = games.heavy.data
  ))
}

# Funcion para jugar "n" partidas bot.vs.bot.game
bot.vs.bot <- function(model, games.data, games.heavy.data){
  
  option = readline("Ingrese el numero de autoentrenamientos que desea: \n")
  repeat {
    if (!is.na(as.numeric(option))){
      if(as.integer(option) == option && option > 0)break
    }
    option = readline("Por favor digite un numero entero positivo\n")
  }
  option = as.numeric(option)
  # Partidas iteradas
  while(option>0){
    result = bot.vs.bot.game(model, games.data, games.heavy.data)
    games.data = result$games.data
    games.heavy.data = result$games.heavy.data
    option = option - 1
  }
  
  # Edicion no referenciada
  games.data <<- games.data
  games.heavy.data <<- games.heavy.data
  print
}

# Funcion para jugar "n" partidas bot.vs.player.game
bot.vs.player <- function(model, games.data, games.heavy.data){
  
  option = readline("Ingrese el número de partidas que desea jugar: \n")
  repeat {
    if (!is.na(as.numeric(option))){
      if(as.integer(option) == option && option > 0) break
    }
    option = readline("Por favor digite un número entero positivo\n")
  }
  
  # Partidas iteradas
  while(option > 0){
    result = bot.vs.player.game(model, games.data, games.heavy.data)
    games.data = result$games.data
    games.heavy.data = result$games.heavy.data
    option = option - 1
  }
  
  # Edición no referenciada
  games.data <<- games.data
  games.heavy.data <<- games.heavy.data
}

#$
bot.vs.external <- function(model, games.data, games.heavy.data){
  
  option = readline("Ingrese el número de partidas bot.vs.external que desea jugar: \n")
  repeat {
    if (!is.na(as.numeric(option))){
      if(as.integer(option) == option && option > 0) break
    }
    option = readline("Por favor digite un número entero positivo\n")
  }
  option = as.numeric(option)
  
  # Partidas iteradas
  while(option > 0){
    result = bot.vs.external.game(model, games.data, games.heavy.data)
    games.data = result$games.data
    games.heavy.data = result$games.heavy.data
    option = option - 1
  }
  
  # Edición no referenciada (actualiza las variables globales/entorno superior)
  games.data <<- games.data
  games.heavy.data <<- games.heavy.data
  
  cat("Finalizado el conjunto de partidas bot.vs.external.\n")
}
