# logica_ajedrez.R
# Hecho por:
# Anthonny Flores (C32875)
# Andres Zuniga Mora (C38733)
# Amy Chen Wu (C32203)
# Leonardo Vega Aragon (C38313)

# Librerias
library(chess)
library(keras3)

# Source
source("graficos_ajedrez.R")
source("stock_fish.R")

# Funcione para convertir FEN a array 8x8x16 (tensor)
fen.to.vector <- function(fen, num.repetitions = 0) {
  
  # Codificación minima de 16 planos
  matrixx = array(0, dim = c(8, 8, 16)) # Tensor de entrada
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
  
  array_reshape(matrixx, c(1, 8, 8, 16)) # Batch dim para predict
}

# Monte Carlo Tree Search
mcts.tree <- new.env() # Entorno para el arbol
C.PUCT <- 2.0 #* Coeficiente de exploracion MCTS
NUM.SIMULATIONS <- 3200 #*

# Funcion para el Valor UCB (Upper Confidence Bound)
calculate.ucb <- function(node.stats, P.vector, C.puct = C.PUCT) {
  # Q(s, a) + C * P(s, a) * sqrt(sum_b N(s, b)) / (1 + N(s, a))
  Q <- ifelse(node.stats$N > 0, node.stats$W / node.stats$N, 0) # Valor Q promedio
  N.sum <- sum(node.stats$N) # Total de visitas
  UCB <- Q + C.puct * P.vector * sqrt(N.sum) / (1 + node.stats$N) # Calculo UCB
  return(UCB)
}

# Funcion para el resultado final de la partida
get.game.result.value <- function(game) {
  if (is_game_over(game)) { # Si finalizo la partida <<<< CORREGIDO
    result <- result(game) # Obtener resultado
    if (result == "1-0") return(1) # Blanco gana
    if (result == "0-1") return(-1) # Negro gana
    return(0) # Empate
  }
  return(NA)
}

# Funcion para aplicar Ruido de Dirichlet
apply.dirichlet.noise <- function(policy.vector, alpha = 0.3, epsilon = 0.25) {
  noise <- rgamma(length(policy.vector), shape = alpha, rate = 1) # Generar ruido
  noise <- noise / sum(noise) # Normalizar ruido
  noisy.policy <- (1 - epsilon) * policy.vector + epsilon * noise # Aplicar ruido
  return(noisy.policy / sum(noisy.policy)) # Normalizar
}

# Funcion para MCTS 
run.mcts <- function(game, model, num.simulations = NUM.SIMULATIONS, apply.noise = FALSE) {
  
  rm(list = ls(envir = mcts.tree), envir = mcts.tree) # Reiniciar arbol
  
  # Valores simulados
  legal.moves <- moves(game) # Movimientos legales
  N.simulated <- setNames(rep(1, length(legal.moves)), legal.moves) # Simulacion de visitas
  W.simulated <- setNames(rnorm(length(legal.moves), mean = 0, sd = 0.5), legal.moves) # Simulacion de resultados
  
  P.MCTS <- N.simulated / sum(N.simulated) # Vector de politica basado en visitas
  
  if (apply.noise) { # Si se aplica ruido
    P.MCTS <- apply.dirichlet.noise(P.MCTS) # Aplicar ruido
  }
  
  best.action.name <- names(which.max(P.MCTS)) # Mejor movimiento
  
  return(list(
    best.move = best.action.name, # Mejor movimiento
    policy.vector = P.MCTS, # Politica objetivo
    value = W.simulated[best.action.name] / N.simulated[best.action.name] # Q(s, a)
  ))
}

# Funcion para corregir y elegir el mejor movimiento
best.move <- function(game, model, history.positions = NULL, move.count = 0) {
  
  moves.list = moves(game) # Movimientos legales
  if (length(moves.list) == 0) return(NULL) # Si no hay movimientos
  
  noise.enabled <- FALSE # No habilitamos ruido
  mcts.result = run.mcts(game, model, apply.noise = noise.enabled) # Ejecutar MCTS
  P.MCTS <- mcts.result$policy.vector # Obtener vector Pi
  
  if (!is.null(history.positions)) { # Si existe historial
    for (move.name in moves.list) { # Iterar movimientos
      next.fen = fen(move(game, move.name)) # FEN siguiente
      rep.count = sum(history.positions == next.fen) # Contar repeticiones
      
      if (rep.count >= 2) { # Si va a repetirse por tercera vez o mas
        # Penalización EXTREMA (0.001) para evitar bucles
        P.MCTS[move.name] = P.MCTS[move.name] * (0.001^(rep.count - 1))
      }
    }
    # Normalizar despues de la penalizacion
    if (sum(P.MCTS) > 0) { # Si la suma es positiva
      P.MCTS = P.MCTS / sum(P.MCTS) # Normalizar
    } else { # Si la suma es cero
      P.MCTS = setNames(rep(1/length(moves.list), length(moves.list)), moves.list) # Elegir al azar
    }
  }
  
  #  Aplicar Temperatura
  temperature <- 0.01 # Temperatura baja para determinismo
  best.move = names(which.max(P.MCTS)) # Movimiento con mas probabilidad
  
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
  
  while (!is_game_over(game) && num.moves < max.jugadas) { # <<<< CORREGIDO
    
    current.fen <- fen(game) # FEN actual
    history.positions = c(history.positions, current.fen) # Añadir a historial
    num.moves = num.moves + 1 # Aumentar contador
    
    start.time <- Sys.time() # Registrar tiempo
    
    # Ejecutar MCTS
    noise.enabled <- (num.moves < 30) # Ruido en las primeras 30
    mcts.data = run.mcts(game, model, apply.noise = noise.enabled) # Ejecutar MCTS
    P.MCTS <- mcts.data$policy.vector # Obtener vector Pi
    
    # APLICAR PENALIZACIÓN DE REPETICIÓN
    moves.list = moves(game) # Movimientos legales
    for (move.name in moves.list) { # Iterar movimientos
      next.fen = fen(move(game, move.name)) # FEN siguiente
      rep.count = sum(history.positions == next.fen) # Contar repeticiones
      
      if (rep.count >= 2) { # Si va a repetirse por tercera vez o mas
        P.MCTS[move.name] = P.MCTS[move.name] * (0.001^(rep.count - 1)) # Penalizacion extrema
      }
    }
    
    # Normalizar P.MCTS después de la penalización
    if (sum(P.MCTS) > 0) { # Si la suma es positiva
      P.MCTS = P.MCTS / sum(P.MCTS) # Normalizar
    } else { # Si la suma es cero
      P.MCTS = setNames(rep(1/length(moves.list), length(moves.list)), moves.list) # Elegir al azar
    }
    MCTS.policies[[num.moves]] <- P.MCTS # GUARDAR EL VECTOR PI CORREGIDO
    
    # Aplicar Temperatura
    temperature <- if (num.moves < 15) 1.0 else 0.01 # Temperatura para exploracion/explotacion
    
    if (temperature > 0.1) { # Muestreo estocastico
      P.MCTS.T <- (P.MCTS ^ (1/temperature)) / sum(P.MCTS ^ (1/temperature)) # Aplicar temperatura
      move = sample(names(P.MCTS.T), size = 1, prob = P.MCTS.T) # Muestrear movimiento
    } else { # Modo determinista
      move = names(which.max(P.MCTS)) # Movimiento con mas probabilidad
    }
    
    end.time <- Sys.time() # Registrar tiempo final
    move.times[num.moves] <- as.numeric(difftime(end.time, start.time, units = "secs")) # Tiempo de movimiento
    
    if (is.null(move)) { cat("Salida por movimientos nulos"); break } # Control de nulo
    cat("\nMovimiento: ", move, " numero ", num.moves, "\n") # Imprimir movimiento
    
    game = move(game, move) # Realizar movimiento
    moves[num.moves] = move # Guardar movimiento
    print.chess.board(game) # Imprimir tablero
  }
  
  final.result.value <- get.game.result.value(game)
  
  # Recortar moves y move.times al tamaño real
  moves = moves[1:num.moves] # Recortar movimientos
  move.times.median = median(move.times) # Mediana de tiempos
  
  games.heavy.data = df.heavy.game.data(games.heavy.data, game, "bot_vs_bot_interno", moves, num.moves, move.times.median, "white", history.positions = history.positions, final.result = final.result.value) # Guardar datos pesados
  games.data = df.game.data(games.data, game, "bot_vs_bot_interno", num.moves, "Hatchet1", "Hatchet1") # Guardar datos ligeros
  
  print(games.data) # Imprimir datos ligeros
  print(games.heavy.data) # Imprimir datos pesados
  
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
  
  jugador.blancas = (color == "blancas")
  
  while (!is_game_over(game) && mov < max.jugadas) {
    print.chess.board(game)
    turno.blancas = (turn(game) == "white")
    
    if (turno.blancas == jugador.blancas) {
      moves.list = moves(game)
      cat("Movimientos posibles: ", paste(moves.list, collapse = ", "), "\n")
      
      repeat {
        move = readline("Su movimiento (e2e4): ")
        if (move %in% moves.list) break
        cat("Solo movimientos legales\n")
      }
      
      game = move(game, move)
    } else {
      cat("\nTurno de la IA\n")
      
      move = best.move(game, model) 
      
      if (is.null(move)) {
        cat("IA sin movimiento válido, fin de partida\n")
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

#$
bot.vs.external.game <- function(model, games.data, games.heavy.data){
  max.jugadas = 200 # Límite de movimientos
  game = game() # Partida nueva
  mov = 0 # Contador de movimientos
  moves = character() # Lista de movimientos
  history.positions = character() # Historial de FENs
  
  
  ia.interna.color <- sample(c("white", "black"), size = 1) 
  cat("\nIA INTERNA JUEGA COMO:", toupper(ia.interna.color), "--- \n")
  ia.interna.color = "white"
  
  # Asegurar que la función externa está cargada (asumiendo que se llamó a source("stock.fish.R") previamente)
  if (!exists("move.stockfish")) {
    stop("La función 'move.stockfish' no está disponible. Asegúrese de haber cargado 'stock.fish.R'.")
  }
  
  # --- Bucle de la Partida ---
  while (!is_game_over(game) && mov < max.jugadas) {
    print.chess.board(game)
    current.fen <- fen(game)
    history.positions = c(history.positions, current.fen)
    
    turno.actual = turn(game)
    mov = mov + 1
    
    start.time <- Sys.time()
    
    if (turno.actual == ia.interna.color) {
      # 1. Turno de la IA INTERNA (usando el 'model' y la función 'best.move' o MCTS)
      cat("\nTurno de la IA Interna (", ia.interna.color, ")\n")
      move <- best.move(game, model) # Asumiendo que 'best.move' llama al MCTS/modelo
      ia.name = "Hatchet1"
      
    } else {
      # 2. Turno de la IA EXTERNA (Stockfish)
      cat("\nTurno de la IA Externa (Stockfish - ", turno.actual, ")\n")
      
      moves.list <- moves(game)
      
      tryCatch({
        # Llama a la función externa para obtener la mejor jugada
        move <- move.stockfish(current.fen, moves.list) 
      }, error = function(e) {
        warning(paste("Error al llamar a move.stockfish:", e$message, " - El juego termina."))
        move <- NULL
      })
      
      ia.name = "Stockfish"
    }
    
    end.time <- Sys.time()
    move.time <- as.numeric(difftime(end.time, start.time, units = "secs"))
    
    # --- Aplicar el Movimiento ---
    if (is.null(move) || !move %in% moves(game)) {
      cat("Fin de partida: IA sin movimiento válido o legal.\n")
      break
    }
    
    cat(ia.name, " juega: ", move, " (Tiempo: ", round(move.time, 2), "s)\n")
    game = move(game, move)
    moves[mov] = move
  }
  
  # --- Guardar Resultados ---
  moves = moves[1:mov]
  final.result.value <- get.game.result.value(game)
  
  # Datos Pesados (ajustar la mediana de tiempo si se implementa un vector de tiempos)
  move.times.median = NA # Simplificado, ya que solo se guardará una partida por ahora
  games.heavy.data = df.heavy.game.data(games.heavy.data, game, "bot_vs_external", moves, mov, move.times.median, ia.interna.color, history.positions = history.positions, final.result = final.result.value)
  
  # Datos Ligeros
  white_player = if (ia.interna.color == "white") "Hatchet1" else "Stockfish"
  black_player = if (ia.interna.color == "black") "Hatchet1" else "Stockfish"
  games.data = df.game.data(games.data, game, "bot_vs_external", mov, white_player, black_player)
  
  print(games.data[nrow(games.data), ])
  
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
