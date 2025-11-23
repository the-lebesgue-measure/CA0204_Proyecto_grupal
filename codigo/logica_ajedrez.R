# logica_ajedrez.R
# Hecho por:
# Anthonny Flores (C32875)
# Andres Zuñiga Mora (C38733)
# Amy Chen Wu (C32203)
# Leonardo Vega Aragon (C38313)

# Librerias
library(chess)
library(keras3)

# Source
source("graficos_ajedrez.R") 



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
    matrixx[,, 15] = 1  # Llenar canal 15
  }
  # Capa constante
  matrixx[,, 16] = 1 # Llenar canal 16
  
  array_reshape(matrixx, c(1, 8, 8, 16)) # Batch dim para predict
}

# Monte Carlo Tree Search
mcts.tree <- new.env() # Entorno para el arbol
C.PUCT <- 2.0 #* Coeficiente de exploracion MCTS
NUM.SIMULATIONS <- 3200 #*



get.node <- function(fen) {
  if (!exists(fen, envir = mcts.tree)) {
    assign(fen, list(
      N = NULL,
      W = NULL,
      P = NULL,
      children = NULL
    ), envir = mcts.tree)
  }
  get(fen, envir = mcts.tree)
}


expand.node <- function(fen, game, model) {
  
  input.tensor <- fen.to.vector(fen)
  pred <- model |> predict(input.tensor)
  
  P.vector <- as.numeric(pred[[1]])
  value    <- as.numeric(pred[[2]])
  
  legal.moves <- moves(game)
  names(P.vector) <- model$move_labels
  
  P.legal <- P.vector[legal.moves]
  P.legal <- P.legal / sum(P.legal)
  
  N <- setNames(rep(0, length(legal.moves)), legal.moves)
  W <- setNames(rep(0, length(legal.moves)), legal.moves)
  children <- setNames(rep(NA, length(legal.moves)), legal.moves)
  
  node <- list(N=N, W=W, P=P.legal, children=children)
  assign(fen, node, envir = mcts.tree)
  
  return(list(node=node, value=value))
}


choose.move.ucb <- function(node) {
  Q <- ifelse(node$N > 0, node$W / node$N, 0)
  N.sum <- sum(node$N)
  UCB <- Q + C.PUCT * node$P * sqrt(N.sum + 1e-8) / (1 + node$N)
  names(which.max(UCB))
}




backup <- function(path, value) {
  v <- value
  for (entry in rev(path)) {
    fen  <- entry$fen
    move <- entry$move
    node <- get.node(fen)
    
    node$N[move] <- node$N[move] + 1
    node$W[move] <- node$W[move] + v
    
    assign(fen, node, envir = mcts.tree)
    v <- -v
  }
}



run.mcts <- function(game, model, num.simulations = NUM.SIMULATIONS, apply.noise = FALSE) {
  
  root.fen <- fen(game)
  rm(list = ls(envir = mcts.tree), envir = mcts.tree)
  
  expanded <- expand.node(root.fen, game, model)
  root <- expanded$node
  
  if (apply.noise) {
    root$P <- apply.dirichlet.noise(root$P)
    assign(root.fen, root, envir = mcts.tree)
  }
  
  for (sim in 1:num.simulations) {
    game.sim <- game
    fen.sim <- root.fen
    path <- list()
    
    while (TRUE) {
      node <- get.node(fen.sim)
      
      if (is.null(node$N)) break
      
      move <- choose.move.ucb(node)
      path[[length(path)+1]] <- list(fen=fen.sim, move=move)
      
      game.sim <- move(game.sim, move)
      fen.sim <- fen(game.sim)
      
      if (!exists(fen.sim, envir = mcts.tree)) break
    }
    
    expanded <- expand.node(fen.sim, game.sim, model)
    value <- expanded$value
    
    backup(path, value)
  }
  
  root <- get.node(root.fen)
  P.final <- root$N / sum(root$N)
  best.move <- names(which.max(P.final))
  
  return(list(
    best.move = best.move,
    policy.vector = P.final,
    value = sum(root$W) / sum(root$N)
  ))
}




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
  if (is_game_over(game)) { # Si finalizo la partida
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
      
      if (rep.count >= 3) { # Si va a repetirse por tercera vez o mas
        # Penalización EXTREMA (0.001) para evitar bucles
        P.MCTS[move.name] = P.MCTS[move.name] - 0.7
      }
    }
    # Normalizar despues de la penalizacion
    if (sum(P.MCTS) > 0) { # Si la suma es positiva
      P.MCTS = P.MCTS / sum(P.MCTS) # Normalizar
    } else { # Si la suma es cero
      P.MCTS = setNames(rep(1/length(moves.list), length(moves.list)), moves.list) # Elegir al azar
    }
  }
  
  #  Aplicar Temperatura
  temperature <- 0.01 # Temperatura baja para determinismo
  best.move = names(which.max(P.MCTS)) # Movimiento con mas probabilidad
  
  return(best.move)
}


bot.vs.bot <- function(model ,games.data, games.heavy.data) {
  
  history.positions <- character()
  max.jugadas <- 240
  game <- game()
  num.moves <- 0
  moves <- character()
  move.times <- numeric()
  MCTS.policies <- list()
  
  while (!is_game_over(game) && num.moves < max.jugadas) {
    
    current.fen <- fen(game)
    history.positions <- c(history.positions, current.fen)
    num.moves <- num.moves + 1
    
    start.time <- Sys.time()
    
    noise.enabled <- (num.moves < 30)
    mcts <- run.mcts(game, model, apply.noise=noise.enabled)
    P <- mcts$policy.vector
    
    legal <- moves(game)
    for (mv in legal) {
      next.fen <- fen(move(game, mv))
      reps <- sum(history.positions == next.fen)
      if (reps >= 2) {
        P[mv] <- P[mv] * (0.001^(reps - 1))
      }
    }
    
    if (sum(P) > 0) P <- P / sum(P)
    else P <- setNames(rep(1/length(legal), length(legal)), legal)
    
    MCTS.policies[[num.moves]] <- P
    
    temperature <- if (num.moves < 15) 1.0 else 0.01
    
    if (temperature > 0.1) {
      P.T <- (P^(1/temperature)) / sum(P^(1/temperature))
      mv <- sample(names(P.T), 1, prob=P.T)
    } else {
      mv <- names(which.max(P))
    }
    
    end.time <- Sys.time()
    move.times[num.moves] <- as.numeric(difftime(end.time, start.time, units="secs"))
    
    game <- move(game, mv)
    moves[num.moves] <- mv
    print.chess.board(game)
  }
  
  final.value <- get.game.result.value(game)
  moves <- moves[1:num.moves]
  median.time <- median(move.times)
  
  games.heavy.data <- df.heavy.game.data(
    games.heavy.data, game, "bot_vs_bot",
    moves, num.moves, median.time, "white",
    history.positions, final.value
  )
  
  games.data <- df.game.data(
    games.data, game, "bot_vs_bot",
    num.moves, "Hatchet1", "Hatchet1"
  )
  
  return(list(
    games.data = games.data,
    games.heavy.data = games.heavy.data
  ))
}


