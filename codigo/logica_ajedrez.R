# logica_ajedrez.R
# Hecho por:
# Anthonny Flores (C32875)
# Leonardo Vega Aragon (C38313)
# Andres Zuniga Mora (C38733)
# Amy Chen Wu (C32203)


# Librerias
library(chess)
library(keras3)
library(processx)

# Source
source("graficos_ajedrez.R")
source("utils.R")

mcts.tree = new.env() # Entorno (hash map/diccionario) global para almacenar y acceder a las estadisticas de los nodos del arbol MCTS.
C.PUCT = 2.0 # Coeficiente de exploracion MCTS (controla el equilibrio exploracion/explotacion en PUCT)
NUM.SIMULATIONS = 20 # Numero de simulaciones MCTS por movimiento

# Constante renombrada a safety.threshold
safety.threshold = 1e-4 # Umbral de seguridad: 0.0001

# Funcion para calcular el Valor UCB/PUCT (Upper Confidence Bound applied to Trees)
calculate.ucb = function(Q, P, N.sum, N.current, C.puct = C.PUCT) {
  # Formula PUCT Q(s,a) + C * P(s,a) * sqrt(N_total) / (1 + N(s,a))
  # Q Valor promedio de la accion (W/N)
  # P Probabilidad Priorizada de la NN
  # N.sum Total de visitas al nodo padre
  # N.current Visitas de la accion actual
  UCB = Q + C.puct * P * sqrt(N.sum) / (1 + N.current)
  return(UCB)
}

# Funcion para aplicar Ruido de Dirichlet
apply.dirichlet.noise = function(policy.vector, alpha = 0.3, epsilon = 0.5) {
  # Genera ruido de Dirichlet para promover la exploracion de movimientos en la raiz
  noise = rgamma(length(policy.vector), shape = alpha, rate = 1)
  noise = noise / sum(noise) # Normalizar el ruido
  # Combina la politica original (P) con el ruido (P_noisy = (1-epsilon)*P + epsilon*Noise)
  noisy.policy = (1 - epsilon) * policy.vector + epsilon * noise
  return(noisy.policy / sum(noisy.policy)) # Normalizar la politica ruidosa final
}

# Funcion para obtener el valor final de la partida
get.game.result.value = function(game) {
  if (is_game_over(game)) { # Si finalizo la partida
    result = result(game) # Obtener resultado (e.g., "1-0", "0-1", "1/2-1/2")
    if (result == "1-0") return(1) # Blanco gana (Valor +1)
    if (result == "0-1") return(-1) # Negro gana (Valor -1)
    return(0) # Empate (Valor 0)
  }
  return(NA) # Si el juego no termina devuelve NA
}

# Funcion para obtener o inicializar un nodo en el arbol MCTS
get.node.stats = function(fen.key, legal.moves, P.model = NULL) {
  if (exists(fen.key, envir = mcts.tree)) {
    return(get(fen.key, envir = mcts.tree)) # Si existe, retorna las estadisticas
  }
  
  # Inicializacion de un nuevo nodo
  N = setNames(rep(0, length(legal.moves)), legal.moves) # N Contador de Visitas por accion
  W = setNames(rep(0, length(legal.moves)), legal.moves) # W Suma de Valores/Ganancias por accion
  
  if (is.null(P.model)) {
    P = N # P Probabilidad Priorizada (se inicializa con ceros si no hay prediccion)
  } else {
    P = setNames(P.model, legal.moves) # P Asignar la probabilidad priorizada P de la NN
  }
  
  node.stats = list(N = N, W = W, P = P) # Estructura del nodo
  assign(fen.key, node.stats, envir = mcts.tree) # Guardar en el entorno mcts.tree
  return(node.stats)
}

# Funcion para ejecutar la busqueda Monte Carlo Tree Search (MCTS)
run.mcts = function(game, model, num.simulations = NUM.SIMULATIONS, apply.noise = FALSE) {
  
  root.fen = fen(game)
  input.tensor = fen.to.vector(root.fen)
  
  # Prediccion de la Red Neuronal (NN) en la raiz
  prediction = model(input.tensor)
  
  V.model.root = as.numeric(prediction$value)
  P.model.raw = c(as.numeric(prediction$policy))
  
  legal.moves = moves(game)
  # Recorte de la politica NN a la longitud de movimientos legales (Recorte incorrecto/Temporal)
  P.model = P.model.raw[1:length(legal.moves)]
  
  # Parche de Asignacion Segura en la Raiz: Normalizacion o Uniforme
  suma.p = sum(P.model, na.rm = TRUE)
  
  if (suma.p > safety.threshold) {
    P.model = P.model / suma.p # Normalizacion si tiene peso util
  } else {
    # Si la suma es insignificante, forzamos la Politica UNIFORME para la exploracion inicial.
    # cat("ADVERTENCIA: P.model recortada tiene suma insignificante (", suma.p, "). Asignando Politica UNIFORME en raiz.\n")
    P.model = rep(1 / length(legal.moves), length(legal.moves))
  }
  
  # P.model es la politica NN LIMPIA y PARCHEADA
  
  root.stats = get.node.stats(root.fen, legal.moves, P.model) # Inicializar el nodo raiz
  
  if (apply.noise) {
    root.stats$P = apply.dirichlet.noise(root.stats$P) # Aplicar ruido de Dirichlet (Solo en Self-Play en la raiz)
  }
  
  # Bucle de Simulaciones MCTS
  for (i in 1:num.simulations) {
    current.game = game # Copia del estado del juego
    path = list() # Inicializar trayectoria de nodos visitados
    
    while (!is_game_over(current.game)) {
      current.fen = fen(current.game)
      current.stats = get.node.stats(current.fen, moves(current.game))
      
      if (sum(current.stats$N) == 0) { # Expansion: Si el nodo no ha sido visitado
        # Evaluar el nuevo nodo con la NN
        input.tensor = fen.to.vector(current.fen)
        prediction = model(input.tensor)
        V.model = as.numeric(prediction$value) # Valor V para retropropagacion
        
        legal.moves = moves(current.game)
        P.model.raw = c(as.numeric(prediction$policy))
        P.model = P.model.raw[1:length(legal.moves)]
        
        # Parche de Asignacion Segura en Expansion
        suma.p.exp = sum(P.model, na.rm = TRUE)
        if (suma.p.exp > safety.threshold) {
          P.model = P.model / suma.p.exp
        } else {
          P.model = rep(1 / length(legal.moves), length(legal.moves))
        }
        
        current.stats$P = setNames(P.model, legal.moves) # Guardar P_model en el nodo
        break # Terminar la fase de Seleccion/Expansion
      }
      
      # Seleccion: Usar la formula PUCT
      N.sum = sum(current.stats$N)
      Q = ifelse(current.stats$N > 0, current.stats$W / current.stats$N, 0)
      ucb.values = calculate.ucb(Q, current.stats$P, N.sum, current.stats$N)
      
      selected.move = names(which.max(ucb.values)) # Seleccionar la accion con PUCT maximo
      
      path[[length(path) + 1]] = list(fen = current.fen, move = selected.move) # Registrar el nodo y el movimiento
      
      current.game = push_move(current.game, selected.move) # Ejecutar el movimiento
    }
    
    # Determinacion del valor final (Z)
    if (is_game_over(current.game)) {
      Z = get.game.result.value(current.game) # Valor del final de partida (1, 0, -1)
      if (is_stalemate(current.game) || 
          is_seventyfive_moves(current.game) || 
          is_fivefold_repetition(current.game)) {
        Z = -0.5
      }
    } else {
      Z = V.model # Valor V de la ultima expansion
    }
    
    # Retropropagacion (Backpropagation)
    for (node.data in rev(path)) {
      stats = get(node.data$fen, envir = mcts.tree)
      
      Z = -Z # Invertir el valor para el jugador anterior
      
      stats$N[node.data$move] = stats$N[node.data$move] + 1 # Actualizar visitas
      stats$W[node.data$move] = stats$W[node.data$move] + Z # Actualizar suma de valores
      
      assign(node.data$fen, stats, envir = mcts.tree) # Guardar estadisticas actualizadas
    }
  } # Final del bucle de simulaciones MCTS
  
  # Calculo de la Politica Final y Mejor Movimiento
  final.visits = root.stats$N
  total.visits = sum(final.visits)
  
  # Parche Final de Robustez: Evitar NaN si no hubo visitas (total.visits == 0)
  if (total.visits == 0) {
    # Si no hubo visitas, la politica es uniforme y el valor es 0.
    P.MCTS.final = setNames(rep(1 / length(final.visits), length(final.visits)), names(final.visits))
    best.action.name = names(which.max(P.MCTS.final))
    Q.best.move = 0
    # cat("ADVERTENCIA: Total de visitas MCTS CERO. P.MCTS es UNIFORME para la seleccion.\n")
  } else {
    # Calculo normal de Pi (politica objetivo)
    P.MCTS.final = final.visits / total.visits
    best.action.name = names(which.max(final.visits))
    Q.best.move = root.stats$W[best.action.name] / root.stats$N[best.action.name]
  }
  
  # Devolver resultados
  return(list(
    best.move = best.action.name,
    policy.vector = P.MCTS.final,
    value = Q.best.move,
    P.model.clean = P.model # <--- AÑADIDO: Política NN limpia como fallback
  ))
}

# Funcion para convertir FEN a tensor (Input para la Red Neuronal)
fen.to.vector = function(fen, num.repetitions = 0) {
  matrixx = array(0, dim = c(8, 8, 18)) # Tensor de entrada 8x8x18
  board.data = strsplit(fen, " ")[[1]] # Datos de FEN (tablero, turno, enroque, etc.)
  board = board.data[1] # Solo el tablero
  rows = strsplit(board, "/")[[1]] # Filas
  
  # Mapeo de piezas a los primeros 12 canales (Blancas 1-6, Negras 7-12)
  map.piece = c("P"=1, "N"=2, "B"=3, "R"=4, "Q"=5, "K"=6, "p"=7, "n"=8, "b"=9, "r"=10, "q"=11, "k"=12) 
  
  for (y in 1:8) { # Recorrido de filas (y)
    x = 1 # Columna inicial (x)
    for (char in strsplit(rows[9 - y], "")[[1]]) { # Recorrido de caracteres (la fila 8 de FEN es la fila 1 del tensor)
      if (grepl("[1-8]", char)) {
        x = x + as.integer(char) # Saltar casillas vacias
      } else {
        piece = map.piece[char] # Obtener el numero de la pieza
        matrixx[y, x, piece] = 1 # Poner 1 en el canal correspondiente
        x = x + 1
      }
    }
  }
  
  # Canales de caracteristicas adicionales (Metadata)
  if (board.data[2] == "w") { # Canal 13 Turno (1 si es Blanco, 0 si es Negro)
    matrixx[,, 13] = 1 
  }
  
  if (board.data[3] != "-") { # Canal 14 Opcion de enroque
    matrixx[,, 14] = 1 
  }
  
  if (num.repetitions >= 2) { # Canal 15 Repeticion de posicion (para la regla de 50 movimientos y 3 repeticiones)
    matrixx[,, 15] = 1 
  }
  matrixx[,, 16] = 1 # Canal 16 Capa constante (siempre 1)
  
  if (length(board.data) >= 5) { # Canal 17 Reloj de medio movimiento (Half-move clock, para la regla de 50)
    half.move.clock = as.integer(board.data[5])
    normalized.clock = min(1, half.move.clock / 100.0) # Normalizado entre 0 y 1
    matrixx[,, 17] = normalized.clock
  }
  
  if (length(board.data) == 6) { # Canal 18 Numero de movimiento completo (Full-move number)
    full.move.number = as.integer(board.data[6])
    normalized.move = min(1, full.move.number / 200.0) # Normalizado entre 0 y 1
    matrixx[,, 18] = normalized.move
  }
  
  array_reshape(matrixx, c(1, 8, 8, 18)) # Reajusta para que sea un tensor de lote (Batch) para la prediccion del modelo
}

# Funcion para elegir el mejor movimiento (ACTUALIZADA con controles de robustez y fallback a NN)
best.move = function(game, model, move.count = 0) {
  moves.list = moves(game) # La lista de movimientos legales originales
  if (length(moves.list) == 0) return(NULL) # Si no hay movimientos, retornar NULL (partida terminada)
  
  # Logica de exploracion/explotacion
  noise.enabled = (move.count < 30) # Ruido para auto-entrenamiento
  high.temp = (move.count < 40) # Temperatura alta para exploracion inicial
  
  mcts.result = run.mcts(game, model, apply.noise = noise.enabled) 
  P.MCTS = mcts.result$policy.vector # Politica Pi
  P.model.clean = mcts.result$P.model.clean # Política NN Limpia (Fallback)
  
  # Si la politica Pi es invalida (NA o suma cero), usa el FALLBACK A NN para la selección
  if (is.na(sum(P.MCTS)) || sum(P.MCTS) == 0) {
    # cat("P.MCTS es invalida (NA/Suma Cero) en la raiz. Usando FALLBACK a NN.\n")
    # Usar P.model.clean para seleccionar el movimiento con mayor probabilidad
    return(names(which.max(P.model.clean))) 
  }
  
  if (high.temp) {
    # MODO EXPLORACION (Muestreo con temperatura T=1.0)
    temperature = 1.0 
    P.exp = P.MCTS ^ (1/temperature) 
    sum.P.exp = sum(P.exp)
    
    if (is.na(sum.P.exp) || sum.P.exp == 0) {
      # FALLBACK CORREGIDO: Si el cálculo P.temp falla, usa la política NN para muestrear
      # cat("Fallo al calcular P.temp (NA/Suma Cero). Usando FALLBACK a la NN para muestreo.\n")
      # Usa la política NN limpia como probabilidades para el muestreo estocástico
      best.move = sample(names(P.model.clean), size = 1, prob = P.model.clean) 
    } else {
      P.temp = P.exp / sum.P.exp # Normalizar
      best.move = sample(names(P.temp), size = 1, prob = P.temp) # Muestreo estocastico
    }
    
  } else {
    
    # MODO EXPLOTACION
    max_index = which.max(P.MCTS) # Encontrar el indice del movimiento con mas probabilidad (visitas)
    
    if (length(max_index) == 0 || is.na(max_index)) {
      # FALLBACK: Si which.max falla, usar la NN
      # cat("Fallo al encontrar el movimiento maximo (which.max). Usando FALLBACK a la NN.\n")
      best.move = names(which.max(P.model.clean))
    } else {
      best.move = names(max_index) # Convertir indice a nombre del movimiento
    }
  }
  
  return(best.move)
}

# Funcion para obtener el elo
elo.update.pair = function(rating.A, rating.B, score.A, k = 20) {
  # Actualiza el rating ELO del jugador A contra un oponente B (rating fijo)
  # Score A 1 (gana), 0 (pierde), 0.5 (empate)
  
  # Calcula la puntuacion esperada (E_A) para el jugador A
  expected.A = 1 / (1 + 10 ^ ((rating.B - rating.A) / 400))
  
  # Calcula el nuevo rating para el jugador A
  new.A = rating.A + k * (score.A - expected.A)
  
  # Solo devolvemos el nuevo ELO de A
  return(new.A)
}

elo.function = function(df, k = 20) {
  
  # Funcion para calcular el ELO del bot basado en sus resultados
  
  # Comprobacion de datos vacios o faltantes
  if (nrow(df) == 0 || !"Resultado" %in% names(df)) {
    cat("Data frame vacio o falta la columna 'Resultado'. Devolviendo ELO inicial (400).\n")
    return(400)
  }
  # Fijo en 400, sin importar el nombre del oponente (Oponente base o ELO fijo)
  rB = 400
  
  # Vector para almacenar el ELO calculado en cada paso
  calculated.elos = numeric(nrow(df))
  
  # Recorremos partidas
  for (i in seq_len(nrow(df))) {
    
    # ELO anterior (rA)
    if (i == 1) {
      rA = 400 # ELO de partida es 400 (base)
    } else {
      rA = calculated.elos[i - 1] # ELO de la partida anterior
    }
    
    res = df$Resultado[i] # Resultado ( "1-0")
    
    # Si el resultado no es valido o esta incompleto, mantenemos el ELO anterior
    if (is.na(res) || res == "" || !res %in% c("1-0", "0-1", "1/2-1/2")) {
      calculated.elos[i] = rA
      next
    }
    
    score.A = if (res == "1-0") 1 else if (res == "0-1") 0 else 0.5 # Convertir resultado a puntaje
    
    new.elo = elo.update.pair(rA, rB, score.A, k = k) # Actualizar ELO
    calculated.elos[i] = new.elo
    
    # Si la columna 'Elo' existe en el df, actualizamos la columna tambien
    if ("Elo" %in% names(df)) {
      df$Elo[i] = new.elo
    }
  }
  
  return(calculated.elos[nrow(df)])
}

# Funcion para auto entrenamiento
bot.vs.bot.game = function(model ,games.data, games.heavy.data){
  history.positions = character() # Historial de posiciones FEN
  max.jugadas = 240 # Limite de jugadas para evitar bucles infinitos
  game = game() # Partida nueva (estado inicial)
  num.moves = 0 # Contador de movimientos
  moves = character() # Lista de movimientos jugados
  move.times = numeric() # Tiempos de calculo por movimiento
  MCTS.policies = list() # (Variable no usada en esta version, eliminar si es redundante)
  
  while (!is_game_over(game) && num.moves < max.jugadas) { # Bucle principal de la partida
    
    current.fen = fen(game) # FEN actual
    history.positions = c(history.positions, current.fen) # Anadir a historial para la regla de repeticion
    num.moves = num.moves + 1 # Aumentar contador
    
    start.time = Sys.time() # Registrar tiempo inicial de MCTS
    
    move = best.move(game, model, move.count = num.moves) # Llamada a best.move (incluye MCTS, ruido y temp)
    
    end.time = Sys.time() # Registrar tiempo final de MCTS
    move.times[num.moves] = as.numeric(difftime(end.time, start.time, units = "secs")) # Tiempo de movimiento
    
    if (is.null(move)) { cat("Salida por movimientos nulos"); break } # Control de nulo (Partida terminada por Ahogado/Jaque Mate no detectado antes de llamar a best.move)
    cat("\nMovimiento: ", move, " numero ", num.moves, "\n") # Imprimir movimiento
    
    game = move(game, move) # Realizar movimiento
    moves[num.moves] = move # Guardar movimiento
    print.chess.board(game) # Deshabilitado para partidas rapidas
    
  }
  
  final.result.value = get.game.result.value(game) # Obtener el resultado final (1, 0, -1)
  
  moves = moves[1:num.moves] # Recortar movimientos a la longitud real de la partida
  move.times.median = median(move.times) # Mediana de tiempos de calculo
  
  # Determinar el color del jugador que NO movio ultimo (es decir, el color al que le tocaria mover si el juego continuara)
  last.player.color = if (turn(game) == "white") "black" else "white"
  # Guardar datos pesados (heavy data) para el entrenamiento de la NN (incluye FENs y resultados)
  games.heavy.data = df.heavy.game.data(games.heavy.data, game, "bot_vs_bot_interno", moves, num.moves, move.times.median, last.player.color, history.positions = history.positions, final.result = final.result.value) 
  # Guardar datos ligeros (light data) para el monitoreo general
  games.data = df.game.data(games.data, game, "bot_vs_bot_interno", num.moves, "Hatchet1", "Hatchet1") 
  
  print(games.data)
  # print(games.heavy.data) 
  
  return(list(
    games.data = games.data, # Retornar datos ligeros
    games.heavy.data = games.heavy.data # Retornar datos pesados
  ))
}

# Funcion para jugar con la ia (Bot vs Humano)
bot.vs.player.game = function(model, games.data, games.heavy.data){
  max.jugadas = 200
  game = game()
  mov = 0
  moves = character()
  
  # Eleccion de color por el jugador humano
  repeat {
    color = tolower(readline("Desea jugar como blancas o negras "))
    if (color %in% c("blancas", "negras")) break
    cat("Escriba 'blancas' o 'negras'\n")
  }
  
  jugador.blancas = (color == "blancas") # TRUE si el jugador humano es blanco
  
  while (!is_game_over(game) && mov < max.jugadas) {
    print.chess.board(game) # Mostrar el tablero
    
    turno.blancas = (turn(game) == "white") # TRUE si es turno de las blancas
    
    if (turno.blancas == jugador.blancas) { # Turno del jugador humano
      moves.list = moves(game)
      cat("Movimientos posibles ", paste(moves.list, collapse = ", "), "\n")
      
      repeat {
        move = readline("Su movimiento (e2e4) ")
        if (move %in% moves.list) break # Validar movimiento
        cat("Solo movimientos legales\n")
      }
      
      game = move(game, move)
    } else { # Turno de la IA
      cat("\nTurno de la IA\n")
      
      move = best.move(game, model) # La IA elige el mejor movimiento (MCTS en modo Explotacion)
      
      if (is.null(move)) {
        cat("IA sin movimiento valido, fin de partida\n")
        break
      }
      
      cat("IA juega ", move, "\n")
      game = move(game, move)
    }
    
    mov = mov + 1
    moves[mov] = move
  }
  
  # Recortar moves y move.times al tamano real
  moves = moves[1:mov]
  move.times = 0 # Contador simple (no se usa tiempo de MCTS aqui)
  
  # Guardar resultados de la partida Bot vs Humano
  games.heavy.data = df.heavy.game.data(games.heavy.data, game, "bot_vs_bot_interno", moves, mov, move.times, "white", history.positions = history.positions, final.result = get.game.result.value(game))
  games.data = df.game.data(games.data, game, "bot_vs_bot_interno", mov, "Hatchet1", "Hatchet1")
  
  # Control de datos, eliminar en entrega
  print(games.data)
  
  return(list(
    games.data = games.data,
    games.heavy.data = games.heavy.data
  ))
}

# Funcion para jugar con ia externa
bot.vs.external.game = function(model, games.data, games.heavy.data){
  max.jugadas = 200 # Limite de movimientos
  game = game() # Partida nueva
  mov = 0 # Contador de movimientos
  moves = character() # Lista de movimientos
  history.positions = character() # Historial de FENs
  
  
  if (!exists("iniciar.stockfish") || !exists("obtener.movimiento.stockfish")) {
    stop("Las funciones de Stockfish no estan disponibles. Asegurese de haber cargado 'stock.fish.R'.")
  }
  
  engine = NULL # Inicializar la variable engine
  tryCatch({
    engine = iniciar.stockfish("stockfish.exe")
  }, error = function(e) {
    stop(paste("Error al iniciar Stockfish", e$message))
  })
  
  
  ia.interna.color = sample(c("white", "black"), size = 1) # Selecciona el color de la IA interna (Hatchet1) al azar
  cat("\nIA INTERNA JUEGA COMO", toupper(ia.interna.color), "--- \n")
  
  
  while (!is_game_over(game) && mov < max.jugadas) {
    print.chess.board(game)
    current.fen = fen(game)
    history.positions = c(history.positions, current.fen)
    
    turno.bool = turn(game)
    # Si turno.bool es FALSE (blancas) -> "white"
    # Si turno.bool es TRUE (negras) -> "black"
    turno.actual = ifelse(turno.bool, "black", "white") # Determina el color del jugador actual
    
    mov = mov + 1
    
    start.time = Sys.time()
    
    if (turno.actual == ia.interna.color) {
      
      cat("\nTurno de la IA Interna (", ia.interna.color, ")\n")
      # Asumo que best.move devuelve una jugada simple, ya que no estamos en modo Self-Play
      move = best.move(game, model) # La IA interna elige el movimiento
      ia.name = "Hatchet1"
      
    } else {
      
      cat("\nTurno de la IA Externa (Stockfish - ", turno.actual, ")\n")
      
      moves.list = moves(game)
      move = NULL # Inicializar move
      
      tryCatch({
        
        # Pedir mejor jugada a Stockfish
        jugada_stockfish = obtener.movimiento.stockfish(
          engine,
          current.fen,
          profundidad_stockfish = 10,
          tiempo_stockfish_ms = 200
        )
        
        # Asignar el resultado
        move = jugada_stockfish
        
        # Si no devuelve jugada, o no esta en la lista, usar primera legal
        if (is.na(move) || !(move %in% moves.list)) { # Si Stockfish falla o devuelve ilegal, usa el primer movimiento valido
          move = moves.list[1]
        }
        
      }, error = function(e) {
        cat("Error al obtener movimiento de Stockfish ", e$message, " - El juego termina.\n")
        move = NULL # Asegurar que move sea NULL si hay error
      })
      
      ia.name = "Stockfish"
    }
    
    end.time = Sys.time()
    move.time = as.numeric(difftime(end.time, start.time, units = "secs"))
    
    if (is.null(move) || !move %in% moves(game)) { # Control de movimiento nulo o ilegal antes de aplicar
      cat("Fin de partida IA sin movimiento valido o legal.\n")
      break
    }
    
    cat(ia.name, " juega ", move, " (Tiempo ", round(move.time, 2), "s)\n")
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
  final.result.value = get.game.result.value(game)
  
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
bot.vs.bot = function(model, games.data, games.heavy.data){
  
  option = readline("Ingrese el numero de autoentrenamientos que desea \n")
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
bot.vs.player = function(model, games.data, games.heavy.data){
  
  option = readline("Ingrese el numero de partidas que desea jugar \n")
  repeat {
    if (!is.na(as.numeric(option))){
      if(as.integer(option) == option && option > 0) break
    }
    option = readline("Por favor digite un numero entero positivo\n")
  }
  
  # Partidas iteradas
  while(option > 0){
    result = bot.vs.player.game(model, games.data, games.heavy.data)
    games.data = result$games.data
    games.heavy.data = result$games.heavy.data
    option = option - 1
  }
  
  # Edicion no referenciada
  games.data <<- games.data
  games.heavy.data <<- games.heavy.data
}

#$
bot.vs.external = function(model, games.data, games.heavy.data){
  
  option = readline("Ingrese el numero de partidas bot.vs.external que desea jugar \n")
  repeat {
    if (!is.na(as.numeric(option))){
      if(as.integer(option) == option && option > 0) break
    }
    option = readline("Por favor digite un numero entero positivo\n")
  }
  option = as.numeric(option)
  
  # Partidas iteradas
  while(option > 0){
    result = bot.vs.external.game(model, games.data, games.heavy.data)
    games.data = result$games.data
    games.heavy.data = result$games.heavy.data
    option = option - 1
  }
  
  # Edicion no referenciada (actualiza las variables globales/entorno superior)
  games.data <<- games.data
  games.heavy.data <<- games.heavy.data
  
  cat("Finalizado el conjunto de partidas bot.vs.external.\n")
}