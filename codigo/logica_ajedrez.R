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

# Funcione para convertir FEN a array 8x8x12 (tensor)
fen.to.vector <- function(fen) { 
  
  matrixx = array(0, dim = c(8, 8, 12))
  board = strsplit(fen, " ")[[1]][1]
  rows = strsplit(board, "/")[[1]]
  
  # Modelo estandar (numeracion de las piezas)
  map.piece = c("P"=1, "N"=2, "B"=3, "R"=4, "Q"=5, "K"=6, "p"=7, "n"=8, "b"=9, "r"=10, "q"=11, "k"=12)
  
  for (y in 1:8) {
    x = 1
    for (char in strsplit(rows[9 - y], "")[[1]]) {
      if (grepl("[1-8]", char)) {
        x = x + as.integer(char)
      } else {
        piece = map.piece[char]
        matrixx[y, x, piece] = 1
        x = x + 1
      }
    }
  }
  array_reshape(matrixx, c(1, 8, 8, 12))  # Batch dim para predict
}

#$ Anadir posible funciones aplicando montecarlo o demas metodos

# Funcion corregida para elegir el mejor movimiento
best.move <- function(game, model, history.positions = NULL) { 
  
  moves.list = moves(game)
  
  if (length(moves.list) == 0) return(NULL)  # Sin movimientos
  
  # Crear batch de todas las posiciones posibles 
  num.moves = length(moves.list)
  batch.boards = array(0, dim = c(num.moves, 8, 8, 12))
  
  # Crear partidas con vista a futuro
  for (i in 1:num.moves) {
    temp.game = move(game, moves.list[[i]])
    batch.boards[i,,, ] = fen.to.vector(fen(temp.game))[1,,, ]  # Extraer sin batch dim
  }
  
  # Predict en batch 
  predictions = predict(model, batch.boards, verbose = 0)
  scores = predictions[["value"]][, 1]  
  
  if (!is.null(history.positions)) {
    for (i in 1:num.moves) {
      temp.game = move(game, moves.list[[i]])
      fen.temp = fen(temp.game)
      count.reps = sum(history.positions == fen.temp)
      if (count.reps >= 3) {  
        scores[i] = scores[i] - 0.7
      }
    }
  }
  
  scores = scores + rnorm(num.moves, mean = 0, sd = 0.0001)
  best.move = moves.list[[which.max(scores)]]
  
  return(best.move)
}

# Funciona para auto entrenamiento
bot.vs.bot.game <- function(model ,games.data, games.heavy.data){
  history.positions = character()  
  max.jugadas = 240
  game = game()
  num.moves = 0
  moves = character()
  move.times <- numeric()
  
  while (!is_game_over(game) && num.moves < max.jugadas) {
    
    history.positions = c(history.positions, fen(game)) 
    num.moves = num.moves + 1
    
    # Registrar tiempo de inicio
    start.time <- Sys.time()
    
    move = best.move(game, model, history.positions)
    
    
    # Registrar tiempo de fin 
    end.time <- Sys.time()
    move.times[num.moves] <- as.numeric(difftime(end.time, start.time, units = "secs"))
    
    if (is.null(move)) { cat("Salida por movimientos nulos"); break }
      cat("\nMovimiento: ", move, " numero ", num.moves, "\n")
      
      game = move(game, move)
      moves[num.moves] = move 
      print.chess.board(game)
  }
  
  
  # Recortar moves y move.times al tamaño real
  moves = moves[1:num.moves]
  move.times = median(move.times)
  
  games.heavy.data = df.heavy.game.data(games.heavy.data, game, "bot_vs_bot_interno", moves, num.moves, move.times, "white", history.positions = history.positions)
  games.data = df.game.data(games.data, game, "bot_vs_bot_interno", num.moves, "Hatchet1", "Hatchet1")
  
  #X Control de datos, eliminar en entrega las dos funciones de abajo
  print(games.data)
  print(games.heavy.data)
  
  return(list(
    games.data = games.data,
    games.heavy.data = games.heavy.data
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
  moves = moves[1:num.moves]
  move.times = 0 # Implementar contador
  
  games.heavy.data = df.heavy.game.data(games.heavy.data, game, "bot_vs_bot_interno", moves, num.moves, moves.times, "white", history.positions = history.positions)
  games.data = df.game.data(games.data, game, "bot_vs_bot_interno", num.moves, "Hatchet1", "Hatchet1")
  
  # Control de datos, eliminar en entrega 
  print(games.data)
  
  return(list(
    games.data = games.data,
    games.heavy.data = games.heavy.data 
  ))
}

#$
# external.game <- function(){}

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
# bot.vs.external <- function(model, games.data, games.heavy.data){}
