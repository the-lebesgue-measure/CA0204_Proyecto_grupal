# datos_ajedrez.R
# Hecho por:
# Anthonny Flores (C32875)
# Andres Zuñiga Mora (C38733)
# Amy Chen Wu (C32203)
# Leonardo Vega Aragon (C38313)

# Librerias
library(chess)

#$
# elo.function <- function(){}

# Funcion para guardar datos principales de las partidas
df.game.data <- function(df, game, mode, num.moves, model.name = "Hatchet1", other.model.name = "Hatchet1") {
  
  # Data principal
  df.copy = data.frame(
    Modo = mode,
    Modelo = model.name,
    Modelo_contrario = other.model.name,
    Finalizado = is_game_over(game),
    Jaque = is_check(game),
    Mate = is_checkmate(game),
    Ahogado = is_stalemate(game),
    Material = is_insufficient_material(game),
    Regla75 = is_seventyfive_moves(game),
    Repeticion5 = is_fivefold_repetition(game),
    Numero_de_movimientos = num.moves
  )
  
  if (is.null(df)) {
    return(df.copy)
  } else {
    df.final.copy = rbind(df, df.copy)
    return(df.final.copy)
  }
}

# Funcion para guardar datos secundarios de las partidas
df.heavy.game.data <- function (df, game, mode, moves, num.moves, move.times = 0, color, model.name = "Hatchet1", other.model.name = "Hatchet1", history.positions = ""){
  
  # Data principal
  df.copy = data.frame(
    Modo = mode,
    Modelo = model.name,
    Modelo_contrario = other.model.name,
    Color = color,
    # Resultado = outcome(game),
    Finalizado = is_game_over(game),
    # Elo = elo.function(moves, color,.Resultado..), # Esto hay que hacerlo para la proxima entrega
    Jaque = is_check(game),
    Mate = is_checkmate(game),
    Numero_de_movimientos = num.moves,
    Movimientos = paste(moves, collapse = "X"),
    Tiempo_movimiento_medio = move.times,
    Tabla_final = fen(game),
    Tabla_totales = paste(history.positions, collapse = "X")
  )
  
  if (is.null(df)) {
    return(df.copy)
    
  } else {
    df.final.copy = rbind(df, df.copy)
    return(df.final.copy)
  }
}

# Funcion para pasar de df a txt
data.to.txt <- function(df, file.name){ 
  exist = file.exists(file.name)
  write.table(df, file = file.name, sep = ",", row.names = FALSE, col.names = !exist, append = exist, quote = FALSE)
}

# Funcion para mostrar data raw
data.manege <- function(games.data, games.heavy.data){
  
  leave = F
  while(!leave){
    cat("Si desea imprimir la data principal digite e imprima \"1\", \npara data secundaria \"2\", y par salir \"3\" : ")
    option = readline()
    repeat {
      if (is.numeric(option)){
        if(as.numeric(option) == option && 0 < option && option < 5 ) break
      }
      option = as.numeric(readline("Por favor digite e imprima solo una de las opciones dadas"))
    }
    
    if (option == 1) {
      print(games.data)
    } else if (option == 2) {
      print(games.heavy.data)
    } else if (option == 3) {
      leave = TRUE
    } 
  }
}

# Funcion para guardar data
data.save <- function(games.data, games.heavy.data){
  data.to.txt(games.data, "../datos/data_juego.csv"); data.to.txt(games.heavy.data, "../datos/data_fuerte.csv")
}

