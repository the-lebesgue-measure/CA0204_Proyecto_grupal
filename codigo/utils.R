# utils.R
# Hecho por:
# Anthonny Flores (C32875)
# Andres Zuñiga Mora (C38733)
# Amy Chen Wu (C32203)
# Leonardo Vega Aragon (C38313)

library(processx)


iniciar.stockfish <- function(ruta) {
  # Se mantiene el mensaje inicial para indicar el estado del motor
  cat("--- Iniciando Stockfish ---\n")
  engine = process$new(ruta, stdin = "|", stdout = "|", stderr = "|")
  
  engine$write_input("uci\n")
  Sys.sleep(0.5) # Tiempo unificado para UCI/ISREADY
  
  engine$write_input("isready\n")
  Sys.sleep(0.5)
  
  engine$write_input("ucinewgame\n")
  Sys.sleep(0.2)
  
  cat("Stockfish iniciado.\n")
  return(engine)
}

movimientos.legales <- function(engine, fen) {
  
  engine$write_input(paste0("position fen ", fen, " moves\n"))
  
  engine$write_input("go perft 1\n")
  Sys.sleep(0.5)
  
  salida = engine$read_output_lines()
  
  jugadas = sub(":.*", "", salida)
  jugadas = jugadas[grep("^[a-h][1-8][a-h][1-8]", jugadas)]
  
  return(jugadas)
}


aplicar.movimiento <- function(engine, fen, mov) {
  
  cmd_pos = paste0("position fen ", fen, " moves ", mov, "\n")
  engine$write_input(cmd_pos)
  
  # Usar el comando 'd' para obtener el nuevo FEN
  engine$write_input("d\n")
  Sys.sleep(0.5) # Aumentado de 0.4 a 0.5
  
  salida = paste(engine$read_output_lines(), collapse = "\n")
  
  linea = regmatches(salida, regexpr("Fen: .*", salida))
  linea = gsub("Fen: ", "", linea)
  
  linea = sub("\n.*", "", linea)
  
  nuevo_fen = trimws(linea)
  
  return(nuevo_fen)
}


obtener.movimiento.stockfish <- function(engine, fen, profundidad, tiempo_ms) {
  
  cmd_pos = paste0("position fen ", fen, " moves\n")
  engine$write_input(cmd_pos)
  
  cmd_go = paste0("go depth ", profundidad, " movetime ", tiempo_ms, "\n")
  engine$write_input(cmd_go)
  
  Sys.sleep(1.5) 
  
  salida = engine$read_output_lines()
  
  linea = tail(salida[grep("^bestmove", salida)], 1)
  
  if (length(linea) == 0) {
    return(NA)
  }
  
  jugada = sub("bestmove ", "", linea)
  jugada_final = sub(" .*", "", jugada)
  
  return(jugada_final)
}