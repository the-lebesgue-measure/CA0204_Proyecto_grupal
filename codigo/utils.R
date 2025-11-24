library(processx)

iniciar_stockfish <- function(ruta) {
  engine <- process$new(ruta, stdin = "|", stdout = "|", stderr = "|")
  engine$write_input("uci\n")
  Sys.sleep(0.3)
  engine$write_input("isready\n")
  Sys.sleep(0.3)
  engine$write_input("ucinewgame\n")
  Sys.sleep(0.2)
  return(engine)
}

movimientos_legales <- function(engine, fen) {
  engine$write_input(paste0("position fen ", fen, " moves\n"))
  engine$write_input("go perft 1\n")
  Sys.sleep(0.5)
  
  salida <- engine$read_output_lines()
  jugadas <- sub(":.*", "", salida)
  jugadas <- jugadas[grep("^[a-h][1-8][a-h][1-8]", jugadas)]
  
  return(jugadas)
}

aplicar_movimiento <- function(engine, fen, mov) {
  engine$write_input(paste0("position fen ", fen, " moves ", mov, "\n"))
  engine$write_input("d\n")
  Sys.sleep(0.4)
  
  salida <- paste(engine$read_output_lines(), collapse = "\n")
  linea <- regmatches(salida, regexpr("Fen: .*", salida))
  linea <- gsub("Fen: ", "", linea)
  
  # cortar todo después de la parte del FEN
  linea <- sub("\n.*", "", linea)
  
  return(trimws(linea))
}


obtener_movimiento_stockfish <- function(engine, fen, profundidad, tiempo_ms) {
  engine$write_input(paste0("position fen ", fen, " moves\n"))
  engine$write_input(
    paste0("go depth ", profundidad, " movetime ", tiempo_ms, "\n")
  )
  Sys.sleep(0.6)
  
  salida <- engine$read_output_lines()
  linea <- salida[grep("^bestmove", salida)]
  if (length(linea) == 0) return(NA)
  
  jugada <- sub("bestmove ", "", linea)
  sub(" .*", "", jugada)
}
