source("configuracion.R")
source("Utils.R")

# Función que conecta Stockfish con el sistema de la red neuronal
move.stockfish <- function(fen, moves.list) {
  # Iniciar motor
  engine <- iniciar_stockfish(ruta_stockfish)
  
  # Pedir mejor jugada a Stockfish
  jugada <- obtener_movimiento_stockfish(
    engine,
    fen,
    profundidad_stockfish,
    tiempo_stockfish_ms
  )
  
  engine$kill()
  
  # Si no devuelve jugada, o no está en la lista → usar primera legal
  if (is.na(jugada) || !(jugada %in% moves.list)) {
    jugada <- moves.list[1]
  }
  
  return(jugada)
}
