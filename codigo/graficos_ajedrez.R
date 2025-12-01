# graficos_ajedrez.R
# Hecho por:
# Anthonny Flores (C32875)
# Andres Zuñiga Mora (C38733)
# Amy Chen Wu (C32203)
# Leonardo Vega Aragon (C38313)

# Librerias 
library(chess) 
library(ggplot2)

# Funcion para convertir FEN a matriz de piezas
fen.to.matrix <- function(fen) { 
  
  # Descomposicion de FEN
  board = strsplit(fen, " ")[[1]][1] 
  rows = strsplit(board, "/")[[1]] 
  matriz = matrix("", nrow = 8, ncol = 8) 
  
  # Por las filas
  for (y in 1:8) { 
    
    x = 1 
    for (char in strsplit(rows[9 - y], "")[[1]]) { 
      
      #  Si hay pieza
      if (grepl("[1-8]", char)) {
        x = x + as.integer(char) 
      } else {
        matriz[y, x] = char 
        x = x + 1 
      }
    }
  }
  return(matriz) 
}

# Funcion para mapear piezas a simbolos Unicode
map.to.unicode <- function(matriz) { 
  
  matriz.unicode = matriz
  matriz.unicode[matriz.unicode == "p"] = "♙" # Negras
  matriz.unicode[matriz.unicode == "n"] = "♘"
  matriz.unicode[matriz.unicode == "b"] = "♗"
  matriz.unicode[matriz.unicode == "r"] = "♖"
  matriz.unicode[matriz.unicode == "q"] = "♕"
  matriz.unicode[matriz.unicode == "k"] = "♔"
  matriz.unicode[matriz.unicode == "P"] = "♟" # Blancas
  matriz.unicode[matriz.unicode == "N"] = "♞"
  matriz.unicode[matriz.unicode == "B"] = "♝"
  matriz.unicode[matriz.unicode == "R"] = "♜"
  matriz.unicode[matriz.unicode == "Q"] = "♛"
  matriz.unicode[matriz.unicode == "K"] = "♚"
  matriz.unicode[matriz.unicode == ""] = ". "
  
  return(matriz.unicode)
}

# Funcion principal para imprimir el tablero
print.chess.board <- function(game) {
  
  matriz = fen.to.matrix(fen(game)) 
  matriz.unicode = map.to.unicode(matriz) 
  
  # Se imprime la matriz 
  cat("\n            Tablero\n") 
  cat("   a   b   c   d   e   f   g   h\n")
  for (i in 8:1) {
    cat(i, " ")
    for (j in 1:8) {
      cat(matriz.unicode[i, j], " ")
    }
    cat("\n")
  }
  cat("   a   b   c   d   e   f   g   h\n")
}
