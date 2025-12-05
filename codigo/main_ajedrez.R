# main_ajedrez.R
# Hecho por:
# Anthonny Flores (C32875)
# Andres Zuñiga Mora (C38733)
# Amy Chen Wu (C32203)
# Leonardo Vega Aragon (C38313)

# Convenciones temporales
# #*: significa es mejorable (optimizable)
# #$: significa que hay que trabajar en ello
# #%: significa que hay que darle prioridad
# #X: significa que hay que eliminar el comentario
# No hay control de errores basicos (casos fortituos)

# Para la documentacion, reglas:
# <- Un espacio antes de comentar
# Sin acentos
# Palabras significativas y cortas
# Variables primitivas se comentan de la forma:
# int variabale = 5 # Suma de iteraciones
# Variables no primiticas
# # Carga de modelo
# model = generate.model() 
# Si es nombre de funcion empieza con minuscula51


# Sources
source("red_neuronal_ajedrez.R")
source("logica_ajedrez.R") 
source("datos_ajedrez.R") 

# main()
main <- function(){
  
  cat("\014") # Limpiar la consola 
  leave = F # Salida main()
  
  # Carga de modelo
  
  model = call.model()
  
  # Carga de toma de datos
  games.data <<- data.frame() # Informacion general de las partidas
  games.heavy.data <<- data.frame() # Informacion complementaria de las partidas
  
  cat("\nBienvenido al proyecto Hatchet1, bot de ajedrez")
  cat("\nHecho por:")
  cat("\nAnthonny Flores (C32975)")
  cat("\nLeonardo Vega Aragon (C38313)")
  cat("\nAndres Zuñiga Mora (C38733)")
  cat("\nAmy Chen Wu (C32203)")
  cat("\nPara mas informacion ver la documentacion\n")
  
  # Consola para la manipulacion de modelo
  while(!leave){
    
    # Captar opcion
    cat("\nEliga un modo, digite e imprima \"1\" para probar bot vs bot (Interna), \n\"2\" para bot vs Usted, \"3\" para bot vs bot (Externa) (Inavilitado) y \"4\" para salir: \n")
    mode = readline()
    repeat {
      if (!is.na(as.numeric(mode))){
        if(as.integer(mode) == mode && 0 < mode && mode < 5) break
      }
      mode = readline("Por favor digite e imprima solo una de las opciones dadas: ")
    }
    
    # Modos de prueba
    if (mode == "1") {
      bot.vs.bot(model, games.data, games.heavy.data)
    } else if (mode == "2") {
      bot.vs.player(model, games.data, games.heavy.data)
    } else if (mode == "3") {
      bot.vs.external(model, games.data, games.heavy.data)
    } else if (mode == "4") {
      leave = TRUE
      cat("\nGracias por usar el Hatchet1\n")
    }
    
    # Visualizar data
    cat("Si desea imprimir la data en consola digite \n\"1\" (Inavilitado) si no es asi, digite \"0\" : \n")
    option = readline()
    repeat {
      if (!is.na(as.numeric(option))){
        if(as.integer(option) == option && (0 == option || 1 == option)) break
      }
      option = readline("Por favor digite e imprima solo una de las opciones dadas: ")
    }
    if(option == 1){
      data.manege(games.data, games.heavy.data)
    }
    
    # Guardar data
    data.save(games.data, games.heavy.data)
  }
  
  cat("\nHasta luego\n")
  
  save.model(model)
}

main()
