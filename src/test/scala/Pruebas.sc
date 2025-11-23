import Datos._
import Itinerarios._

// Ejemplo curso pequeño
val itsCurso = itinerarios(vuelosCurso,aeropuertosCurso)
//2.1 Aeropuertos incomunicados
val its1 = itsCurso("MID", "SVCS")
val its2 = itsCurso("CLO", "SVCS")

// 4 itinerarios CLO-SVO

val its3 = itsCurso("CLO","SVO")

//2 itinerarios CLO-MEX

val its4 = itsCurso("CLO", "MEX")

//2 itinerarios CTG-PTY
val its5 = itsCurso("CTG","PTY")

val itsTiempoCurso = itinerariosTiempo(vuelosCurso,aeropuertosCurso)

// prueba itinerariosTiempo
val itst1 = itsTiempoCurso("MID", "SVCS")
val itst2 = itsTiempoCurso("CLO", "SVCS")

// 4 itinerarios CLO-SVO

val itst3 = itsTiempoCurso("CLO","SVO")

//2 itinerarios CLO-MEX

val itst4 = itsTiempoCurso("CLO", "MEX")

//2 itinerarios CTG-PTY
val itst5 = itsTiempoCurso("CTG","PTY")


itinerarios(vuelosA1,aeropuertos)("HOU","BNA")
itinerariosTiempo(vuelosA1,aeropuertos)("HOU","BNA")