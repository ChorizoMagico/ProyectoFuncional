import Datos._
import Itinerarios._
import ItinerariosPar._
import org.scalameter._

// ========= Herramienta de medición de tiempo de ejecución =========
def tiempoDe[T](body: => T) = {
  val timeA1 =
    config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 60,
      Key.verbose -> false
    ) withWarmer(new Warmer.Default) measure {
      body
    }
  timeA1
}

def encabezado(txt: String): Unit = {
  println("\n" + "=" * 80)
  println(txt)
  println("=" * 80)
}

def aceleracion(tSec: Quantity[Double], tPar: Quantity[Double]): Double =
  tSec.value / tPar.value

// --------------------------------------------------------------------
// 1. itinerarios vs itinerariosPar
// --------------------------------------------------------------------

val conjuntoVuelos200 = vuelosC1 ++ vuelosC2


// --- Dataset pequeño: 15 vuelos (A1) ABQ -> SEA ---
encabezado("itinerarios vs itinerariosPar - 15 vuelos (A1) ABQ -> SEA")

val funcionSec15 = itinerarios(vuelosA1, aeropuertos)
val funcionPar15 = itinerariosPar(vuelosA1, aeropuertos)

val tiempoSec15 = tiempoDe { funcionSec15("ABQ", "SEA") }
val tiempoPar15 = tiempoDe { funcionPar15("ABQ", "SEA") }

println(s"Secuencial 15: $tiempoSec15")
println(s"Paralelo   15: $tiempoPar15")
println(s"Speedup 15 (sec/par): ${aceleracion(tiempoSec15, tiempoPar15)}")

// --- Dataset mediano: 40 vuelos (B1) LAX -> ATL ---
encabezado("itinerarios vs itinerariosPar - 40 vuelos (B1) LAX -> ATL")

val funcionSec40 = itinerarios(vuelosB1, aeropuertos)
val funcionPar40 = itinerariosPar(vuelosB1, aeropuertos)

val tiempoSec40 = tiempoDe { funcionSec40("LAX", "ATL") }
val tiempoPar40 = tiempoDe { funcionPar40("LAX", "ATL") }

println(s"Secuencial 40: $tiempoSec40")
println(s"Paralelo   40: $tiempoPar40")
println(s"Speedup 40 (sec/par): ${aceleracion(tiempoSec40, tiempoPar40)}")

/*
// --- Dataset grande: 100 vuelos (C1) SFO -> ATL ---
encabezado("itinerarios vs itinerariosPar - 100 vuelos (C1) SFO -> ATL")

val funcionSec100 = itinerarios(vuelosC1, aeropuertos)
val funcionPar100 = itinerariosPar(vuelosC1, aeropuertos)

val tiempoSec100 = tiempoDe { funcionSec100("SFO", "ATL") }
val tiempoPar100 = tiempoDe { funcionPar100("SFO", "ATL") }

println(s"Secuencial 100: $tiempoSec100")
println(s"Paralelo   100: $tiempoPar100")
println(s"Speedup 100 (sec/par): ${aceleracion(tiempoSec100, tiempoPar100)}")

// --- Dataset muy grande: 200 vuelos (C1 ++ C2) SFO -> ATL ---
encabezado("itinerarios vs itinerariosPar - 200 vuelos (C1 ++ C2) SFO -> ATL")

val funcionSec200 = itinerarios(conjuntoVuelos200, aeropuertos)
val funcionPar200 = itinerariosPar(conjuntoVuelos200, aeropuertos)

val tiempoSec200 = tiempoDe { funcionSec200("SFO", "ATL") }
val tiempoPar200 = tiempoDe { funcionPar200("SFO", "ATL") }

println(s"Secuencial 200: $tiempoSec200")
println(s"Paralelo   200: $tiempoPar200")
println(s"Speedup 200 (sec/par): ${aceleracion(tiempoSec200, tiempoPar200)}")

*/


// --------------------------------------------------------------------
// 2. itinerariosTiempo vs itinerariosTiempoPar
// --------------------------------------------------------------------

// --- Dataset pequeño: 15 vuelos (A1) ABQ -> SEA ---
encabezado("itinerariosTiempo vs itinerariosTiempoPar - 15 vuelos (A1) ABQ -> SEA")

val funTiempoSec15 = itinerariosTiempo(vuelosA1, aeropuertos)
val funTiempoPar15 = itinerariosTiempoPar(vuelosA1, aeropuertos)

val tTiempoSec15 = tiempoDe { funTiempoSec15("ABQ", "SEA") }
val tTiempoPar15 = tiempoDe { funTiempoPar15("ABQ", "SEA") }

println(s"Tiempo secuencial 15: $tTiempoSec15")
println(s"Tiempo paralelo 15: $tTiempoPar15")
println(s"Factor aceleración 15: ${aceleracion(tTiempoSec15, tTiempoPar15)}")

// --- Dataset mediano: 40 vuelos (B1) LAX -> ATL ---
encabezado("itinerariosTiempo vs itinerariosTiempoPar - 40 vuelos (B1) LAX -> ATL")

val funTiempoSec40 = itinerariosTiempo(vuelosB1, aeropuertos)
val funTiempoPar40 = itinerariosTiempoPar(vuelosB1, aeropuertos)

val tTiempoSec40 = tiempoDe { funTiempoSec40("LAX", "ATL") }
val tTiempoPar40 = tiempoDe { funTiempoPar40("LAX", "ATL") }

println(s"Tiempo secuencial 40: $tTiempoSec40")
println(s"Tiempo paralelo 40: $tTiempoPar40")
println(s"Factor aceleración 40: ${aceleracion(tTiempoSec40, tTiempoPar40)}")

/*
// --- Dataset grande: 100 vuelos (C1) SFO -> ATL ---
encabezado("itinerariosTiempo vs itinerariosTiempoPar - 100 vuelos (C1) SFO -> ATL")

val funTiempoSec100 = itinerariosTiempo(vuelosC1, aeropuertos)
val funTiempoPar100 = itinerariosTiempoPar(vuelosC1, aeropuertos)

val tTiempoSec100 = tiempoDe { funTiempoSec100("SFO", "ATL") }
val tTiempoPar100 = tiempoDe { funTiempoPar100("SFO", "ATL") }

println(s"Tiempo secuencial 100: $tTiempoSec100")
println(s"Tiempo paralelo 100: $tTiempoPar100")
println(s"Factor aceleración 100: ${aceleracion(tTiempoSec100, tTiempoPar100)}")

// --- Dataset muy grande: 200 vuelos (C1 ++ C2) SFO -> ATL ---
encabezado("itinerariosTiempo vs itinerariosTiempoPar - 200 vuelos (C1 ++ C2) SFO -> ATL")

val funTiempoSec200 = itinerariosTiempo(conjuntoVuelos200, aeropuertos)
val funTiempoPar200 = itinerariosTiempoPar(conjuntoVuelos200, aeropuertos)

val tTiempoSec200 = tiempoDe { funTiempoSec200("SFO", "ATL") }
val tTiempoPar200 = tiempoDe { funTiempoPar200("SFO", "ATL") }

println(s"Tiempo secuencial 200: $tTiempoSec200")
println(s"Tiempo paralelo 200: $tTiempoPar200")
println(s"Factor aceleración 200: ${aceleracion(tTiempoSec200, tTiempoPar200)}")

*/


// 3. ==================  itinerariosEscalas vs itinerariosEscalasPar  ==================

// --- Dataset pequeño: 15 vuelos (A1) ABQ -> SEA ---
encabezado("itinerariosEscalas vs itinerariosEscalasPar - 15 vuelos (A1) ABQ -> SEA")

val funEscalasSec15 = itinerariosEscalas(vuelosA1, aeropuertos)
val funEscalasPar15 = itinerariosEscalasPar(vuelosA1, aeropuertos)

val tEscalasSec15 = tiempoDe { funEscalasSec15("ABQ", "SEA") }
val tEscalasPar15 = tiempoDe { funEscalasPar15("ABQ", "SEA") }

println(s"Escalas secuencial 15: $tEscalasSec15")
println(s"Escalas paralelo 15: $tEscalasPar15")
println(s"Mejora escalas 15: ${aceleracion(tEscalasSec15, tEscalasPar15)}")

// --- Dataset mediano: 40 vuelos (B1) LAX -> ATL ---
encabezado("itinerariosEscalas vs itinerariosEscalasPar - 40 vuelos (B1) LAX -> ATL")

val funEscalasSec40 = itinerariosEscalas(vuelosB1, aeropuertos)
val funEscalasPar40 = itinerariosEscalasPar(vuelosB1, aeropuertos)

val tEscalasSec40 = tiempoDe { funEscalasSec40("LAX", "ATL") }
val tEscalasPar40 = tiempoDe { funEscalasPar40("LAX", "ATL") }

println(s"Escalas secuencial 40: $tEscalasSec40")
println(s"Escalas paralelo 40: $tEscalasPar40")
println(s"Mejora escalas 40: ${aceleracion(tEscalasSec40, tEscalasPar40)}")

/*
// --- Dataset grande: 100 vuelos (C1) SFO -> ATL ---
encabezado("itinerariosEscalas vs itinerariosEscalasPar - 100 vuelos (C1) SFO -> ATL")

val funEscalasSec100 = itinerariosEscalas(vuelosC1, aeropuertos)
val funEscalasPar100 = itinerariosEscalasPar(vuelosC1, aeropuertos)

val tEscalasSec100 = tiempoDe { funEscalasSec100("SFO", "ATL") }
val tEscalasPar100 = tiempoDe { funEscalasPar100("SFO", "ATL") }

println(s"Escalas secuencial 100: $tEscalasSec100")
println(s"Escalas paralelo 100: $tEscalasPar100")
println(s"Mejora escalas 100: ${aceleracion(tEscalasSec100, tEscalasPar100)}")

// --- Dataset muy grande: 200 vuelos (C1 ++ C2) SFO -> ATL ---
encabezado("itinerariosEscalas vs itinerariosEscalasPar - 200 vuelos (C1 ++ C2) SFO -> ATL")

val funEscalasSec200 = itinerariosEscalas(conjuntoVuelos200, aeropuertos)
val funEscalasPar200 = itinerariosEscalasPar(conjuntoVuelos200, aeropuertos)

val tEscalasSec200 = tiempoDe { funEscalasSec200("SFO", "ATL") }
val tEscalasPar200 = tiempoDe { funEscalasPar200("SFO", "ATL") }

println(s"Escalas secuencial 200: $tEscalasSec200")
println(s"Escalas paralelo 200: $tEscalasPar200")
println(s"Mejora escalas 200: ${aceleracion(tEscalasSec200, tEscalasPar200)}")
*/


// 4.==================  itinerariosAire vs itinerariosAirePar  ==================

// --- Dataset pequeño: 15 vuelos (A1) ABQ -> SEA ---
encabezado("itinerariosAire vs itinerariosAirePar - 15 vuelos (A1) ABQ -> SEA")

val funAireSec15 = itinerariosAire(vuelosA1, aeropuertos)
val funAirePar15 = itinerariosAirePar(vuelosA1, aeropuertos)

val tAireSec15 = tiempoDe { funAireSec15("ABQ", "SEA") }
val tAirePar15 = tiempoDe { funAirePar15("ABQ", "SEA") }

println(s"Tiempo en aire sec 15: $tAireSec15")
println(s"Tiempo en aire par 15: $tAirePar15")
println(s"Ganancia aire 15: ${aceleracion(tAireSec15, tAirePar15)}")

// --- Dataset mediano: 40 vuelos (B1) LAX -> ATL ---
encabezado("itinerariosAire vs itinerariosAirePar - 40 vuelos (B1) LAX -> ATL")

val funAireSec40 = itinerariosAire(vuelosB1, aeropuertos)
val funAirePar40 = itinerariosAirePar(vuelosB1, aeropuertos)

val tAireSec40 = tiempoDe { funAireSec40("LAX", "ATL") }
val tAirePar40 = tiempoDe { funAirePar40("LAX", "ATL") }

println(s"Tiempo en aire sec 40: $tAireSec40")
println(s"Tiempo en aire par 40: $tAirePar40")
println(s"Ganancia aire 40: ${aceleracion(tAireSec40, tAirePar40)}")

/*
// --- Dataset grande: 100 vuelos (C1) SFO -> ATL ---
encabezado("itinerariosAire vs itinerariosAirePar - 100 vuelos (C1) SFO -> ATL")

val funAireSec100 = itinerariosAire(vuelosC1, aeropuertos)
val funAirePar100 = itinerariosAirePar(vuelosC1, aeropuertos)

val tAireSec100 = tiempoDe { funAireSec100("SFO", "ATL") }
val tAirePar100 = tiempoDe { funAirePar100("SFO", "ATL") }

println(s"Tiempo en aire sec 100: $tAireSec100")
println(s"Tiempo en aire par 100: $tAirePar100")
println(s"Ganancia aire 100: ${aceleracion(tAireSec100, tAirePar100)}")

// --- Dataset muy grande: 200 vuelos (C1 ++ C2) SFO -> ATL ---
encabezado("itinerariosAire vs itinerariosAirePar - 200 vuelos (C1 ++ C2) SFO -> ATL")

val funAireSec200 = itinerariosAire(conjuntoVuelos200, aeropuertos)
val funAirePar200 = itinerariosAirePar(conjuntoVuelos200, aeropuertos)

val tAireSec200 = tiempoDe { funAireSec200("SFO", "ATL") }
val tAirePar200 = tiempoDe { funAirePar200("SFO", "ATL") }

println(s"Tiempo en aire sec 200: $tAireSec200")
println(s"Tiempo en aire par 200: $tAirePar200")
println(s"Ganancia aire 200: ${aceleracion(tAireSec200, tAirePar200)}")

*/

// 5.==================  itinerarioSalida vs itinerarioSalidaPar  ==================

// --- Dataset pequeño: 15 vuelos (A1) HOU -> BNA ---
encabezado("itinerarioSalida vs itinerarioSalidaPar - 15 vuelos (A1) HOU -> BNA")

val funSalidaSec15 = itinerarioSalida(vuelosA1, aeropuertos)
val funSalidaPar15 = itinerarioSalidaPar(vuelosA1, aeropuertos)

val tSalidaSec15 = tiempoDe { funSalidaSec15("HOU", "BNA", 18, 30) }
val tSalidaPar15 = tiempoDe { funSalidaPar15("HOU", "BNA", 18, 30) }

println(s"Salida programada sec 15: $tSalidaSec15")
println(s"Salida programada par 15: $tSalidaPar15")
println(s"Ratio salida 15: ${aceleracion(tSalidaSec15, tSalidaPar15)}")

// --- Dataset mediano: 40 vuelos (B1) DFW -> ORD ---
encabezado("itinerarioSalida vs itinerarioSalidaPar - 40 vuelos (B1) DFW -> ORD")

val funSalidaSec40 = itinerarioSalida(vuelosB1, aeropuertos)
val funSalidaPar40 = itinerarioSalidaPar(vuelosB1, aeropuertos)

val tSalidaSec40 = tiempoDe { funSalidaSec40("DFW", "ORD", 18, 30) }
val tSalidaPar40 = tiempoDe { funSalidaPar40("DFW", "ORD", 18, 30) }

println(s"Salida programada sec 40: $tSalidaSec40")
println(s"Salida programada par 40: $tSalidaPar40")
println(s"Ratio salida 40: ${aceleracion(tSalidaSec40, tSalidaPar40)}")

/*
// --- Dataset grande: 100 vuelos (C1) ORD -> TPA ---
encabezado("itinerarioSalida vs itinerarioSalidaPar - 100 vuelos (C1) ORD -> TPA")

val funSalidaSec100 = itinerarioSalida(vuelosC1, aeropuertos)
val funSalidaPar100 = itinerarioSalidaPar(vuelosC1, aeropuertos)

val tSalidaSec100 = tiempoDe { funSalidaSec100("ORD", "TPA", 18, 30) }
val tSalidaPar100 = tiempoDe { funSalidaPar100("ORD", "TPA", 18, 30) }

println(s"Salida programada sec 100: $tSalidaSec100")
println(s"Salida programada par 100: $tSalidaPar100")
println(s"Ratio salida 100: ${aceleracion(tSalidaSec100, tSalidaPar100)}")

// --- Dataset muy grande: 200 vuelos (C1 ++ C2) ORD -> TPA ---
encabezado("itinerarioSalida vs itinerarioSalidaPar - 200 vuelos (C1 ++ C2) ORD -> TPA")

val funSalidaSec200 = itinerarioSalida(conjuntoVuelos200, aeropuertos)
val funSalidaPar200 = itinerarioSalidaPar(conjuntoVuelos200, aeropuertos)

val tSalidaSec200 = tiempoDe { funSalidaSec200("ORD", "TPA", 18, 30) }
val tSalidaPar200 = tiempoDe { funSalidaPar200("ORD", "TPA", 18, 30) }

println(s"Salida programada sec 200: $tSalidaSec200")
println(s"Salida programada par 200: $tSalidaPar200")
println(s"Ratio salida 200: ${aceleracion(tSalidaSec200, tSalidaPar200)}")

*/

println("\n*** PRUEBAS DE RENDIMIENTO FINALIZADAS ***")