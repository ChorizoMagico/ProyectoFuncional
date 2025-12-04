import Datos._
import Itinerarios._
import ItinerariosPar._
import org.scalameter._

// ========= Función del profe para medir tiempo =========
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

def titulo(txt: String): Unit = {
  println("\n" + "=" * 80)
  println(txt)
  println("=" * 80)
}

def speedup(tSeq: Quantity[Double], tPar: Quantity[Double]): Double =
  tSeq.value / tPar.value

val vuelos200b = vuelosC1 ++ vuelosC2

// ==================  itinerariosEscalas vs itinerariosEscalasPar  ==================

// --- 15 vuelos (A1) HOU -> BNA ---
titulo("itinerariosEscalas vs itinerariosEscalasPar - 15 vuelos (A1) HOU -> BNA")

val funEscSeq15 = itinerariosEscalas(vuelosA1, aeropuertos)
val funEscPar15 = itinerariosEscalasPar(vuelosA1, aeropuertos)

val tEscSeq15 = tiempoDe { funEscSeq15("HOU", "BNA") }
val tEscPar15 = tiempoDe { funEscPar15("HOU", "BNA") }

println(s"Escalas 15 seq: $tEscSeq15")
println(s"Escalas 15 par: $tEscPar15")
println(s"Aceleración Escalas 15: ${speedup(tEscSeq15, tEscPar15)}")

// --- 40 vuelos (B1) DFW -> ORD ---
titulo("itinerariosEscalas vs itinerariosEscalasPar - 40 vuelos (B1) DFW -> ORD")

val funEscSeq40 = itinerariosEscalas(vuelosB1, aeropuertos)
val funEscPar40 = itinerariosEscalasPar(vuelosB1, aeropuertos)

val tEscSeq40 = tiempoDe { funEscSeq40("DFW", "ORD") }
val tEscPar40 = tiempoDe { funEscPar40("DFW", "ORD") }

println(s"Escalas 40 seq: $tEscSeq40")
println(s"Escalas 40 par: $tEscPar40")
println(s"Aceleración Escalas 40: ${speedup(tEscSeq40, tEscPar40)}")

// --- 100 vuelos (C1) ORD -> TPA ---
titulo("itinerariosEscalas vs itinerariosEscalasPar - 100 vuelos (C1) ORD -> TPA")

val funEscSeq100 = itinerariosEscalas(vuelosC1, aeropuertos)
val funEscPar100 = itinerariosEscalasPar(vuelosC1, aeropuertos)

val tEscSeq100 = tiempoDe { funEscSeq100("ORD", "TPA") }
val tEscPar100 = tiempoDe { funEscPar100("ORD", "TPA") }

println(s"Escalas 100 seq: $tEscSeq100")
println(s"Escalas 100 par: $tEscPar100")
println(s"Aceleración Escalas 100: ${speedup(tEscSeq100, tEscPar100)}")

// --- 200 vuelos (C1 ++ C2) ORD -> TPA ---
titulo("itinerariosEscalas vs itinerariosEscalasPar - 200 vuelos (C1 ++ C2) ORD -> TPA")

val funEscSeq200 = itinerariosEscalas(vuelos200b, aeropuertos)
val funEscPar200 = itinerariosEscalasPar(vuelos200b, aeropuertos)

val tEscSeq200 = tiempoDe { funEscSeq200("ORD", "TPA") }
val tEscPar200 = tiempoDe { funEscPar200("ORD", "TPA") }

println(s"Escalas 200 seq: $tEscSeq200")
println(s"Escalas 200 par: $tEscPar200")
println(s"Aceleración Escalas 200: ${speedup(tEscSeq200, tEscPar200)}")



/* ==================  itinerariosAire vs itinerariosAirePar  ==================

// --- 15 vuelos (A1) HOU -> BNA ---
titulo("itinerariosAire vs itinerariosAirePar - 15 vuelos (A1) HOU -> BNA")

val funAirSeq15 = itinerariosAire(vuelosA1, aeropuertos)
val funAirPar15 = itinerariosAirePar(vuelosA1, aeropuertos)

val tAirSeq15 = tiempoDe { funAirSeq15("HOU", "BNA") }
val tAirPar15 = tiempoDe { funAirPar15("HOU", "BNA") }

println(s"Aire 15 seq: $tAirSeq15")
println(s"Aire 15 par: $tAirPar15")
println(s"Aceleración Aire 15: ${speedup(tAirSeq15, tAirPar15)}")

// --- 40 vuelos (B1) DFW -> ORD ---
titulo("itinerariosAire vs itinerariosAirePar - 40 vuelos (B1) DFW -> ORD")

val funAirSeq40 = itinerariosAire(vuelosB1, aeropuertos)
val funAirPar40 = itinerariosAirePar(vuelosB1, aeropuertos)

val tAirSeq40 = tiempoDe { funAirSeq40("DFW", "ORD") }
val tAirPar40 = tiempoDe { funAirPar40("DFW", "ORD") }

println(s"Aire 40 seq: $tAirSeq40")
println(s"Aire 40 par: $tAirPar40")
println(s"Aceleración Aire 40: ${speedup(tAirSeq40, tAirPar40)}")

// --- 100 vuelos (C1) ORD -> TPA ---
titulo("itinerariosAire vs itinerariosAirePar - 100 vuelos (C1) ORD -> TPA")

val funAirSeq100 = itinerariosAire(vuelosC1, aeropuertos)
val funAirPar100 = itinerariosAirePar(vuelosC1, aeropuertos)

val tAirSeq100 = tiempoDe { funAirSeq100("ORD", "TPA") }
val tAirPar100 = tiempoDe { funAirPar100("ORD", "TPA") }

println(s"Aire 100 seq: $tAirSeq100")
println(s"Aire 100 par: $tAirPar100")
println(s"Aceleración Aire 100: ${speedup(tAirSeq100, tAirPar100)}")

// --- 200 vuelos (C1 ++ C2) ORD -> TPA ---
titulo("itinerariosAire vs itinerariosAirePar - 200 vuelos (C1 ++ C2) ORD -> TPA")

val funAirSeq200 = itinerariosAire(vuelos200b, aeropuertos)
val funAirPar200 = itinerariosAirePar(vuelos200b, aeropuertos)

val tAirSeq200 = tiempoDe { funAirSeq200("ORD", "TPA") }
val tAirPar200 = tiempoDe { funAirPar200("ORD", "TPA") }

println(s"Aire 200 seq: $tAirSeq200")
println(s"Aire 200 par: $tAirPar200")
println(s"Aceleración Aire 200: ${speedup(tAirSeq200, tAirPar200)}")

*/

/* ==================  itinerarioSalida vs itinerarioSalidaPar  ==================

// --- 15 vuelos (A1) HOU -> BNA ---
titulo("itinerarioSalida vs itinerarioSalidaPar - 15 vuelos (A1) HOU -> BNA")

val funSalSeq15 = itinerarioSalida(vuelosA1, aeropuertos)
val funSalPar15 = itinerarioSalidaPar(vuelosA1, aeropuertos)

val tSalSeq15 = tiempoDe { funSalSeq15("HOU", "BNA", 18, 30) }
val tSalPar15 = tiempoDe { funSalPar15("HOU", "BNA", 18, 30) }

println(s"Salida 15 seq: $tSalSeq15")
println(s"Salida 15 par: $tSalPar15")
println(s"Aceleración Salida 15: ${speedup(tSalSeq15, tSalPar15)}")

// --- 40 vuelos (B1) DFW -> ORD ---
titulo("itinerarioSalida vs itinerarioSalidaPar - 40 vuelos (B1) DFW -> ORD")

val funSalSeq40 = itinerarioSalida(vuelosB1, aeropuertos)
val funSalPar40 = itinerarioSalidaPar(vuelosB1, aeropuertos)

val tSalSeq40 = tiempoDe { funSalSeq40("DFW", "ORD", 18, 30) }
val tSalPar40 = tiempoDe { funSalPar40("DFW", "ORD", 18, 30) }

println(s"Salida 40 seq: $tSalSeq40")
println(s"Salida 40 par: $tSalPar40")
println(s"Aceleración Salida 40: ${speedup(tSalSeq40, tSalPar40)}")

// --- 100 vuelos (C1) ORD -> TPA ---
titulo("itinerarioSalida vs itinerarioSalidaPar - 100 vuelos (C1) ORD -> TPA")

val funSalSeq100 = itinerarioSalida(vuelosC1, aeropuertos)
val funSalPar100 = itinerarioSalidaPar(vuelosC1, aeropuertos)

val tSalSeq100 = tiempoDe { funSalSeq100("ORD", "TPA", 18, 30) }
val tSalPar100 = tiempoDe { funSalPar100("ORD", "TPA", 18, 30) }

println(s"Salida 100 seq: $tSalSeq100")
println(s"Salida 100 par: $tSalPar100")
println(s"Aceleración Salida 100: ${speedup(tSalSeq100, tSalPar100)}")

// --- 200 vuelos (C1 ++ C2) ORD -> TPA ---
titulo("itinerarioSalida vs itinerarioSalidaPar - 200 vuelos (C1 ++ C2) ORD -> TPA")

val funSalSeq200 = itinerarioSalida(vuelos200b, aeropuertos)
val funSalPar200 = itinerarioSalidaPar(vuelos200b, aeropuertos)

val tSalSeq200 = tiempoDe { funSalSeq200("ORD", "TPA", 18, 30) }
val tSalPar200 = tiempoDe { funSalPar200("ORD", "TPA", 18, 30) }

println(s"Salida 200 seq: $tSalSeq200")
println(s"Salida 200 par: $tSalPar200")
println(s"Aceleración Salida 200: ${speedup(tSalSeq200, tSalPar200)}")

*/

println("\nBENCHMARKS COMPLETADOS")
