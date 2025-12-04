import Datos._
import Itinerarios._
import ItinerariosPar._
import org.scalameter._

// ========= Función del profe para medir tiempo =========
def tiempoDe[T](body: => T) = {
  val timeA1 = config(
    KeyValue(Key.exec.minWarmupRuns -> 20),
    KeyValue(Key.exec.maxWarmupRuns -> 60),
    KeyValue(Key.verbose -> false)
  ) withWarmer(new Warmer.Default) measure (body)
  timeA1
}


def titulo(txt: String): Unit = {
  println("\n" + "=" * 80)
  println(txt)
  println("=" * 80)
}

def speedup(tSeq: Quantity[Double], tPar: Quantity[Double]): Double =
  tSeq.value / tPar.value

// --------------------------------------------------------------------
// 1. itinerariosTiempo vs itinerariosTiempoPar  (ACTIVO)
// --------------------------------------------------------------------

// --- 15 vuelos (A1) HOU -> BNA ---
titulo("itinerariosTiempo vs itinerariosTiempoPar - 15 vuelos (A1) HOU -> BNA")

val funTpoSeq15 = itinerariosTiempo(vuelosA1, aeropuertos)
val funTpoPar15 = itinerariosTiempoPar(vuelosA1, aeropuertos)

val tSeq15 = tiempoDe { funTpoSeq15("HOU", "BNA") }
val tPar15 = tiempoDe { funTpoPar15("HOU", "BNA") }

println(s"Secuencial 15: $tSeq15")
println(s"Paralelo   15: $tPar15")
println(s"Aceleración 15 (seq/par): ${speedup(tSeq15, tPar15)}")

// --- 40 vuelos (B1) DFW -> ORD ---
titulo("itinerariosTiempo vs itinerariosTiempoPar - 40 vuelos (B1) DFW -> ORD")

val funTpoSeq40 = itinerariosTiempo(vuelosB1, aeropuertos)
val funTpoPar40 = itinerariosTiempoPar(vuelosB1, aeropuertos)

val tSeq40 = tiempoDe { funTpoSeq40("DFW", "ORD") }
val tPar40 = tiempoDe { funTpoPar40("DFW", "ORD") }

println(s"Secuencial 40: $tSeq40")
println(s"Paralelo   40: $tPar40")
println(s"Aceleración 40 (seq/par): ${speedup(tSeq40, tPar40)}")

// --- 100 vuelos (C1) ORD -> TPA ---
titulo("itinerariosTiempo vs itinerariosTiempoPar - 100 vuelos (C1) ORD -> TPA")

val funTpoSeq100 = itinerariosTiempo(vuelosC1, aeropuertos)
val funTpoPar100 = itinerariosTiempoPar(vuelosC1, aeropuertos)

val tSeq100 = tiempoDe { funTpoSeq100("ORD", "TPA") }
val tPar100 = tiempoDe { funTpoPar100("ORD", "TPA") }

println(s"Secuencial 100: $tSeq100")
println(s"Paralelo   100: $tPar100")
println(s"Aceleración 100 (seq/par): ${speedup(tSeq100, tPar100)}")

// --- 200 vuelos (C1 ++ C2) ORD -> TPA ---
titulo("itinerariosTiempo vs itinerariosTiempoPar - 200 vuelos (C1 ++ C2) ORD -> TPA")

val vuelos200b = vuelosC1 ++ vuelosC2

val funTpoSeq200 = itinerariosTiempo(vuelos200b, aeropuertos)
val funTpoPar200 = itinerariosTiempoPar(vuelos200b, aeropuertos)

val tSeq200 = tiempoDe { funTpoSeq200("ORD", "TPA") }
val tPar200 = tiempoDe { funTpoPar200("ORD", "TPA") }

println(s"Secuencial 200: $tSeq200")
println(s"Paralelo   200: $tPar200")
println(s"Aceleración 200 (seq/par): ${speedup(tSeq200, tPar200)}")

// --------------------------------------------------------------------
// 2. Esqueleto de PRUEBAS PARA TODAS LAS FUNCIONES (COMENTADO)
// --------------------------------------------------------------------

/* ==================  itinerarios vs itinerariosPar  ==================

// --- 15 vuelos (A1) HOU -> BNA ---
titulo("itinerarios vs itinerariosPar - 15 vuelos (A1) HOU -> BNA")

val funItsSeq15 = itinerarios(vuelosA1, aeropuertos)
val funItsPar15 = itinerariosPar(vuelosA1, aeropuertos)

val tItsSeq15 = tiempoDe { funItsSeq15("HOU", "BNA") }
val tItsPar15 = tiempoDe { funItsPar15("HOU", "BNA") }

println(s"itinerarios 15 seq: $tItsSeq15")
println(s"itinerarios 15 par: $tItsPar15")
println(s"Aceleración itinerarios 15: ${speedup(tItsSeq15, tItsPar15)}")

// --- 40 vuelos (B1) DFW -> ORD ---
titulo("itinerarios vs itinerariosPar - 40 vuelos (B1) DFW -> ORD")

val funItsSeq40 = itinerarios(vuelosB1, aeropuertos)
val funItsPar40 = itinerariosPar(vuelosB1, aeropuertos)

val tItsSeq40 = tiempoDe { funItsSeq40("DFW", "ORD") }
val tItsPar40 = tiempoDe { funItsPar40("DFW", "ORD") }

println(s"itinerarios 40 seq: $tItsSeq40")
println(s"itinerarios 40 par: $tItsPar40")
println(s"Aceleración itinerarios 40: ${speedup(tItsSeq40, tItsPar40)}")

// --- 100 vuelos (C1) ORD -> TPA ---
titulo("itinerarios vs itinerariosPar - 100 vuelos (C1) ORD -> TPA")

val funItsSeq100 = itinerarios(vuelosC1, aeropuertos)
val funItsPar100 = itinerariosPar(vuelosC1, aeropuertos)

val tItsSeq100 = tiempoDe { funItsSeq100("ORD", "TPA") }
val tItsPar100 = tiempoDe { funItsPar100("ORD", "TPA") }

println(s"itinerarios 100 seq: $tItsSeq100")
println(s"itinerarios 100 par: $tItsPar100")
println(s"Aceleración itinerarios 100: ${speedup(tItsSeq100, tItsPar100)}")

// --- 200 vuelos (C1 ++ C2) ORD -> TPA ---
titulo("itinerarios vs itinerariosPar - 200 vuelos (C1 ++ C2) ORD -> TPA")

val funItsSeq200 = itinerarios(vuelos200b, aeropuertos)
val funItsPar200 = itinerariosPar(vuelos200b, aeropuertos)

val tItsSeq200 = tiempoDe { funItsSeq200("ORD", "TPA") }
val tItsPar200 = tiempoDe { funItsPar200("ORD", "TPA") }

println(s"itinerarios 200 seq: $tItsSeq200")
println(s"itinerarios 200 par: $tItsPar200")
println(s"Aceleración itinerarios 200: ${speedup(tItsSeq200, tItsPar200)}")

*/

/* ==================  itinerariosEscalas vs itinerariosEscalasPar  ==================

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

*/

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
