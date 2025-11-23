import scala.annotation.tailrec

package object ItinerariosPar {
  
  import Datos._
  import common._
  import Itinerarios._

  def obtenerTiempoEsperaPar(aeropuertos: List[Aeropuerto], itinerario: Itinerario, acc: Int): Int = {
    itinerario match {
      case Nil | _ :: Nil => acc
      case vuelo1 :: vuelo2 :: tail => {
        val (v1DstGMT, v2OrgGMT) = parallel(obtenerGMT(aeropuertos, vuelo1.Dst), obtenerGMT(aeropuertos, vuelo2.Org))

        val HLv1GMT = if (vuelo1.HL - v1DstGMT < 0) (vuelo1.HL - v1DstGMT + 24) else (vuelo1.HL - v1DstGMT)
        val HSv2GMT = if (vuelo2.HS - v2OrgGMT < 0) (vuelo2.HS - v2OrgGMT + 24) else (vuelo2.HS - v2OrgGMT)

        val diferenciaHvGMT = (HSv2GMT * 60 + vuelo2.MS) - (HLv1GMT * 60 + vuelo1.ML)

        obtenerTiempoEsperaPar(aeropuertos, vuelo2 :: tail, acc + diferenciaHvGMT)
      }
    }
  }

  def obtenerTiempoVueloPar(aeropuertos: List[Aeropuerto], itinerario: Itinerario): Int = {
    itinerario.foldRight(0)((vuelo, acc) => {
      val (vOrgGMT, vDstGMT) = parallel(obtenerGMT(aeropuertos, vuelo.Org), obtenerGMT(aeropuertos, vuelo.Dst))
      val HSvGMT = if (vuelo.HS - vOrgGMT < 0) (vuelo.HS - vOrgGMT + 24) else (vuelo.HS - vOrgGMT)
      val HLvGMT = if (vuelo.HL - vDstGMT < 0) (vuelo.HL - vDstGMT + 24) else (vuelo.HL - vDstGMT)
      val diferenciaHvGMT = ((HLvGMT * 60 + vuelo.ML) - (HSvGMT * 60 + vuelo.MS))
      if (diferenciaHvGMT < 0) acc + diferenciaHvGMT + (24 * 60) else acc + diferenciaHvGMT
    })
  }

  def itinerariosPar(vuelos: List[Vuelo],
                     aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    //Verifica si un elemento pertenece a una lista
    @tailrec
    def pertenece(x: String, xs: List[String]): Boolean = xs match {
      case Nil => false
      case h :: t =>
        if (h == x) true
        else pertenece(x, t)
    }

    // Umbral variable
    val UMBRAL = 4

    // Versión paralela de verCaminos
    def verCaminosPar(actual: String,
                      dst: String,
                      visitados: List[String]): List[Itinerario] = {

      if (actual == dst) {
        List(Nil)
      } else {
        val salidas =
          vuelos.filter(v => v.Org == actual && !pertenece(v.Dst, visitados))

        // Procesa una lista de vuelos de salida (secuencialmente)
        def procesar(xs: List[Vuelo]): List[Itinerario] =
          xs.flatMap { v =>
            val subcaminos = verCaminosPar(v.Dst, dst, v.Dst :: visitados)
            subcaminos.map(camino => v :: camino)
          }

        if (salidas.length <= UMBRAL) {
          // Rama pequeña: mejor secuencial
          procesar(salidas)
        } else {
          // Rama grande: divide la lista de salidas y procesa en paralelo
          val (izq, der) = salidas.splitAt(salidas.length / 2)

          val tareaIzq = task {
            procesar(izq)
          }
          val resDer = procesar(der)

          val resIzq = tareaIzq.join
          resIzq ++ resDer
        }
      }
    }

    // Función retornada
    (org: String, dst: String) => {
      verCaminosPar(org, dst, List(org))
    }
  }

  def itinerariosTiempoPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    (a: String, b: String) => {

      // Función que obtiene el tiempo total de un itinerario
      def obtenerTiempoTotal(itinerario: Itinerario): Int = {
        val (tiempoVuelo, tiempoEspera) = parallel(obtenerTiempoVueloPar(aeropuertos, itinerario), obtenerTiempoEsperaPar(aeropuertos, itinerario, 0))
        tiempoVuelo + tiempoEspera
      }

      // Obtener todos los itinerarios entre los aeropuertos a y b
      val todosItinerarios = itinerariosPar(vuelos, aeropuertos)(a, b)

      // Mapear cada itinerario a su tiempo total en paralelo
      todosItinerarios.map(itinerario => task((itinerario, obtenerTiempoTotal(itinerario))))
        .map(_.join()).sortBy(_._2).take(3).map(_._1)
    }
  }

  def itinerariosEscalasPar(vuelos: List[Vuelo],
                            aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    def numEscalasTotales(it: Itinerario): Int = {
      @tailrec
      def loop(resto: Itinerario, vuelos: Int, escs: Int): Int = resto match {
        case Nil => vuelos + escs
        case v :: t => loop(t, vuelos + 1, escs + v.Esc)
      }

      loop(it, 0, 0)
    }

    val itBasePar = itinerariosPar(vuelos, aeropuertos)

    val maxItinerarios = 3

    (org: String, dst: String) => {
      val todos: List[Itinerario] = itBasePar(org, dst)

      val ordenados: List[Itinerario] =
        todos.sortBy(it => numEscalasTotales(it))

      ordenados.take(maxItinerarios)
    }
  }

}