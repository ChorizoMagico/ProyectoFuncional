import scala.annotation.tailrec

package object ItinerariosPar {
  
  import Datos._
  import common._
  import Itinerarios._

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