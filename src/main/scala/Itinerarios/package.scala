import scala.annotation.tailrec

package object Itinerarios {
  import Datos._
  import common._

  def itinerarios(vuelos: List[Vuelo],
                  aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    def pertenece(x: String, xs: List[String]): Boolean = xs match {
      case Nil => false
      case h :: t =>
        if (h == x) true
        else pertenece(x, t)
    }

    def verCaminos(actual: String,
                   dst: String,
                   visitados: List[String]): List[Itinerario] = {

      if (actual == dst) {
        List(Nil)
      } else {
        val caminos =
          for {
            v <- vuelos
            if v.Org == actual
            if !pertenece(v.Dst, visitados)
            subcamino <- verCaminos(v.Dst, dst, v.Dst :: visitados)
          } yield v :: subcamino

        caminos
      }
    }

    (org: String, dst: String) => {
      verCaminos(org, dst, List(org))
    }
  }

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    def numEscalasTotales(it: Itinerario): Int = {
      @tailrec
      def loop(resto: Itinerario, vuelos: Int, escs: Int): Int = resto match {
        case Nil =>
          vuelos + escs
        case v :: t =>
          loop(t, vuelos + 1, escs + v.Esc)
      }

      loop(it, 0, 0)
    }

    val itBase = itinerarios(vuelos, aeropuertos) // tu función 3.1

    val maxItinerarios = 3

    (org: String, dst: String) => {
      val todos: List[Itinerario] = itBase(org, dst)

      val ordenados: List[Itinerario] =
        todos.sortBy(it => numEscalasTotales(it))

      ordenados.take(maxItinerarios)
    }
    
  }

}