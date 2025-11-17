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

}