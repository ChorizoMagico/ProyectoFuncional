import scala.annotation.tailrec

package object Itinerarios {
  import Datos._
  import common._

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    def pertenece(x: String, xs: List[String]): Boolean = xs match {
      case Nil => false
      case h :: t =>
        if (h == x) true
        else pertenece(x, t)
    }

    def verCaminos(actual: String, dst: String, visitados: List[String]): List[Itinerario] = {
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

  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {

    def calcularHoraLlegada(itinerario: Itinerario, aeropuertos: List[Aeropuerto]): (Int, Int) = {

      val ultimoVuelo = itinerario.last
      val aeropuertoDestino = aeropuertos.find(_.Cod == ultimoVuelo.Dst).fold {
        throw new Exception(s"Aeropuerto ${ultimoVuelo.Dst} no encontrado")
      } { aeropuerto =>
        aeropuerto
      }

      val gmtDestino = aeropuertoDestino.GMT
      val horaUTC = convertirLocalAUTC(ultimoVuelo.HL, ultimoVuelo.ML, gmtDestino)

      horaUTC
    }

    def convertirLocalAUTC(horaLocal: Int, minutoLocal: Int, gmt: Int): (Int, Int) = {
      var horaUTC = horaLocal - gmt
      var minutoUTC = minutoLocal

      if (horaUTC < 0) horaUTC += 24
      if (horaUTC >= 24) horaUTC -= 24

      (horaUTC, minutoUTC)
    }

    def horaSalidaMinutos(itinerario: Itinerario): Int = {
      val primerVuelo = itinerario.head
      primerVuelo.HS * 60 + primerVuelo.MS
    }

    def llegaAntesDeCita(itinerario: Itinerario, hCita: Int, mCita: Int): Boolean = {
      val (hLlegada, mLlegada) = calcularHoraLlegada(itinerario, aeropuertos)
      hLlegada < hCita || (hLlegada == hCita && mLlegada <= mCita)
    }

    (org: String, dst: String, hDst: Int, mDst: Int) => {

      val todosItinerarios = itinerarios(vuelos, aeropuertos)(org, dst)

      if (todosItinerarios.isEmpty) {
        throw new Exception(s"No hay itinerarios de $org a $dst disponibles!")
      }

      val itinerariosValidos = todosItinerarios.filter(itin =>
        llegaAntesDeCita(itin, hDst, mDst)
      )

      if (itinerariosValidos.isEmpty) {
        throw new Exception(s"No hay itinerarios que lleguen antes de $hDst:$mDst")
      }

      itinerariosValidos.maxBy(horaSalidaMinutos)
    }

  }


}