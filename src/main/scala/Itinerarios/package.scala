import Datos.{Aeropuerto, Itinerario, Vuelo}

package object Itinerarios {

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

  def obtenerGMT(aeropuertos: List[Aeropuerto], aeropuerto: String): Int = {
    (aeropuertos.find(_.Cod == aeropuerto).get.GMT / 100).toInt
  }

  def obtenerTiempoEspera(aeropuertos: List[Aeropuerto], itinerario: Itinerario, acc: Int): Int = {
    itinerario match {
      case Nil | _ :: Nil => acc
      case vuelo1 :: vuelo2 :: tail => {
        val (v1DstGMT, v2OrgGMT) = (obtenerGMT(aeropuertos, vuelo1.Dst), obtenerGMT(aeropuertos, vuelo2.Org))

        val HLv1GMT = if (vuelo1.HL - v1DstGMT < 0) (vuelo1.HL - v1DstGMT + 24) else (vuelo1.HL - v1DstGMT)
        val HSv2GMT = if (vuelo2.HS - v2OrgGMT < 0) (vuelo2.HS - v2OrgGMT + 24) else (vuelo2.HS - v2OrgGMT)

        val diferenciaHvGMT = (HSv2GMT * 60 + vuelo2.MS) - (HLv1GMT * 60 + vuelo1.ML)
        obtenerTiempoEspera(aeropuertos, vuelo2 :: tail, acc + diferenciaHvGMT)
      }
    }
  }
  
  
  def obtenerTiempoVuelo(aeropuertos: List[Aeropuerto], itinerario: Itinerario): Int = {
    itinerario.foldRight(0)((vuelo, acc) => {
      val (vOrgGMT, vDstGMT) = (obtenerGMT(aeropuertos, vuelo.Org), obtenerGMT(aeropuertos, vuelo.Dst))
      val HSvGMT = if (vuelo.HS - vOrgGMT < 0) (vuelo.HS - vOrgGMT + 24) else (vuelo.HS - vOrgGMT)
      val HLvGMT = if (vuelo.HL - vDstGMT < 0) (vuelo.HL - vDstGMT + 24) else (vuelo.HL - vDstGMT)
      val diferenciaHvGMT = ((HLvGMT * 60 + vuelo.ML) - (HSvGMT * 60 + vuelo.MS))
      if (diferenciaHvGMT < 0) acc + diferenciaHvGMT + (24 * 60) else acc + diferenciaHvGMT
    })
  }
  
  
  def itinerariosTiempo(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    (a: String, b: String) => {
      def obtenerTiempoTotal(itinerario: Itinerario): Int = {
        obtenerTiempoVuelo(aeropuertos, itinerario) + obtenerTiempoEspera(aeropuertos, itinerario, 0)
      }

      itinerarios(vuelos, aeropuertos)(a, b)
        .sortWith((a, b) => {
          obtenerTiempoTotal(a) < obtenerTiempoTotal(b)
        })
        .take(3)
    }
  }
}
