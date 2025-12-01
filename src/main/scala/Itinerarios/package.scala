import scala.annotation.tailrec

package object Itinerarios {
  import Datos._
  import common._


  // Función 1. itinerarios
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


  // Función 2. itinerariosTiempo

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

  // Función 3. itinerariosEscalas

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

  //Función 4. itinerariosAire

  def itinerariosAire(vuelos: List[Vuelo],
                      aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    (cod1: String, cod2: String) => {
      def tiempoEnAire(itinerario: Itinerario): Int =
        obtenerTiempoVuelo(aeropuertos, itinerario)

      itinerarios(vuelos, aeropuertos)(cod1, cod2)
        .sortBy(tiempoEnAire)
        .take(3)
    }
  }

  //Función 5. itinerarioSalida

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