import scala.annotation.tailrec

package object Itinerarios {
  import Datos._
  import common._


  // ===================================================================
  // Función 1. itinerarios
  // Descripción: Encuentra todas las rutas posibles entre dos aeropuertos
  // Retorna una función que dados origen y destino, devuelve lista de itinerarios
  // ===================================================================
  def itinerarios(vuelos: List[Vuelo],
                  aeropuertos:
                  List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // Verifica si un elemento pertenece a una lista
    def pertenece(x: String, xs: List[String]): Boolean = xs match {
      case Nil => false
      case h :: t =>
        if (h == x) true
        else pertenece(x, t)
    }

    // Explora recursivamente todos los caminos desde actual hasta dst
    // evitando ciclos con la lista de visitados
    def verCaminos(actual: String,
                   dst: String,
                   visitados: List[String]): List[Itinerario] = {

      if (actual == dst) {
        List(Nil) // Caso base: llegamos al destino
      } else {
        val caminos =
          for {
            v <- vuelos
            if v.Org == actual // Vuelos que salen del aeropuerto actual
            if !pertenece(v.Dst, visitados) // Evitar ciclos
            subcamino <- verCaminos(v.Dst, dst, v.Dst :: visitados) // Recursión
          } yield v :: subcamino

        caminos
      }
    }

    (org: String, dst: String) => {
      verCaminos(org, dst, List(org))
    }
  }


  // ===================================================================
  // Función 2. itinerariosTiempo
  // Descripción: Encuentra los 3 itinerarios más rápidos considerando
  // tiempo de vuelo + tiempo de espera entre conexiones
  // ===================================================================

  // Obtiene la zona horaria GMT de un aeropuerto
  def obtenerGMT(aeropuertos: List[Aeropuerto], aeropuerto: String): Int = {
    (aeropuertos.find(_.Cod == aeropuerto).get.GMT / 100).toInt
  }

  // Calcula el tiempo total de espera entre conexiones
  // Convierte todo a GMT para cálculos correctos
  def obtenerTiempoEspera(aeropuertos: List[Aeropuerto], itinerario: Itinerario, acc: Int): Int = {
    itinerario match {
      case Nil | _ :: Nil => acc // Sin espera si hay 0 o 1 vuelo
      case vuelo1 :: vuelo2 :: tail => {
        val (v1DstGMT, v2OrgGMT) = (obtenerGMT(aeropuertos, vuelo1.Dst), obtenerGMT(aeropuertos, vuelo2.Org))

        // Ajustar horas a GMT
        val HLv1GMT = if (vuelo1.HL - v1DstGMT < 0) (vuelo1.HL - v1DstGMT + 24) else (vuelo1.HL - v1DstGMT)
        val HSv2GMT = if (vuelo2.HS - v2OrgGMT < 0) (vuelo2.HS - v2OrgGMT + 24) else (vuelo2.HS - v2OrgGMT)

        // Calcular diferencia en minutos
        val diferenciaHvGMT = (HSv2GMT * 60 + vuelo2.MS) - (HLv1GMT * 60 + vuelo1.ML)
        obtenerTiempoEspera(aeropuertos, vuelo2 :: tail, acc + diferenciaHvGMT)
      }
    }
  }

  // Calcula el tiempo total en el aire para un itinerario
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

      // Ordenar por tiempo total y tomar los 3 mejores
      itinerarios(vuelos, aeropuertos)(a, b)
        .sortWith((a, b) => {
          obtenerTiempoTotal(a) < obtenerTiempoTotal(b)
        })
        .take(3)
    }
  }

  // ===================================================================
  // Función 3. itinerariosEscalas
  // Descripción: Encuentra los 3 itinerarios con menor número de escalas
  // Cuenta tanto las conexiones entre vuelos como las escalas internas
  // ===================================================================

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    // Calcula el número total de escalas: (conexiones - 1) + escalas internas
    def numEscalasTotales(it: Itinerario): Int = {
      @tailrec
      def loop(resto: Itinerario, vuelos: Int, escs: Int): Int = resto match {
        case Nil =>
          if(vuelos == 0) vuelos + escs
          else (vuelos - 1) + escs // Restar 1 porque n vuelos = n-1 conexiones
        case v :: t =>
          loop(t, vuelos + 1, escs + v.Esc)
      }

      loop(it, 0, 0)
    }

    val itBase = itinerarios(vuelos, aeropuertos)

    val maxItinerarios = 3

    (org: String, dst: String) => {
      val todos: List[Itinerario] = itBase(org, dst)

      val ordenados: List[Itinerario] =
        todos.sortBy(it => numEscalasTotales(it))

      ordenados.take(maxItinerarios)
    }

  }

  // ===================================================================
  // Función 4. itinerariosAire
  // Descripción: Encuentra los 3 itinerarios con menor tiempo en el aire
  // Solo considera tiempo de vuelo, ignora tiempos de espera
  // ===================================================================

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

  // ===================================================================
  // Función 5. itinerarioSalida
  // Descripción: Encuentra el itinerario que sale lo más tarde posible
  // pero llega antes de una hora de cita especificada
  // ===================================================================

  def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {

    def aMinutos(h: Int, m: Int): Int = h * 60 + m

    (org: String, dst: String, hCita: Int, mCita: Int) => {

      val todosItinerarios = itinerarios(vuelos, aeropuertos)(org, dst)

      if (todosItinerarios.isEmpty) {
        throw new Exception(s"No existen rutas entre $org y $dst")
      }

      val citaMinutos = aMinutos(hCita, mCita)

      // Maximizar hora de salida, penalizando itinerarios que llegan tarde
      val mejorItinerario = todosItinerarios.maxBy { itin =>
        val primerVuelo = itin.head
        val ultimoVuelo = itin.last

        val salidaMinutos = aMinutos(primerVuelo.HS, primerVuelo.MS)
        val llegadaMinutos = aMinutos(ultimoVuelo.HL, ultimoVuelo.ML)

        if (llegadaMinutos <= citaMinutos) {
          salidaMinutos // Llega a tiempo: usar hora de salida
        } else {
          salidaMinutos - 1440 // Llega tarde: penalizar restando un día
        }
      }

      mejorItinerario
    }
  }


}