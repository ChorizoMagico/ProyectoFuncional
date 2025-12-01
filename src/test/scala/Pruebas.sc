
case class Aeropuerto(Cod: String, X: Int, Y: Int, GMT: Int)

case class Vuelo(Aln: String, Num: Int, Org: String, HS: Int, MS: Int, Dst: String, HL: Int, ML: Int, Esc: Int)

type Itinerario = List[Vuelo]

val aeropuertos= List(
  Aeropuerto("CTG", 300, 800, -500), // Cartagena
  Aeropuerto("PTY", 400, 1000, -500), // Ciudad de Panamá
)

val vuelos = List(
  Vuelo("COPA", 1234, "CTG", 10, 0, "PTY", 11, 30, 0),
  Vuelo("COPA", 1234, "CTG", 2, 0, "PTY", 11, 30, 0),
  Vuelo("COPA", 1234, "CTG", 11, 15, "PTY", 11, 30, 0),

)


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


def itinerarioSalida(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String, Int, Int) => Itinerario = {

  def aMinutos(h: Int, m: Int): Int = h * 60 + m

  (org: String, dst: String, hCita: Int, mCita: Int) => {

    val todosItinerarios = itinerarios(vuelos, aeropuertos)(org, dst)

    if (todosItinerarios.isEmpty) {
      throw new Exception(s"No existen rutas entre $org y $dst")
    }

    val citaMinutos = aMinutos(hCita, mCita)

    val mejorItinerario = todosItinerarios.maxBy { itin =>
      val primerVuelo = itin.head
      val ultimoVuelo = itin.last

      val salidaMinutos = aMinutos(primerVuelo.HS, primerVuelo.MS)
      val llegadaMinutos = aMinutos(ultimoVuelo.HL, ultimoVuelo.ML)

      if (llegadaMinutos <= citaMinutos) {
        salidaMinutos
      } else {
        salidaMinutos - 1440
      }
    }

    mejorItinerario
  }
}


val itSalidaCurso = itinerarioSalida(vuelos, aeropuertos)

val itsal1 = itSalidaCurso("CTG", "PTY", 11,40)
val itsal2 = itSalidaCurso("CTG", "PTY", 11,55)
val itsal3 = itSalidaCurso("CTG", "PTY", 10,31)