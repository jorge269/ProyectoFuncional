import scala.util.Random
import scala.annotation.tailrec
import scala.concurrent._
import scala.concurrent.duration._
package object Riego {

  //Un tablon es una tripleta con el tiempo de supervivencia.
  //el tiempo de riego y la prioridad del tablon.
  type Tablon = (Int, Int, Int)

  //Una finca es un vector de tablones.
  type Finca = Vector[Tablon]
  //si f:Finca, f(i) = (ts_i, tr_i, p_i)

  //La distancia entre dos tablones se representa por
  //Una matriz
  type Distancia = Vector[Vector[Int]]

  //Una programación de riego es un vector que asocia cada tablon
  //icon su turno de riego ( 0 es el primer turno
  //n-1 es el último turno)
  type ProgRiego = Vector[Int]
  //si v: ProgRiego, y v.length == n, v es una permutación
  //de {0,...,n-1} v(i) es el turno de riego del tablon i
  //para 0 <= i < n

  //El tiempo de inicio de riego es un vector que asocia cada tablon i
  //con el momento del tiempo en que se riega.
  type TiempoInicioRiego = Vector[Int]
  //Si t: TiempoInicioRiego y t.length == n, t(i)
  //es la hora a la que inicia a regarse el tablon i.

  val random = new Random()

  def fincaAlAzar(long: Int): Finca = {
    //Crea una finca de long tablones
    //con valores aleatorios entre 1 y long*2 para
    //el tiempo de supervivencia entre y long para el tiempo
    //de regado y entre 1 y 4 para la prioridad
    val v = Vector.fill(long) {
      (random.nextInt(long * 2) + 1,
        random.nextInt(long) + 1,
        random.nextInt(4) + 1)

    }
    v
  }

  def distanciaAlAzar(long: Int): Distancia = {
    //Crea una matriz de distancias para una finca
    //de long tablones, con valores aleatorios entre
    //1 y long*3
    val v = Vector.fill(long, long) {
      random.nextInt(long * 3) + 1
    }
    Vector.tabulate(long, long)((i, j) => if (i < j) v(i)(j)
    else if (i == j) 0
    else v(j)(i))
  }

  //Exploran las entradas de cada tablon:
  def tsup(f: Finca, i: Int): Int = {
    f(i)._1
  }

  def treg(f: Finca, i: Int): Int = {
    f(i)._2
  }

  def prio(f: Finca, i: Int): Int = {
    f(i)._3
  }

  def tIR(f: Finca, pi: ProgRiego): TiempoInicioRiego = {
    //Dada una finca f y una programación de riego pi,
    //y f.length == n, tIR(f, pi) devuelve t: TiempoInicioRiego
    //tal que t(i) es el tiempo en que inicia el riego del
    //tablon i de la finca f segun pi
    val n = f.length // Número de tablones en la finca

    @tailrec
    def calcularTIR(tablones: List[(Tablon, Int)], tiempoActual: Int, result: Vector[Int]): Vector[Int] = tablones match {
      case Nil => result // Caso base: no quedan tablones por procesar, se devuelve el resultado
      case (tablon, index) :: tail =>
        val tablonIndex= index
        val tRegado = treg(f, tablonIndex) // Tiempo de riego del tablón actual
        val tInicio = tiempoActual // Tiempo de inicio de riego del tablón actual
        val nuevoResult = result.updated(index, tInicio) // Actualizar el vector de resultados con el tiempo de inicio de riego

        calcularTIR(tail, tiempoActual + tRegado, nuevoResult) // Llamada recursiva con el resto de tablones y el tiempo actualizado
    }

    val tablonesOrdenados = f.zipWithIndex.sortBy { case (_, i) => pi(i) } // Ordenar los tablones según la programación de riego
    val tiempoInicioRiego = Vector.fill(n)(0) // Inicializar el vector de tiempo de inicio de riego

    calcularTIR(tablonesOrdenados.toList, 0, tiempoInicioRiego) // Llamar a la función auxiliar con los tablones ordenados, tiempo inicial 0 y el vector de tiempo de inicio de riego
  }
  def costoRiegoTablon(i: Int, f: Finca, pi:ProgRiego): Int ={
    //devuelve el costo de regar el tablon i de la finca f
    //con la programación pi
    val tSupervivencia = tsup(f, i) // Tiempo de supervivencia del tablón i
    val tRiego = treg(f, i) // Tiempo de riego del tablón i
    val turnoRiego = pi(i) // Turno de riego asignado al tablón i en la programación

    val tablonesAnteriores = f.zipWithIndex.filter { case (_, index) => index < i } // Tablones anteriores al tablón i
    val tablonesMismoTurno = tablonesAnteriores.filter { case (_, index) => pi(index) == turnoRiego } // Tablones con el mismo turno de riego que el tablón i

    val tiempoInicioRiego = tIR(f, pi) // Calcular el tiempo de inicio de riego para todos los tablones según la programación

    val tablonesMismoTurnoTiempoInicio = tablonesMismoTurno.map { case (_, index) => (index, tiempoInicioRiego(index)) } // Tablones con el mismo turno de riego y su respectivo tiempo de inicio

    val costoRiego = tablonesMismoTurnoTiempoInicio.foldLeft(0) { case (costo, (tablonIndex, tiempoInicio)) =>
      val tRiegoOtroTablon = treg(f, tablonIndex) // Tiempo de riego del otro tablón con el mismo turno
      val tInicioOtroTablon = tiempoInicio // Tiempo de inicio de riego del otro tablón con el mismo turno

      val tiempoFinOtroTablon = tInicioOtroTablon + tRiegoOtroTablon // Tiempo de fin de riego del otro tablón

      if (tiempoFinOtroTablon > tiempoInicio) {
        val solapamiento = tiempoFinOtroTablon - tiempoInicio // Calcular el solapamiento de tiempos
        costo + solapamiento // Sumar el solapamiento al costo total
      } else {
        costo // No hay solapamiento, mantener el costo actual
      }
    }

    val costoTotal = costoRiego + tSupervivencia // Sumar el costo de riego al tiempo de supervivencia del tablón

    costoTotal

  }
  def costoRiegoFinca(f:Finca, pi: ProgRiego): Int = {
    //devuelve el costo total de regar una finca f dada
    //una programación de riego pi
    val tablonesIndices = f.indices // Índices de los tablones en la finca
    val costosTablones = tablonesIndices.map(i => costoRiegoTablon(i, f, pi)) // Calcular el costo de riego para cada tablón en la finca
    val costoTotal = costosTablones.sum // Sumar los costos individuales para obtener el costo total de regar la finca

    costoTotal

  }
  def costoMovilidad(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    val n = f.length // Número de tablones en la finca
    val tiempoInicioRiego = tIR(f, pi) // Calcular el tiempo de inicio de riego para todos los tablones según la programación

    val costosMovilidad = for {
      i <- 0 until n // Iterar sobre los tablones en la finca
      j <- 0 until n // Iterar sobre los tablones en la finca
      if i != j // Evitar calcular la distancia y el costo para el mismo tablón
    } yield {
      val tRiegoI = treg(f, i) // Tiempo de riego del tablón i
      val tInicioI = tiempoInicioRiego(i) // Tiempo de inicio de riego del tablón i
      val tRiegoJ = treg(f, j) // Tiempo de riego del tablón j
      val tInicioJ = tiempoInicioRiego(j) // Tiempo de inicio de riego del tablón j

      val distancia = d(i)(j) // Distancia entre los tablones i y j

      val tiempoFinI = tInicioI + tRiegoI // Tiempo de fin de riego del tablón i
      val tiempoInicioJ = tInicioJ // Tiempo de inicio de riego del tablón j

      val solapamiento = tiempoFinI - tiempoInicioJ // Calcular el solapamiento de tiempos entre los tablones i y j

      distancia * solapamiento // Calcular el costo de movilidad entre los tablones i y j
    }

    val costoTotal = costosMovilidad.sum // Sumar todos los costos de movilidad

    costoTotal

  }


def costoRiegoFincaPar(f: Finca, pi: ProgRiego): Int = {
  // Obtiene los índices de los tablones en la finca
  val tablones = f.indices

  // Realiza los cálculos de costoRiegoTablon en paralelo utilizando mapAsync
  val parallelCalculations = tablones.mapAsync(4)(i => Future(costoRiegoTablon(i, f, pi)))

  // Espera a que se completen todas las tareas paralelas y obtiene los resultados
  val resultFuture = Future.sequence(parallelCalculations)
  val result = Await.result(resultFuture, Duration.Inf)

  // Retorna la suma de los resultados individuales
  result.sum
}

def costoMovilidadPar(f: Finca, pi: ProgRiego, d: Distancia): Int = {
  // Obtiene los índices de los tablones en la finca
  val tablones = f.indices

  // Realiza los cálculos de costoMovilidadTablon en paralelo utilizando flatMap y mapAsync
  val parallelCalculations = tablones.flatMap { i =>
    tablones.filter(_ != i).mapAsync(4) { j =>
      Future(costoMovilidadTablon(i, j, f, pi, d))
    }
  }

  // Espera a que se completen todas las tareas paralelas y obtiene los resultados
  val resultFuture = Future.sequence(parallelCalculations)
  val result = Await.result(resultFuture, Duration.Inf)

  // Retorna la suma de los resultados individuales
  result.sum
}

  
  //generando programaciones de riego
  /*def generarProgramacionesRiego(f: Finca):Vector[ProgRiego] = {
    //Dada una finca de n tablones, devuelve todas las
    //posibles programaciones de riego de la finca.
    val n = f.length // Número de tablones en la finca

    def permute(nums: Vector[Int]): Vector[Vector[Int]] = {
      if (nums.length <= 1) {
        Vector(nums)
      } else {
        for {
          i <- nums.indices.toVector
          subPermutation <- permute(nums.take(i) ++ nums.drop(i + 1))
        } yield nums(i) +: subPermutation
      }
    }

    val tablonesIndices = Vector.tabulate(n)(i => i) // Índices de los tablones en la finca
    val programaciones = permute(tablonesIndices) // Generar todas las permutaciones de los índices de los tablones

    programaciones

  }*/



}
