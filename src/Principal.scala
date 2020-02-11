import scala.io.StdIn._
import scala.math._

  /*
  
    Autor:
      Castillo Landeros Miguel Eduardo.
    Fecha y hora:
      11/02/2020 13:17 Hrs.

   */

object Principal {
  def main(args: Array[String]): Unit = {

    print("\n\t\t\t\tACTIVIDAD 4\n\tCALCULO DE MEDIA y DESVIACION ESTANDAR\n¿Cuantos datos van a insertar? R: ")
    var cantidadDatos = readInt()
    printf("\tINCERTE LOS %d DATOS\n", cantidadDatos)

    var datosF = lecturaDeDatos(cantidadDatos)

    impresion(datosF)

  }

  def lecturaDeDatos( datos: Int ): Array[Double] = {

    var datosIncertados = new Array[Double](datos)
    for (actual <- 0 to (datos-1)){
      printf("Dato #%d: ", (actual+1))
      datosIncertados(actual) = readDouble()
    }
    return datosIncertados

  }

  def impresion( datosF: Array[Double]): Unit = {
    printf("\n\tRESULTADOS:\n")
    var mediaP = calculoMedia(datosF)
    println( "MEDIA: "+ mediaP )
    println( "DESVIACION ESTANDAR: "+ calculoDesviacion(datosF, mediaP) )
  }

  def calculoMedia(datos: Array[Double]): Double = {

    var media: Double = 0.0
    for (elemento <- datos){
      media = media + elemento
    }
    media = (media / datos.length)

    return media
  }

  def calculoDesviacion(datos: Array[Double], media: Double): Double = {

    var desviacion: Double = 0.0
    for (elemento <- datos){
      desviacion = desviacion + pow( abs(elemento - media) ,2)
    }
    desviacion = desviacion / datos.length
    desviacion = sqrt(desviacion)

    return desviacion
  }

}
