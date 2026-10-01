package Multiplicacion

/*
 * PeasantAlgorithm:
 * Multiplica dos enteros positivos utilizando el algoritmo
 * campesino de multiplicación mediante recursión lineal.
 *
 * Si a es par, se divide a entre 2 y se duplica b.
 * Si a es impar, además se suma b al resultado.
 */

def PeasantAlgorithm(a: Int, b: Int): Int = {
  if (a == 0) {
    0
  } else if (a % 2 == 0) {
    PeasantAlgorithm(a / 2, b + b)
  } else {
    PeasantAlgorithm(a / 2, b + b) + b
  }
}

def PeasantAlgorithmIt(x: Int, y: Int): Int = {
  var a = x
  var b = y
  var resultado = 0 // Acumula los valores de b cuando a es impar

  while (a != 0) {
    if (a % 2 == 0) {
      a = a / 2
      b = b + b
    } else {
      resultado = resultado + b
      a = a / 2
      b = b + b
    }
  }

  resultado
}
/*
 * splitMultiply:
 * Multiplica dos enteros utilizando el Split Algorithm.
 *
 * Cada número se divide en dos partes:
 *
 * a = 10^m * x + y
 * b = 10^m * z + w
 *
 * Se realizan cuatro multiplicaciones recursivas:
 * x*z, y*z, x*w y y*w.
 */

def splitMultiply(a: Int, b: Int): Int = {
  if (a.abs < 10 && b.abs < 10) {
    a * b
  } else {
    val n = math.max(a.abs.toString.length, b.abs.toString.length)
    val m = n / 2
    val potencia = math.pow(10, m).toInt

    val x = a / potencia
    val y = a % potencia
    val z = b / potencia
    val w = b % potencia

    val xz = splitMultiply(x, z)
    val yz = splitMultiply(y, z)
    val xw = splitMultiply(x, w)
    val yw = splitMultiply(y, w)

    xz * potencia * potencia + (yz + xw) * potencia + yw
  }
}
/*
 * fastMultiply:
 * Multiplica dos enteros utilizando el Fast Algorithm
 * o algoritmo de Karatsuba.
 *
 * Se utilizan tres multiplicaciones recursivas en lugar
 * de las cuatro utilizadas por splitMultiply.
 *
 * La identidad utilizada es:
 *
 * y*z + x*w = x*z + y*w - (x-y)*(z-w)
 */

def fastMultiply(a: Int, b: Int): Int = {
  if (a.abs < 10 && b.abs < 10) {
    a * b
  } else {
    val n = math.max(a.abs.toString.length, b.abs.toString.length)
    val m = n / 2
    val potencia = math.pow(10, m).toInt

    val x = a / potencia
    val y = a % potencia
    val z = b / potencia
    val w = b % potencia

    val xz = fastMultiply(x, z)
    val yw = fastMultiply(y, w)
    val diferencia = fastMultiply(x - y, z - w)

    val medio = xz + yw - diferencia

    xz * potencia * potencia + medio * potencia + yw
  }
}