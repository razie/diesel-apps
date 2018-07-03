package testing


class Game(val board:Array[Array[Int]]) {

  def fill(g:Game, x:Int, y:Int) {
    var numbs=x*y

    for(i <- 0 to 2)
      for(cur <- 1 to numbs/2) {
        g.board(i)(cur) = 0
      }
  }
}

object memGame extends App {

  def create(x:Int, y:Int) = {
    val board = new Array[Array[Int]](x)
    for (i <- 0 until board.length) {
      var a = new Array[Int](y)
      board(i) = a
    }
    new Game(board)
  }


}

abstract class Shape(name:String) {
  def draw : Unit
}

trait Ofsetable {
  def offset (x:Int) : Unit
}

class Circle extends Shape("circle") with Ofsetable {
  override def draw : Unit = {}
  def offset (x:Int) : Unit = {}
}

abstract class Poly (name:String) extends Shape (name) {
  def edges : Int
}

class Square extends Poly(name="square") {
  override def draw : Unit = {}
  override def edges : Int = 4
}

object XApp extends App {
  def drawShapes (l:Array[Shape]) = {
    for (s <- l)
      s.draw
  }

  val c:Shape = new Circle
  val a:Array[Shape] = Array(new Circle, new Square)
  drawShapes(a)
  println(a.mkString)
}


