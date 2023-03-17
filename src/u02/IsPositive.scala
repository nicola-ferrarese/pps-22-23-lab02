package u02

object IsPositive extends App:

  // 3a I
  val positive2: Int => String = n =>
    n match
      case n if n > 0 => "positive"
      case n if n < 0 => "negative"
      case _ => "zero"

  // 3a II
  def positive(n: Int): String = n match
    case n if n > 0 => "positive"
    case n if n < 0 => "negative"
    case _ => "zero"
  println("Testing Positive func:")
  println("\tpositive(3): " +positive(3))
  println("\tpositive(-3): " +positive(-3))
  println("\tpositive(0): " +positive(0))


  val empty: String => Boolean = _ == "" // predicate on strings

  // 3b
  def neg(inputFunc: String => Boolean): String => Boolean =
    x => !inputFunc(x)

  // 3c
  def neg2[A](f: A => Boolean): A => Boolean = x => !f(x)  // or !f(_)

  val notEmpty = neg(empty)
  println("Testing neg func:")
  println("\tnotEmpty(''): " + notEmpty(""))
  val notEmpty2 = neg2(empty)
  println("\tnotEmpty2(''): " + notEmpty2(""))

  // 4 - currying
  println("Testing currying func: ")
  val curriedFuncVal: Int => Int => Int => Boolean =
    x => y => z => x <= y && y == z

  println("\tcurriedFuncVal(1)(2)(3): " + curriedFuncVal(1)(2)(3))

  val nonCurriedFuncVal: (Int, Int, Int) => Boolean =
    (x, y, z) => x <= y && y == z
  println("\tnonCurriedFuncVal(1,2,2): "+ nonCurriedFuncVal(1, 2, 2))
  def curriedFunc(x: Int)(y: Int)(z: Int): Boolean =
    x <= y && y == z
  println("\tcurriedFunc(1)(2)(2): " + curriedFunc(1)(2)(2))

  def nonCurriedFunc(x: Int, y: Int, z: Int): Boolean =
    x <= y && y == z
  println("\tnonCurriedFunc(1,2,3): "+ nonCurriedFunc(1, 2, 3))


  // 5 - Functional Composition
  println("Testing compose func:")
  def compose(f: Int => Int, g: Int => Int): Int => Int =
    x => f(g(x))
  println("\tcompose(_ - 1, _ * 2)(3): "+ compose(_ - 1, _ * 2)(3)) // 5

  // Generic version of compose
  def compose2[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))

  val x: Int => Int = _ - 1
  val y: Int => Int = _ * 2
  println("\tcompose2(_ - 1, _ * 2)(3): "+ compose2(x, y)(3)) // 5

  // 6 - recursion
  println("Testing recursion func:")
  @annotation.tailrec
  private def gcd(a: Int, b: Int): Int = b match
    case 0 => a
    case _ => gcd(b, a % b)

  println("\tgcd(14, 21): "+gcd(14, 21)) // 7

  // 7-
  enum Shape:
    case Circle(radius: Double)
    case Rectangle(width: Double, height: Double)
    case Square(side: Double)

  object Shape:
    def perimeter(shape: Shape): Double = shape match
      case Circle(radius) => 2 * math.Pi * radius
      case Rectangle(width, height) => 2 * (width + height)
      case Square(side) => 4 * side

    def contains(shape: Shape, point: (Double, Double)): Boolean = shape match
      case Circle(radius) =>
        val (x, y) = point
        x * x + y * y <= radius * radius
      case Rectangle(width, height) =>
        val (x, y) = point
        x >= 0 && x <= width && y >= 0 && y <= height
      case Square(side) =>
        val (x, y) = point
        x >= 0 && x <= side && y >= 0 && y <= side

  import Shape.*

  val r = Rectangle(3, 4)
  println("Rectangle:")
  println("\tPerimeter:\n\tExpected: 14.0 - Actual: "+perimeter(r)) // 14.0
  println("\tContains:\n\tExpected: true - Actual: "+contains(r, (1, 1))) // true
  println("\tExpected: false - Actual: "+contains(r, (-1, -1))) // false

  val s = Square(2)
  println("Square:")
  println("\tPerimeter:\n\tExpected: 8.0 - Actual: "+perimeter(s)) // 8.0
  println("\tContains:\n\tExpected: true - Actual: "+contains(r, (1, 1))) // true
  println("\tExpected: false - Actual: "+contains(r, (-1, -1))) // false

  val c = Circle(3.14)
  println("Circle:")
  println("\tPerimeter:\n\tExpected: 19.73 - Actual: "+perimeter(c)) // 19.73
  println("\tContains:\n\tExpected: true - Actual: "+contains(c, (1, 1))) // true
  println("\tExpected: true - Actual: "+contains(c, (-1, -1))) // false

  // 8 - Functional combinators
  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity

  object Option:
    def isEmpty[A](opt: Option[A]): Boolean = opt match
      case None() => true
      case _ =>  false

    def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match
      case Some(a) => a
      case _ => orElse

    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match
      case Some(a) => f(a)
      case _ => None()

    def filter[A](opt: Option[A])(f: A => Boolean): Option[A] = opt match
      case Some(a) => opt
      case _ => None()

    def map[A, B](opt: Option[A])(f: A => B): Option[B] = opt match
      case Some(a) => Some(f(a))
      case _ => None()

    def fold[A,B](opt: Option[A])(z: B)(f: (A) => B): B = opt match
      case Some(a) => f(a)
      case _ => z

  import Option.*

  println("Testing Option func:")
  println("\tfilter(Some(5))(_ > 2): "+filter(Some(5))(_ > 2)) // Some(5)
  println("\tmap(Some(5))(_ > 2): "+map(Some(5))(_ > 2)) // Some(true)
  println("\tmap(Some(5))(_ > 8): "+map(Some(5))(_ > 8)) // Some(true)
  println("\tmap(None[Int]())(_ > 2): "+map(None[Int]())(_ > 2)) // Some(true)
  println("\tfold(Some(5))(1)( _  + 1): "+fold(Some(5))(1)( _  + 1))
  println("\tfold(None[Int]())(1)(_ + 1): "+fold(None[Int]())(1)(_ + 1))