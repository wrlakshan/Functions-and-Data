//19000792

object RationalNumbers {
  def main(args: Array[String]) = {
    var x = new Rational(3,4)
    var y = new Rational(5,8)
    var z = new Rational(2,7)
    var a = x-y-z
    println(a.toString)
  }
}

class Rational(x:Int, y:Int){
  require(y>0,"Denominator can't be zero")
  private def gcd(a:Int, b:Int):Int = if(b==0) a else gcd(b, a%b)
  val g=if(gcd(x,y)<0) 1 else gcd(x,y)

  val numer = x /g
  val denom = y/g

  def add(r: Rational): Rational ={
    new Rational(this.numer*r.denom + this.denom*r.numer, this.denom*r.denom)
  }
  def mul(r: Rational): Rational ={
    new Rational(numer*r.numer, denom*r.denom)
  }
  def dev(r: Rational): Rational ={
    new Rational(numer*r.denom, denom*r.numer)
  }
  def neg = new Rational(-this.numer, this.denom)
  override def toString: String = {
    numer + "/" + denom
  }
  def sub(r: Rational): Rational ={
    this + r.neg
  }
  def +(r:Rational):Rational ={
    this.add(r)
  }
  def -(r:Rational):Rational = {
    this.sub(r)
  }
}