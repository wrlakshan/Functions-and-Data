//19000792

object Bank {
  val j:Account = new Account("1", 4070, -2000)
  val k:Account = new Account("2", 4071, -1000)
  val l:Account = new Account("3", 4072, -9000)
  val m:Account = new Account("4", 4073, -200)
  val n:Account = new Account("5", 4074, -2300)
  var bank:List[Account] = List(j,k,l,m,n)

  val find=(n:String,b:List[Account]) =>
    b.filter(x=>x.nic.equals(n))

  val overdraft=(b:List[Account]) =>
    b.filter(x=>x.balance<0)

  val interest=(b:List[Account]) =>
    b.map(x=>if(x.balance>=0) x.balance*1.05 else x.balance*1.1)

  def balance(b:List[Double]): Double = {
    b.reduce((x,y) => x+y)
  }


  def main(args: Array[String]) = {
    println("Overdrafted accounts : " + overdraft(bank))
    println("Sum of all account balances: " + balance(interest(bank)))
  }
}


class Account(id:String, n:Int, b:Double){
  val nic:String = id
  val acnumber:Int = n
  var balance:Double = b
  
  override def toString =   "["+nic+":"+acnumber +":"+ balance+"]"

  def withdraw(amount:Double): Unit ={
    this.balance -= amount
  }
  def deposit(amount:Double): Unit ={
    this.balance += amount
  }
  def transfer(acc:Account, amount:Double): Unit ={
    this.withdraw(amount)
    acc.deposit(amount)
  }
}
