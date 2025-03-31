//Michał Klatkowski

//zadanie 1

class MyPair[A,B](var fst: A, var snd: B):
  override def toString: String = s"($fst, $snd)"

val myPair = MyPair(1.9, "Test")
myPair.fst == 1.9
myPair.snd == "Test"
myPair.toString == "(1.9, Test)"
myPair.fst = 7.8
myPair.snd = "Second"
myPair.toString == "(7.8, Second)"

//zadanie 2

class BankAccount(initialBalance : Double):
  private var balance = initialBalance
  def checkBalance = balance
  def deposit(amount : Double) = { balance += amount; balance}
  def withdraw(amount : Double) = { balance -= amount; balance}
  override def toString = "%.2f".format(balance)

class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance):
  override def deposit(amount: Double): Double = super.deposit(amount - 1)
  override def withdraw(amount: Double): Double = super.withdraw(amount + 1)

val ca = CheckingAccount(100)
ca.deposit(10) == 109
ca.withdraw(8) == 100

class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance):
  private var transactionsCounter = 0
  def earnMonthlyInterest(): Unit =
    transactionsCounter = 0
    super.deposit(checkBalance * 0.01)
  override def deposit(amount: Double): Double =
    transactionsCounter +=1
    super.deposit(if transactionsCounter <= 3 then amount else amount -1)
  override def withdraw(amount: Double): Double =
    transactionsCounter +=1
    super.withdraw(if transactionsCounter <= 3 then amount else amount + 1)

val sa = SavingsAccount(100)
sa.earnMonthlyInterest()
sa.checkBalance == 101
sa.deposit(9) == 110
sa.withdraw(10) == 100
sa.deposit(11) == 111
sa.withdraw(1) == 109
sa.deposit(92) == 200
sa.earnMonthlyInterest()
sa.checkBalance == 202
sa.withdraw(302) == -100
sa.earnMonthlyInterest()
sa.checkBalance == -101


//zadanie 3

abstract class Zwierz(val imie: String = "bez imienia"):
  def rodzaj: String
  def dajGlos: String
  override def toString: String = s"$rodzaj $imie daje głos $dajGlos!"

class Pies(override val imie: String = "bez imienia") extends Zwierz(imie):
  override def rodzaj: String = "Pies"
  override def dajGlos: String = "Hau, hau"

class Kot(override val imie: String = "bez imienia") extends Zwierz(imie):
  override def rodzaj: String = "Kot"
  override def dajGlos: String = "Miał"

val pies1 = Pies()
val pies2 = Pies("Burek")
val kot1 = Kot()
val kot2 = Kot("Mruczek")

object TestZwierza:
  def main(args: Array[String]): Unit =
    val zwierzeta = Vector[Zwierz](Pies(), Pies("Burek"), Kot(), Kot("Mruczek"))
    for zwierz <- zwierzeta do println(zwierz)

TestZwierza.main(Array())