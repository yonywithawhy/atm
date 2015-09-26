package some.site.ydl.atm

import scala.util._
// import io.StdIn._

object ATM {
    type Email = String
    // TODO: Email will have to change for validation
    case class User(name:String,age:Int,username:String,password:String,email:Email)
    case class Transaction(id:Long,description:String) // TODO: this will have to be real at some point
    case class Account(var balance:Int,owner:User,transactions:List[Transaction])
    val activeAccount = Account(owner = User("Alfred Hitchcock",99,"al_the_great","myAmazingPassword","al@fred.com"),transactions = List(Transaction(1,"Did Something"),Transaction(2,"Did something else")),balance = 100073)

    def displayHello():Unit = {
        println("Hello $user! Welcome to the ATM.")
    }
    def promptUserAndGetSelection():Try[Int] = {
        println("""
At the ATM we have the following options:
0 - Get fast cash ($20)
1 - Make a deposit
2 - Make a withdrawal
3 - See my balance
4 - See recent transcations on my account
What would you like to do? """)
        Try(readInt())
    }  
    def validateChoice(t:Try[Int]):Unit = {
        t match {
            case Success(i) => i match {
                case 0 => performFastCash()
                case 1 => promptDeposit()
                case 2 => promptWithdrawal()
                case 3 => promptBalanceInquiry()
                case 4 => promptTransactionInquiry()
                case _ => validateChoice(promptUserAndGetSelection())
            }
            case Failure(e) => println(s"Unfortunately I was unable to figure out what your choice was. Exiting now with error :: " + e.getMessage())
        }
    }
    def performFastCash():Unit = {
        attemptWithdrawal(getSmartFastCashAccount(),2000) match {
            case Right(b) => relinquishCash(2000); println(s"Thank you, your account balance is $b")
            case Left(b) => println(s"Your account came up short so I was unable to perform a fash cash withdrawal. Your balance is $b")
        }
    }
    def getSmartFastCashAccount():Account = activeAccount
    def relinquishCash(amount:Int):Unit = println(s"Printing cash amount of ${amount/100.0}")
    def promptDeposit():Unit = println("Want some deposit?")
    def promptWithdrawal():Unit = {
        println("How much would you like to withdraw?")
        val tempLine = readLine()
        val requestedWithdrawal = Try(tempLine.toDouble)
        requestedWithdrawal match {
            case Success(d:Double) => attemptWithdrawal(activeAccount,(d*100).toInt)
            case Failure(e) => println("You can only put in a number, silly!"); promptWithdrawal()
        }
    }
    def attemptWithdrawal(anAccount:Account,amountInCents:Int):Either[Int,Int] = { //TODO: What if the account has an extremely large amount in it?
        val withdrawalAttempt = anAccount.balance - amountInCents

        if (withdrawalAttempt < 0) {
            Left(anAccount.balance) 
        } else {
            anAccount.balance = withdrawalAttempt // TODO: change this to safe getter method
            println("Withdrawal successful.")
            Right(withdrawalAttempt)
        }
    }
    def promptBalanceInquiry():Unit = println("Your balance is " + activeAccount.balance)
    def promptTransactionInquiry():Unit = {
        println("Your transactions are as follows:")
        activeAccount.transactions.foreach(t => println(s"${t.id}\t${t.description}"))
    }
    def displayGoodbye():Unit = {
        println("Thank you for your business. Have a nice day!")
    }
    def main(args:Array[String]) = {
        val FAST_CASH_IN_CENTS = 2000
        displayHello()
        validateChoice(promptUserAndGetSelection())
        displayGoodbye()
    }
}
