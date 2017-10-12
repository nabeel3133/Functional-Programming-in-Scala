object Tasks {
  def main(args: Array[String]): Unit = {
    val randomList = List.fill(10)(100).map(scala.util.Random.nextInt)
    println("Random List is: ");
    print(randomList);
    println()
    val n_value = 3
    println("The "+n_value+" from the last is without using TailRecursive: "+lastNth(n_value,randomList))
    println("The "+n_value+" from the last is when using TailRecursive: "+lastNthRecursive(n_value,randomList.length, randomList))

    val check = 8
    if((isPrime(check,2)==true))
      println(check + " is a prime number")
    else
      println(check + " is not a prime number")

    val firstValue = 24
    val secondValue = 33
    if(firstValue < secondValue)
      {
        if((areCoPrimes(firstValue,secondValue,firstValue)==true))  //Last argument is the minimum of the two
          println(firstValue + " " + secondValue + " are co-primes")
        else
          println(firstValue + " " + secondValue + " are not co-primes")
      }
    else if(firstValue > secondValue)
      {
        if((areCoPrimes(firstValue,secondValue,secondValue)==true))  //Last argument is the minimum of the two
        println(firstValue + " " + secondValue + " are co-primes")
        else
        println(firstValue + " " + secondValue + " are not co-primes")
      }
    else
      println("Both are not co-primes")

    val firstBoolean = false
    val secondBoolean = false
    if(XOR_CHECK(firstBoolean,secondBoolean)== false)
      println("XOR of "+ firstBoolean + " " + secondBoolean + " is false" )
    else
      println("XOR of "+ firstBoolean + " " + secondBoolean + " is true" )

    if(AND_CHECK(firstBoolean,secondBoolean)== false)
      println("AND of "+ firstBoolean + " " + secondBoolean + " is false" )
    else
      println("AND of "+ firstBoolean + " " + secondBoolean + " is true" )

    if(NAND_CHECK(firstBoolean,secondBoolean)== false)
      println("NAND of "+ firstBoolean + " " + secondBoolean + " is false" )
    else
      println("NAND of "+ firstBoolean + " " + secondBoolean + " is true" )

  }
  def lastNth(number:Int, theList:List[Int]): Int  =
  {
    if(number < theList.length)
      {
        val number_to_be_found= theList.length - number
        theList(number_to_be_found);
      }
    else
      throw new IndexOutOfBoundsException
  }

  def lastNthRecursive(number:Int,last:Int,theList:List[Int]): Int  =
  {
    if(last == (theList.length - number))
      {
        theList(last);
      }
    else
      lastNthRecursive(number, last-1, theList)
  }

  def isPrime(number : Int, start:Int): Boolean =
  {
      if (number<1)
        return false
      if (start!=number && start>1)
      {
        if (number % start==0)
        return false
      }
      if (start==1)
      return true
      isPrime(number,start-1)
  }


  def areCoPrimes(firstNumber: Int, secondNumber : Int, divisor:Int): Boolean =
  {
    if(divisor==1)
      true
    else if(firstNumber%divisor==0&&secondNumber%divisor==0)
      false
    else
      return areCoPrimes(firstNumber, secondNumber, divisor-1)
  }

  def XOR_CHECK(first:Boolean, second:Boolean): Boolean =
  {
     if(first==false && second==false)
       false
     else if(first==true||second==true)
       true
     else (first==true && second==true)
       false
  }

  def AND_CHECK(first:Boolean, second:Boolean): Boolean =
  {
    if(first==false||second==false)
      false
    else
      true
  }

  def NAND_CHECK(first:Boolean, second:Boolean): Boolean =
  {
    if(first==true && second==true)
      false
    else
      true
  }

}
