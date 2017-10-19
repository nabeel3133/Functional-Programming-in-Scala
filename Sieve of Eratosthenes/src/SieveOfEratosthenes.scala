object SieveOfEratosthenes extends App {

  def returnList(number:Int): List[Int] =
  {
    2 to number toList
  }

  def removeMultiples(theNumber: Int, theList: List[Int]): List[Int] =
  {
        theList.filter(a=> !(a%theNumber==0 && a!=theNumber))
  }


  def eratosthenesAlgo(thePrime:Int, theList:List[Int], accu:Int, size:Int): List[Int] ={
    val multiplesRemoved = removeMultiples(thePrime, theList)
    val nextPrime = multiplesRemoved(accu)
    if(theList.isEmpty)
      List()
    else if(thePrime*thePrime>size)
      {
      theList
      }
    else
    {
      eratosthenesAlgo(nextPrime, multiplesRemoved, accu+1, size)
    }
  }

  println("Enter the number upto which you want prime numbers(Calcuation using Eratosthenes): ");
  val size = scala.io.StdIn.readInt()
  val accu = 0
  val theList = returnList(size)
  println(eratosthenesAlgo(2,theList, accu, size))

}
