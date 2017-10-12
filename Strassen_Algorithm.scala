object Strassen_Algorithm {

  def main(args: Array[String]): Unit =
  {
    print("\nMatrix 1 rows: ");
    val rows =  scala.io.StdIn.readInt();
    print("\nMatrix 1 Columns: ");
    val cols = scala.io.StdIn.readInt();
    if(!IsPowerOf2(cols) || !IsPowerOf2(rows))
    {
      print("Matrix dimension should be in power of 2")
      return
    }
    val matrix1 = Array.ofDim[Int](rows,cols)
      for (i<-0 to rows-1)
    {
      for (j<-0 to cols-1)
      {
        print("\nEnter value for  element "+(i+1)+" x "+(j+1)+": ")
        matrix1(i)(j) = scala.io.StdIn.readInt();
      }
    }
    print("\nMatrix 2 rows: ")
    val rows2 =  scala.io.StdIn.readInt();
    print("\nMatrix 2 Columns: ")
    val cols2 = scala.io.StdIn.readInt();
    val matrix2 = Array.ofDim[Int](rows2,cols2)
    if(!IsPowerOf2(cols2) || !IsPowerOf2(rows2))
    {
      print("Matrix should be in power of 2 dimension")
      return
    }
      for (i<-0 to rows2-1)
    {
      for (j<-0 to cols2-1)
      {
        print("\nEnter value for  element "+(i+1)+" x "+(j+1)+": ")
        matrix2(i)(j) = scala.io.StdIn.readInt();
      }
      
    }

    if (rows==2)
    {
      if ((rows!=rows2)||((rows!=cols)||(rows2!=cols2)))
      {
        print("Rows and columns should be same")
      }
      else
      {
        val result = StrassenMultiply(matrix1, matrix2 ,rows,cols)
        printmatrix(result,rows)
      }
    }
    else
    {
      if ((rows!=cols || rows2!=cols2)||(rows!=rows2)||(rows%2!=0))
      {
        print("Rows and columns should be same")
      }
      else
      {
        val result = StrassenMultiply(matrix1, matrix2, rows,cols)
        printmatrix(result,rows)
      }
    }
  }


  def Add(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]], rows:Int, columns:Int): Array[Array[Int]] =
  {
    val AddAnswer = Array.ofDim[Int](rows,columns)
    for(i<-0 to rows-1)
    {
      for(j<-0 to columns-1)
        AddAnswer(i)(j) = matrix1(i)(j) + matrix2(i)(j)
    }
    return AddAnswer
  }

  def Subtract(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]], rows:Int, columns:Int): Array[Array[Int]] =
  {
    val SubtractAnswer = Array.ofDim[Int](rows,columns)
    for(i<-0 to rows-1)
    {
      for(j<-0 to columns-1)
        SubtractAnswer(i)(j) = matrix1(i)(j) - matrix2(i)(j)
    }
    return SubtractAnswer
  }

  def StrassenMultiply(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]], rows:Int, columns:Int): Array[Array[Int]] =
  {
        val resultantMatrix = Array.ofDim[Int](rows,columns)
          if(rows == 2 && columns == 2)
          {
            val P1 = matrix1(0)(0) * (matrix2(0)(1) - matrix2(1)(1))
            val P2 = matrix2(1)(1) * (matrix1(0)(0) + matrix1(0)(1))
            val P3 = matrix2(0)(0) * (matrix1(1)(0) + matrix1(1)(1))
            val P4 = matrix1(1)(1) * (matrix2(1)(0) - matrix2(0)(0))
            val P5 = (matrix1(0)(0) + matrix1(1)(1)) * (matrix2(0)(0) + matrix2(1)(1))
            val P6 = (matrix1(0)(1) - matrix1(1)(1)) * (matrix2(1)(0) + matrix2(1)(1))
            val P7 = (matrix1(0)(0) - matrix1(1)(0)) * (matrix2(0)(0) + matrix2(1)(1))
            resultantMatrix(0)(0) = P5 + P4 - P2 + P6
            resultantMatrix(0)(1) = P1 + P2
            resultantMatrix(1)(0) = P3 + P4
            resultantMatrix(1)(1) = P1 + P5 - P3 - P7
          }

         else
         {
          val half = rows/2
          //Calculating sub matrices for the first matrix
          val subMatrix1 = subMatrix(matrix1, (rows/2)-1, (columns/2)-1, 0, 0, rows, columns)
          val subMatrix2 = subMatrix(matrix1, (rows/2)-1, columns-1, 0, columns/2, rows, columns)
          val subMatrix3 = subMatrix(matrix1, rows-1, (columns/2)-1, rows/2, 0, rows, columns)
          val subMatrix4 = subMatrix(matrix1,  rows-1, columns-1, rows/2, columns/2, rows, columns)


          //Calculating sub matrices for the second matrix
          val subMatrix5 = subMatrix(matrix2, (rows/2)-1, (columns/2)-1, 0, 0, rows, columns)
          val subMatrix6 = subMatrix(matrix2, (rows/2)-1, columns-1, 0, columns/2, rows, columns)
          val subMatrix7 = subMatrix(matrix2, rows-1, (columns/2)-1, rows/2, 0, rows, columns)
          val subMatrix8 = subMatrix(matrix2,  rows-1, columns-1, rows/2, columns/2, rows, columns)

          //Calculating P1 to P7 for these submatrices
          val P1 = StrassenMultiply(subMatrix1,Subtract(subMatrix6,subMatrix8,half, half), rows/2, columns/2)
          val P2 = StrassenMultiply(Add(subMatrix1,subMatrix2,half,half),subMatrix8, rows/2, columns/2)
          val P3 = StrassenMultiply(Add(subMatrix3,subMatrix4,half,half),subMatrix5, rows/2, columns/2)
          val P4 = StrassenMultiply(subMatrix4,Subtract(subMatrix7,subMatrix5,half,half), rows/2, columns/2)
          val P5 = StrassenMultiply(Add(subMatrix1,subMatrix4,half,half),Add(subMatrix5,subMatrix8,half,half), rows/2, columns/2)
          val P6 = StrassenMultiply(Subtract(subMatrix2,subMatrix4,half,half),Add(subMatrix7,subMatrix8,half,half), rows/2, columns/2)
          val P7 = StrassenMultiply(Subtract(subMatrix1,subMatrix3,half,half),Add(subMatrix5,subMatrix6,half,half), rows/2, columns/2)

            val FirstAnswer = Add(Add(P6,P5, half,half),Subtract(P4,P2,half,half),half,half)
            val SecondAnswer = Add(P1,P2,half,half)
            val ThirdAnswer = Add(P3,P4, half,half)
            val FourthAnswer = Subtract(Subtract(Add(P1,P5,half,half),P3,half,half),P7,half,half)
            for(i<-0 to half-1)
            {
              for (j<-0 to half-1)
              {
                resultantMatrix(i)(j) = FirstAnswer(i)(j)
              }
            }
            for(i<-0 to half-1)
            {
              for (j<-half to rows-1)
              {
                resultantMatrix(i)(j) = SecondAnswer(i)(j-half)
              }
            }
            for(i<-half to rows-1)
            {
              for (j<-0 to half-1)
              {
                resultantMatrix(i)(j) = ThirdAnswer(i-half)(j)
              }
            }
            for(i<-half to rows-1)
            {
              for (j<-half to columns-1)
              {
                resultantMatrix(i)(j) = FourthAnswer(i-half)(j-half)
              }
            }
          }
        return resultantMatrix
  }

  def subMatrix(matrix: Array[Array[Int]], rows:Int, columns:Int, row_start:Int, column_start:Int, theRows:Int, theColumns:Int): Array[Array[Int]] = {
    val theSubMatrix = Array.ofDim[Int](theRows/2,theColumns/2)
    for(i<-row_start to rows) {
      for (j <- column_start to columns) {
        theSubMatrix(i-row_start)(j-column_start) = matrix(i)(j)
      }
    }
    return theSubMatrix
  }
  def printmatrix(theMatrix:Array[Array[Int]],end:Int){
    for (i<-0 to (end-1))
    {
      for(j<-0 to (end-1))
      {
        print ((theMatrix(i)(j))+" ")
      }
      print("\n");
    }
  }

  def IsPowerOf2(number:Int):Boolean={
    if (number == 0 || number==1)
      return false
    if(number==2)
      return true
    if(number<10)
      {
        if((number/2)%2 == 0)
          return true
        else
          return false
      }
    else
    {
      IsPowerOf2(number-10)
    }
  }
}
