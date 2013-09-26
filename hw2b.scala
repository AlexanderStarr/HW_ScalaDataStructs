//==================================================
// 22c22 Fall 2013
//
// Homework 2, Part B
//
// Name: Alexander Starr
//
//==================================================


//================================================
// Vec
//================================================

class Vec private (private val elems:Array[Double]) {
  val length = elems.length
  
  /* Initializes a vector with a single value x. */
  def this(x:Double) = this(Array(x))

  /* Returns the double at index i. */
  def apply(i:Int) = elems(i)
  
  /* Concatenates two vectors. */
  def ++(other:Vec) = {
    val r_length = length + other.length
    val r_elems = new Array[Double](r_length)
    for (i <- 0 to length - 1) r_elems(i) = elems(i)
    for (i <- 0 to other.length - 1) r_elems(length + i) = other.elems(i)
    new Vec(r_elems)
  }

  /* Adds each element of one vector to the corresponding one in the other. */
  def +(other:Vec) = {
    val r_elems = new Array[Double](length)
    for (i <- 0 to length - 1) r_elems(i) = elems(i) + other.elems(i)
    new Vec(r_elems)
  }
  
  /* Sums the multiplication of each corresponding element in two Vecs. */
  def *(other:Vec) = {
    var p = 0.0
    for (i <- 0 to length - 1) 
      p = p + (elems(i) * other.elems(i))
    p
  }
  
  override def toString = {
    var s = "<" + this.apply(0)
    for (j <- 1 to length - 1)
      s = s + ", " + this.apply(j)
    s + ">"
  }

  // overrides the default definition of == (an alias of equals)
  override def equals(other:Any):Boolean = other match {
    // if other is an instance of Vec then ...
    case o:Vec =>
      for (j <- 0 to length - 1)
        if (this.apply(j) != o.apply(j)) return false
      true
    // otherwise return false
    case _ => false
  }
}


//================================================
// Matrix
//================================================

// Suggestion: for testing purposes it is convenient
//             to implement toString first


// (The main constructor of) Matrix takes as input
// an array of n > 0 vectors all of the same length 
class Matrix private (a:Array[Vec]) {

  // Public constructor
  def this(v:Vec) = this(Array(v))
  def this(x:Double) = this(Array[Vec](new Vec(x)))
 
  // the array is stored as is in the private field rows
  // the element at position (i,j) in the matrix is stored
  // as the j-th element of the i-th vector in rows
  private val rows = a

	// integer field h stores the height of the matrix
	val h = rows.length

  // integer field w stores the width of the matrix
  val w = rows(0).length
	
 /* given i in [0 .. h-1] and j in [1 .. w-1],
    apply(i,j) returns the matrix element at position (i,j)
 */
 def apply(i:Int, j:Int) = a(i)(j)
  
	/* given a matrix other with other.w == w,
	   /(other) returns a new matrix m such that
	   - m.h == h + other.h
	   - m.w == w
	   - for all i in [0 .. h-1] and j in [0 .. w-1],  m(i,j) == this(i,j)
	   - for all i in [h .. r.h-1] and j in [0 .. w-1],  m(i,j) == other(i,j)
	*/
	def /(other:Matrix) = new Matrix(rows ++ other.rows)
	
	/* given a matrix other with other.h == h,
	   ++(other) returns a new matrix m such that
	   - m.h == h 
	   - m.w == w + other.w
	   - for all i in [0 .. h-1] and j in [0 .. w-1],  m(i,j) == this(i,j)
	   - for all i in [0 .. h-1] and j in [w .. r.w-1],  m(i,j) == other(i,j)
	*/
	def ++(other:Matrix) = {
    val newA = rows
    for(i <- 0 to h-1) {
      newA(i) = newA(i) ++ other.rows(i)
    }
    new Matrix(newA)
  }

	/* given a matrix other with other.h == h and other.w == w,
	   +(other) returns a new matrix m such that
	   - m.h == h 
	   - m.w == w 
	   - for all i in [0 .. h-1] and j in [0 .. w-1],
	     m(i,j) == this(i,j) + other(i,j)
	*/
	def +(other:Matrix) = {
    val newA = rows
    for(i <- 0 to h-1) {
      newA(i) = newA(i) + other.rows(i)
    }
    new Matrix(newA)
  }

 /* given a j in [0 .. w-1],
    col(j) returns a vector v whose elements come 
    from the j-th column of the matrix, i.e.,
    - v.length == h
    - for all i in [0 .. h-1] ,  v(i) == this(i,j)  
 */
  private def col (j:Int) = {
   var c = new Vec(rows(0)(j))
   for (i <- 1 to h - 1)
     c = c ++ new Vec(rows(i)(j))
   c
 }

	/* transpose returns a new matrix m such that
	   - m.h == w 
	   - m.w == h
	   - for all i in [0 .. m.h-1] and j in [0 .. m.w-1],  m(i,j) == this(j,i)

	   Suggestion: use method col
	*/
 def transpose =  
	  this // replace this whole line with your implementation
  
  
  /* given an i in [0 .. h-1], rowToString(i) 
    returns a string of the form  | d1 d2 ... dw |
    where d1, ..., dw are the (the string representations of)
    the elements in row i 
 */
 private def rowToString(i:Int) = {
   var s = "| "
   for (j <- 0 to w - 1) 
     s = s + this.apply(i,j) + " "
   s ++ "|"
 }

/* toString returns a string of the form

   | d_00 d_01 ... d_0q |
   | d_10 d_11 ... d_1q |
   |  .    .        .   |
   |  .    .        .   |
   |  .    .        .   |
   | d_p0 d_p1 ... d_pq |
   
   where d_ij is the (the string representation of) this(i,j)
 */
 override def toString =  {
  var s = ""
  for (i <- 0 to h-1) {
    s = s + this.rowToString(i) + "\n"
  }
  s
 }

 // overrides the default definition of == (an alias of equals)
 override def equals(other:Any):Boolean = other match {
   // if o is an instance of Matrix then ...
   case o:Matrix =>
     for (i <- 0 to h - 1; j <- 0 to w - 1)
       if (this(i,j) != o(i,j)) return false
     true
		// otherwise return false
		case _ => false
  }
}


// A few test cases you could try
val m1 = new Matrix(1) ++ new Matrix(2) ++ new Matrix(3) 
val m2 = new Matrix(4) ++ new Matrix(5) ++ new Matrix(6)
val m3 = m1 / m2 / m1
val m4 = m1 ++ m2


