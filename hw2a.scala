//==================================================
// 22c22 Fall 2013
//
// Homework 2, Part A
//
// Name: Alexander Starr
//
//==================================================


abstract class Dictionary {
  
  /* put(k,v) adds value v to the dictionary under key k.  
     put may be called with the same key multiple times,
     all key->values pairs get stored in the dictionary
  */
  def put(key:String, value: Any):Unit

  /* get(k) returns Some(v) where v is one of the values 
     associated with key k in the dictionary, if any.
     It returns None if k is associated to no value.
  */   
  def get(key:String):Option[Any]

   /* remove(k) removes one value v associated with key k
      from the dictionary, if any, and returns it as Some(v).  
      It returns None if k is associated to no value.
  */   
  def remove(key:String):Option[Any]

 /* toList() returns a list containing all the key/value pairs 
    in the dictionary.
    If multiple values have the same key k, each is listed
    in a separate pair with k.
   */
   def toList():List[(String,Any)]

 /* toString() returns a string containing all the key/value pairs 
     in the dictionary. The string has the form 
     
        Dictionary(key_1 -> value_1, ..., key_n -> value_n)
     
     where each key_i is a key and each value_i is (the string
     representation of) the corresponding value.
     If multiple values have the same key k, each is present
     in a separate pair with k.
   */
   override def toString():String

  /* getAll(k) returns the list of all the values associated
     with key k in the dictionary.
  */   
  def getAll(key:String):List[Any] =
    Nil // replace this whole line with your implementation
  

  /* removeAll(k) removes from the dictionary all the values 
     associated with key k in the dictionary, if any.
  */   
  def removeAll(key:String) {
    // replace this comment with your implementation
  }
}



class ListDictionary extends Dictionary {
  /* the dictionary is implemented using a list of key/value pairs */
  private var d = List[(String,Any)]()
  
  // add your implementation of Dictionary's abstract methods here

}



