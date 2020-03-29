// A class of objects to represent a set

class IntSet{
  // State: S : P(Int) (where "P" represents power set)

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  private type Node = IntSet.Node
  // Constructor
  private def Node(datum: Int, next: Node) = new IntSet.Node(datum, next)

  // Init: S = {}

  // Abs: S = {i / i is an Int && i = theSet.next.nex. ... . datum}
  // DTI: card = size of S

  // dummy header
  private var theSet : Node = Node(-1,null) // or however empty set is represented
  private var card = 0

  /** Convert the set to a string.
    * (The given implementation is not sufficient.) */
  override def toString : String = {
    if (size == 0) return "{}"
    if (size == 1) return "{" + theSet.next.datum + "}"
    var string = "{" + theSet.next.datum + ", "
    var set = theSet.next.next
    while (set.next != null) {
      string = string + set.datum + ", "
      set = set.next
    }
    string = string + set.datum + "}"
    return string
  }

  /** Add element e to the set
    * Post: S = S_0 U {e} */
  def add(e: Int) : Unit = {
    var node = find(e)
    var t = node.next
    node.next = Node(e,t)
    card += 1
  }

  /** Length of the list
    * Post: S = S_0 && returns #S */
  def size : Int = card

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S) */
  def contains(e: Int) : Boolean = {
    var node = find(e)
    if (node.next == null) return false
    return true
  }

  // returns node after which e is found
  def find(e: Int): Node = {
    // room for time improvement; binary search
    var set = theSet
    while (set.next != null) {
      if (set.next.datum == e) return set
      set = set.next
    }
    return set
  }

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
  def any : Int = {
    assert (size > 0, "Set is empty")
    return theSet.next.datum
  }

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  override def equals(that: Any) : Boolean = that match {
    // room for time improvement; simply check if sizes are equal
    // and if they are, compare each parallel element
    case s: IntSet => subsetOf(s) && s.subsetOf(this)
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  def remove(e: Int) : Boolean = {
    var node = find(e)
    if (node.next == null) return false
    node.next = node.next.next
    card -= 1
    return true
  }

  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
  def subsetOf(that: IntSet) : Boolean = {
    if (size == 0) return true
    if (size > that.size) return false
    var set = theSet.next
    while (set != null){
      if(! that.contains(set.datum)) return false
      set = set.next
    }
    return true
  }

  // ----- optional parts below here -----

  /** return union of this and that.
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  def union(that: IntSet) : IntSet = {
    val res = IntSet()
    val copyThis = IntSet(); var setThis = theSet.next
    val copyThat = IntSet(); var setThat = that.theSet.next
    while (setThis != null) {
      copyThis.add(setThis.datum)
      setThis = setThis.next
    }
    while (setThat != null) {
      copyThat.add(setThat.datum)
      setThat = setThat.next
    }

    while(copyThat.size > 0) {
      val a = copyThat.any
      res.add(a)
      if(copyThis.contains(a)) copyThis.remove(a)
      copyThat.remove(a)
    }
    while(copyThis.size > 0) {
      val a = copyThis.any
      res.add(a)
      copyThis.remove(a)
    }
    return res
  }

  /** return intersection of this and that.
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  def intersect(that: IntSet) : IntSet = {
    val res = IntSet()
    val copyThis = this.union(IntSet())
    val copyThat = that.union(IntSet())
    if(copyThat.size < copyThis.size){
      while(copyThat.size > 0) {
        val a = copyThat.any
        if(copyThis.contains(a)){
          copyThis.remove(a)
          res.add(a)
        }
        copyThat.remove(a)
      }
    } else {
      while(copyThis.size > 0) {
        val a = copyThis.any
        if(copyThat.contains(a)){
          copyThat.remove(a)
          res.add(a)
        }
        copyThis.remove(a)
      }
    }
    return res
  }

  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
  def map(f: Int => Int) : IntSet = {
    val res = IntSet()
    var set = theSet.next
    while (set != null) {
      res.add(f(set.datum))
      set = set.next
    }
    res
  }

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  def filter(p : Int => Boolean) : IntSet = {
    val res = IntSet()
    var set = theSet.next
    while (set != null) {
      if(p(set.datum)) res.add(set.datum)
      set = set.next
    }
    res
  }
}


// The companion object
object IntSet{
  /** The type of nodes defined in the linked list */
  private class Node(var datum: Int, var next: Node)

  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined
    * the main constructor and the add operation.
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply(xs: Int*) : IntSet = {
    val s = new IntSet; for(x <- xs) s.add(x); s
  }

def main(args: Array[String]) = {
  val Set = IntSet(3,4,5)
  val Set2 = IntSet(3,4,1)
  val empty = IntSet()
  def succ(x: Int): Int = x+1
  def even(x: Int): Boolean = x % 2 == 0
  println(Set.union(Set2))
  println(Set.intersect(Set2))
  println(Set.map(succ))
  println(Set.filter(even))

  //println(Set.remove(4))
  //println(Set.toString)

  //println(Set.subsetOf(Set2))

  //println(Set.toString)
  //println(Set.any)
  //println(empty.toString)
  //println(empty.any)
}
}
