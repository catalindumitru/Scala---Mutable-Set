// A class of objects to represent a set of integers using a linked list.

// Details of the implementation:
// - I use a dummy header node in order to remove particular cases from
//   operations such as find and remove.
// - I do not store an element several times, because that would complicate 
//   operations like size, remove, equals, union or intersect.
// - I store the elements in increasing order so that I am able to print
//   the set like that and to make find faster.
// - I include a variable "card", the cardinal of the set, in order to make
// - the size function O(1)

// Terminology: "DTI" means datatype invariant and "Abs" stands for the
// abstraction function.

import org.scalatest.FunSuite

// Some unit tests.
class TestSet extends FunSuite{
  val set = IntSet(1,2,3,4)
  val empty = IntSet()
  // small set operations
  test ("Print set") { assert(set.toString == "{1, 2, 3, 4}") }
  test ("Print singleton"){ assert(IntSet(1).toString == "{1}")}
  test ("Print empty set"){ assert(empty.toString == "{}")}
  test ("Adding elments") { set.add(5); assert(set == IntSet(1,2,3,4,5)) }
  test ("Check for size") { assert(set.size === 5)}
  test ("Remove element") { assert(set.remove(1) === true);
                            assert(set.size === 4);
                            assert(set.remove(10) === false);
                            assert(set.size === 4) }
  test ("Contains elem")  { assert(set.contains(1) === false);
                            assert(set.contains(2) === true) }
  test ("Check equality") { assert(set.equals(IntSet(2,3,4,5)))}
  test ("Check subset")   { assert(set.subsetOf(IntSet(-4,-3,-2,-1)) === false)}
  test ("Check union")    { assert(set.union(IntSet(1,6,7)) == IntSet(1,2,3,4,5,6,7));
                            assert(set.union(IntSet(10,1,12)) == IntSet(10,1,12).union(set))}
  test ("Check intersect"){ assert(set.intersection(IntSet(0)) == IntSet()) }
  def succ(x: Int) = x + 1
  def odd(x: Int) = x % 2 == 1
  test ("Check map")      { assert(set.map(succ) === IntSet(3,4,5,6))}
  test ("Check filter")   { assert(set.filter(odd) == IntSet(3,5))}

  // empty set operations
  test ("Remove elem from {}") {assert(empty.remove(5) === false)}
  test ("Return elem of {}") {intercept[AssertionError](empty.any)}
  test ("Add elem to {}") { empty.add(5); assert(empty == IntSet(5));
                            empty.remove(5); assert(empty.size === 0)}
  test ("Union with {}") { assert(empty.union(set) == set)
                           assert(empty.union(set) == set.union(empty)) }
  test ("Intersect with {}") { assert(empty.intersection(set) == empty)
                               assert(empty.intersection(set) == set.intersection(empty)) }
  test ("Apply map to {}"){ assert(empty.map(succ) == IntSet())}
  test ("Appy filter to {}") {assert(empty.filter(odd) == IntSet())}
}

class IntSet{
  // State: S : P(Int) (where "P" represents power set)

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  private type Node = IntSet.Node
  // Constructor
  private def Node(datum: Int, next: Node) = new Node(datum, next)

  // Init: S = {}

  // Abs: S = {i / i is an Int && i = n.datum for some n in L(theSet.next) }
  // DTI: card = size of S

  private var theSet : Node = Node(-1,null) // dummy header
  private var card = 0

  /** Convert the set to a string. */
  override def toString : String = {
    if (size == 0) return "{}"
    if (size == 1) return "{" + theSet.next.datum + "}"
    var string = "{" + theSet.next.datum
    var set = theSet.next.next
    while (set != null) {
      string = string + ", " + set.datum
      set = set.next
    }
    string = string + "}"
    return string
  } // O(card)

  // returns node after which e should be found in an increasing order
  // and a boolean that records whether e is in the set or not.
  private def find(e: Int): (Node, Boolean) = {
    var set = theSet
    while (set.next != null && set.next.datum < e)
      set = set.next
    return (set, set.next != null && set.next.datum == e)
  } // O(card)

  /** Add element e to the set
    * Post: S = S_0 U {e} */
  def add(e: Int) : Unit = {
    var (node,isIn) = find(e)
    if (!isIn) {
      node.next = Node(e,node.next)
      card += 1
    }
  } // O(card)

  /** Length of the list
    * Post: S = S_0 && returns #S */
  def size : Int = card // O(1)

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S) */
  def contains(e: Int) : Boolean = {
    var (node, isIn) = find(e)
    return isIn
  } // O(card)

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
  def any : Int = {
    assert (size > 0, "Set is empty")
    return theSet.next.datum
  } // O(1)

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  override def equals(that: Any) : Boolean = that match {
    // Simply check if sizes are equal
    // and if they are, compare each parallel element
    case s: IntSet => {
      if (s.size != this.size) return false
      var setThis = theSet.next
      var setThat = s.theSet.next
      while (setThis != null && setThis.datum == setThat.datum) {
        setThis = setThis.next;
        setThat = setThat.next;
      }
      return setThis == null
    }
    case _ => false
  } // O(card)

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  def remove(e: Int) : Boolean = {
    var (node, isIn) = find(e)
    if (!isIn) return false
    node.next = node.next.next
    card -= 1
    return true
  } // O(card)

  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
  def subsetOf(that: IntSet) : Boolean = {
    if (size == 0) return true
    if (size > that.size) return false
    var setThis = theSet.next
    var setThat = that.theSet.next
    while (setThis != null){ // O(card)
      while(setThat != null && setThat.datum < setThis.datum) setThat = setThat.next
      if(setThat == null || setThat.datum > setThis.datum) return false
      setThis = setThis.next
    }
    return true
  } // O(that.card)

  // ----- optional parts below here -----

  private def addLeft(e: Int): Unit = {
    theSet.next = Node(e,theSet.next)
    card += 1
  } // O(1)

  private def reverse : IntSet = {
    var res = IntSet()
    var set = theSet.next
    while (set != null) {
      res.addLeft(set.datum)
      set = set.next
    }
    return res
  }

  /** return union of this and that.
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  def union(that: IntSet) : IntSet = {
    val u = IntSet()
    var setThis = theSet.next; var setThat = that.theSet.next
    while (setThis != null && setThat != null){
      if (setThis.datum < setThat.datum){
        u.addLeft(setThis.datum); setThis = setThis.next
      }
      else if (setThis.datum > setThat.datum){
        u.addLeft(setThat.datum); setThat = setThat.next
      }
      else {u.addLeft(setThat.datum); setThis = setThis.next; setThat = setThat.next}
    }
    if(setThis != null)
      while(setThis != null) {u.addLeft(setThis.datum); setThis = setThis.next}
    if(setThat != null)
      while(setThat != null) {u.addLeft(setThat.datum); setThat = setThat.next}
    return u.reverse

  } // O(union.card), O(card + that.card)

  /** return intersection of this and that.
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  def intersection(that: IntSet) : IntSet = {
    val i = IntSet()
    var setThis = theSet.next; var setThat = that.theSet.next
    while (setThis != null && setThat != null) {
      if (setThis.datum < setThat.datum)
        setThis = setThis.next
      else if (setThis.datum > setThat.datum)
        setThat = setThat.next
      else {i.addLeft(setThat.datum); setThis = setThis.next; setThat = setThat.next}
    }
    return i.reverse
  } // O(min{card,that.card})

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
  } // O(card^2)

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  def filter(p : Int => Boolean) : IntSet = {
    val res = IntSet()
    var set = theSet.next
    while (set != null) {
      if(p(set.datum)) res.addLeft(set.datum)
      set = set.next
    }
    return res.reverse
  } // O(card)
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
}
