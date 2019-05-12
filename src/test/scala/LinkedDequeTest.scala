import org.scalatest._

/**
  * @since 2019
  * @author Anton Vlasov - whalemare
  */
class LinkedDequeTest extends FlatSpec {

  "Linked" should "add items to head" in {
    val linked = new LinkedDeque[Int]()
    linked.addHead(0)
    linked.addHead(1)
    linked.addHead(2)
    assert(linked.peekHead === 2)
  }

  "Items" should "be inserted to last" in {
    val linked = new LinkedDeque[Int]()
    linked.addLast(0)
    linked.addLast(1)
    linked.addLast(2)
    assert(linked.peekHead === 0)
  }

  "Items" should "be deleted from head" in {
    val linked = new LinkedDeque[Int]()
    linked.addLast(0)
    linked.addLast(1)
    linked.addLast(2)
    assert(linked.removeHead === 0)
    assert(linked.peekHead === 1)
    assert(linked.peekLast === 2)
  }

  "Items" should "deleted from last" in {
    val linked = new LinkedDeque[Int]()
    linked.addLast(0)
    linked.addLast(1)
    linked.addLast(2)
    assert(linked.removeLast === 2)
    assert(linked.peekHead === 0)
    assert(linked.peekLast === 1)
  }

  "Items" should "iteratable" in {
    val linked = new LinkedDeque[Int]()
    linked.addLast(0)
    linked.addLast(1)
    linked.addLast(2)
    for (item <- linked) {
//      println(item)
    }
  }

  "Items" should "be indexed" in {
    val linked = new LinkedDeque[Int]()
    linked.addLast(-123) // 0
    linked.addLast(2) // 1
    linked.addLast(333) // 2
    linked.addLast(1111) // 3
    linked.addLast(4) // 4
    linked.addLast(9) // 5
    assert(linked.indexOf(333) === 2)
  }

  "Items" should "be sorted" in {
    val linked = new LinkedDeque[Int]()
    linked.addLast(20)
    linked.addLast(10)
    linked.addLast(40)
    linked.addLast(30)
    linked.addLast(60)
    linked.addLast(50)
    linked.sort((left, right) => {
      left.compareTo(right)
    })
    println(linked.toString)
  }
}
