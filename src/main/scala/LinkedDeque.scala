import java.util
import java.util.NoSuchElementException

import com.sun.istack.internal.Nullable

class LinkedDeque[Item] extends Iterable[Item] {

  private case class Node[T](item: T, @Nullable var next: Node[T])

  var mSize: Int = 0

  @Nullable
  private var mHead: Node[Item] = _

  @Nullable
  private var mLast: Node[Item] = _

  override def isEmpty: Boolean = mHead == null

  def peekHead: Item = {
    if (mHead == null) throw new NoSuchElementException("Queue underflow")
    mHead.item
  }

  def peekLast: Item = {
    if (mLast == null) throw new NoSuchElementException("Queue underflow")
    mLast.item
  }

  def addHead(item: Item): Unit = {
    val prevHead = mHead
    val newNode = new Node[Item](item, prevHead)
    mHead = newNode
    if (prevHead == null) mLast = newNode
    mSize += 1
  }

  def addLast(item: Item): Unit = {
    val l = mLast
    val newNode = Node(item, null)
    mLast = newNode
    if (l == null) mHead = newNode
    else l.next = newNode
    mSize += 1
  }

  def removeHead: Item = {
    if (isEmpty) throw new NoSuchElementException("Queue is empty")
    val item = mHead.item
    mHead = mHead.next
    mSize -= 1
    if (isEmpty) mLast = null
    item
  }

  def removeLast: Item = {
    if (mLast == null) throw new NoSuchElementException("Queue is empty")
    val element = mLast.item
    if (mHead eq mLast) {
      mHead = null
      mLast = null
    }
    else {
      var prevToLast = mHead
      while ( {
        prevToLast.next ne mLast
      }) prevToLast = prevToLast.next
      mLast = prevToLast
      mLast.next = null
    }
    mSize -= 1
    element
  }

  def indexOf(item: Item): Int = {
    var index = 0
    if (item == null) {
      return mSize
    } else {
      var x = mHead
      while (x != null) {
        if (item == x.item) return index
        index += 1

        x = x.next
      }
    }
    -1
  }

  override def toString: String = {
    val s = new StringBuilder
    var skipped = false
    for (item <- this) {
      if (skipped) s + "->" + item
      else s + item.toString
      skipped = true
    }
    s.toString
  }

  override def iterator: Iterator[Item] = new ListIterator(mHead)

  private class ListIterator(var current: Node[Item]) extends Iterator[Item] {
    override def hasNext: Boolean = current != null

    override def next: Item = {
      if (!hasNext) throw new NoSuchElementException
      val item = current.item
      current = current.next
      item
    }
  }

}