package fintech.homework07

import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

/**
  * Реализовать алгоритмы quick-sort и merge-sort
  *  использую *подходящие* *мутабельные* коллекции
  */

object Sorting {

  private def quickSort_times[T](list: Buffer[T])(implicit ord: Ordering[T]): Buffer[T] = {
    def partition(list: Buffer[T], pivot: T): (Buffer[T], Buffer[T]) = {
      val left = list.filter(item => ord.compare(item, pivot) < 0)
      val right = list.filter(item => ord.compare(item, pivot) >= 0)
      (left, right)
    }

    if (list.length < 2)
      list
    else {
      val (left, right) = partition(list.tail, list.head)
      (quickSort_times(left) :+ list.head) ++ quickSort_times(right)
    }
  }

  private def mergeSort_times[T](list : Buffer[T])(implicit ord : Ordering[T]) : Buffer[T] = {
    def merge(left: Buffer[T], right: Buffer[T]): Buffer[T] = {
      (left.isEmpty, right.isEmpty) match {
        case (true, true) | (false, true) => left
        case (true, false) => right
        case (false, false) =>
          val x = left.head
          val y = right.head
          if (ord.compare(x, y) <= 0) {
            x +: merge(left.tail, right)
          }
          else {
            y +: merge(left, right.tail)
          }
      }
    }

    if (list.length < 2)
      list
    else {
      val (left, right) = list.splitAt(list.length / 2)

      merge(mergeSort_times(left), mergeSort_times(right))
    }
  }

  def mergeSort[T](data : ListBuffer[T])(implicit ord : Ordering[T]): Unit= {
    val result : ListBuffer[T] = ListBuffer.empty[T]

    for (item <-  mergeSort_times(data))
      result.+=:(item)

    data.clear()

    for (item <- result)
      data.+=:(item)
  }

  def quickSort[T](data : ListBuffer[T])(implicit ord : Ordering[T]): Unit= {
    val result : ListBuffer[T] = ListBuffer.empty[T]

    for (item <- quickSort_times(data))
      result.+=:(item)

    data.clear()

    for (item <- result)
      data.+=:(item)
  }
}
