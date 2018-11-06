package fintech.homework07

import scala.collection.mutable.ListBuffer

/**
  * Реализовать алгоритмы quick-sort и merge-sort
  *  использую *подходящие* *мутабельные* коллекции
  */

object Sorting {

  private def mergeSort[T](list : ListBuffer[T])(implicit ord : Ordering[T]) : ListBuffer[T] = {
    def merge(left: ListBuffer[T], right: ListBuffer[T]): ListBuffer[T] = {
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

      merge(mergeSort(left), mergeSort(right))
    }
  }

  def mergeSort[T](iterable : Seq[T])(implicit ord : Ordering[T]): Seq[T] = {
    var list = new ListBuffer[T]()
    var result: Seq[T] = iterable

    for (item <- iterable) {
      list += item
      result = result.tail
    }

    list = mergeSort(list)

    for (item <- list) {
      result = result :+ item
    }

    result
  }
//
//  def quickSort(???)

}
