package fintech.homework07

import scala.annotation.tailrec
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

/**
  * Реализовать алгоритмы quick-sort и merge-sort
  *  использую *подходящие* *мутабельные* коллекции
  */

object Sorting {
  private def mergeSort_times[T](list: Buffer[T])(implicit ord: Ordering[T]): Buffer[T] = {
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

  def mergeSort[T](inputList: ListBuffer[T])(implicit ordering: Ordering[T]): Unit = {

    def makeSorting(inputList: ListBuffer[T])(implicit ordering: Ordering[T]): ListBuffer[T] = {
      def merge(left: ListBuffer[T], right: ListBuffer[T]): ListBuffer[T] = {
        (left.length, right.length) match {
          case (0, _) => right
          case (_, 0) => left
          case (_, _) =>
            if (ordering.compare(left.head, right.head) <= 0)
              left.head +: merge(left.tail, right)
            else
              right.head +: merge(left, right.tail)
        }
      }

      val n = inputList.length / 2
      if (n == 0) inputList
      else {
        val (left, right) = inputList splitAt n
        merge(makeSorting(left), makeSorting(right))
      }
    }

    val sorted: ListBuffer[T] = makeSorting(inputList)
    inputList.clear()
    for (item <- sorted)
      inputList += item
  }


  def quickSort[T](data: ListBuffer[T])(implicit ord: Ordering[T]): Unit = {
    def quickSort_times(left: Int = 0, right: Int = data.length - 1)(implicit ord: Ordering[T]): Unit = {
      def swap(i: Int, j: Int): Unit = {
        val pair = (data(i), data(j)).swap
        data(i) = pair._1
        data(j) = pair._2
      }

      def partition(left: Int, right: Int): Int = {
        val pivot = data(right)
        var i = left - 1
        for (j <- left until right) {
          if (ord.compare(data(j), pivot) < 0) {
            if (i != j) {
              i += 1
              swap(i, j)
            }
          }
        }

        swap(i + 1, right)

        i + 1
      }

      if (left < right) {
        val p = partition(left, right)

        quickSort_times(left, p - 1)
        quickSort_times(p + 1, right)
      }
    }

    val result: ListBuffer[T] = ListBuffer.empty[T]

    quickSort_times()
    for (item <- data)
      result.+=:(item)

    data.clear()

    for (item <- result)
      data.+=:(item)
  }
}
