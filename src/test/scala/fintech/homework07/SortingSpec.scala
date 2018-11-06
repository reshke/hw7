package fintech.homework07
import org.scalatest.{FlatSpec, Matchers}
import fintech.homework07.Sorting._
import scala.collection.mutable.IndexedSeq

class SortingSpec extends FlatSpec with Matchers {
  it should "do smth" in {
    val result = mergeSort(Seq(1 , 2 ,3))

    result should be(Seq(1 ,2 , 3))
  }

  it should "sort correctly" in {
    val result = mergeSort(Seq(3, 2, 1))
    val resultSecond = mergeSort(Seq(2, 1, 3))
    val correctSortedSeq = Seq(1 ,2 , 3)

    result should be(correctSortedSeq)
    resultSecond should be(correctSortedSeq)
  }

  it should "sort correctly T <: Seq" in {
    val result = mergeSort(List(3, 2, 1))
    val resultSecond = mergeSort(IndexedSeq(2, 3, 1, 4) )

    result should be(List(1, 2, 3))
    resultSecond should be(IndexedSeq(1, 2, 3, 4))
  }

  it should "sort with implicit ordering well" in {
    implicit val intOrdering = new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = if (x < y) 1 else if (x == y) 0 else -1
    }
    val resultSecond = mergeSort(IndexedSeq(2, 3, 1, 4))

    resultSecond should be(IndexedSeq(4, 3, 2, 1))
  }

  it should "sort with implicit ordering well, some another case" in {
    implicit val intOrdering = new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = if (x % 5 > y % 5) 1 else if (x % 5 == y % 5) 0 else -1
    }
    val resultSecond = mergeSort(IndexedSeq(2, 3, 1, 4, 5, 6, 7, 9, 8))

    resultSecond should be(IndexedSeq(5, 1, 6, 2, 7, 3, 8, 4, 9)) // сохраняет порядок
  }
}
