package fintech.homework07
import org.scalatest.{FlatSpec, Matchers}
import fintech.homework07.Sorting._
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.LinearSeq

class SortingSpec extends FlatSpec with Matchers {
  "mergeSort" should "do smth" in {
    val data = ListBuffer(1, 2, 3)
    mergeSort(data)

    data should be(ListBuffer(1, 2, 3))
  }

  "quickSort" should "do smth" in {
    val data = ListBuffer(1, 2, 3)
    quickSort(data)

    data should be(ListBuffer(1, 2, 3))
  }

  "mergeSort" should "sort correctly" in {
    val data = ListBuffer(3, 2, 1)
    val dataSecond = ListBuffer(2, 1, 3)
    val correctSortedData = ListBuffer(1, 2, 3)

    mergeSort(data)
    mergeSort(dataSecond)

    data should be(correctSortedData)
    dataSecond should be(correctSortedData)
  }

  "quickSort" should "sort correctly" in {
    val data = ListBuffer(3, 2, 1)
    val dataSecond = ListBuffer(2, 1, 3)
    val correctSortedData = ListBuffer(1, 2, 3)

    quickSort(data)
    quickSort(dataSecond)

    data should be(correctSortedData)
    dataSecond should be(correctSortedData)
  }

  "mergeSort" should "sort correctly T <% Ordering" in {
    val result = ListBuffer(3, 2, 1)
    val resultSecond = ListBuffer("2", "3", "1", "4")
    val resultThird = ListBuffer("sdbjoisd", "ansoi", "aaaaa", "hgodp", "hh")

    mergeSort(result)
    mergeSort(resultSecond)
    mergeSort(resultThird)

    result should be(ListBuffer(1, 2, 3))
    resultSecond should be(ListBuffer("1", "2", "3", "4"))
    resultThird should be(ListBuffer("aaaaa", "ansoi", "hgodp", "hh", "sdbjoisd"))
  }

  "quickSort" should "sort correctly T <% Ordering" in {
    val result = ListBuffer(3, 2, 1)
    val resultSecond = ListBuffer("2", "3", "1", "4")
    val resultThird = ListBuffer("sdbjoisd", "ansoi", "aaaaa", "hgodp", "hh")

    quickSort(result)
    quickSort(resultSecond)
    quickSort(resultThird)

    result should be(ListBuffer(1, 2, 3))
    resultSecond should be(ListBuffer("1", "2", "3", "4"))
    resultThird should be(ListBuffer("aaaaa", "ansoi", "hgodp", "hh", "sdbjoisd"))
  }

  "mergeSort" should "sort with implicit ordering well" in {
    implicit val intOrdering = new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = if (x < y) 1 else if (x == y) 0 else -1
    }

    val data = ListBuffer(2, 3, 1, 4)
    mergeSort(data)

    data should be(ListBuffer(4, 3, 2, 1))
  }


  "quickSort" should "sort with implicit ordering well" in {
    implicit val intOrdering = new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = if (x < y) 1 else if (x == y) 0 else -1
    }

    val data = ListBuffer(2, 3, 1, 4)
    quickSort(data)

    data should be(ListBuffer(4, 3, 2, 1))
  }

  "mergeSort" should "sort with implicit ordering well, some another case" in {
    implicit val intOrdering = new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = if (x % 5 > y % 5) 1 else if (x % 5 == y % 5) 0 else -1
    }

    val data = ListBuffer(2, 3, 1, 4, 5, 6, 7, 9, 8)
    mergeSort(data)

    data should be(ListBuffer(5, 1, 6, 2, 7, 3, 8, 4, 9)) // сохраняет порядок
  }

  "quickSort" should "sort with implicit ordering well, some another case" in {
    implicit val intOrdering = new Ordering[Int] {
      override def compare(x: Int, y: Int): Int = if (x % 5 > y % 5) 1 else if (x % 5 == y % 5) 0 else -1
    }

    val data = ListBuffer(2, 3, 1, 4, 5, 6, 7, 9, 8, 3, 3, 3)
    quickSort(data)

    data should be(ListBuffer(5, 1, 6, 2, 7, 3, 8, 3, 3, 3, 4, 9)) // тоже сохраняет порядок
  }
}
