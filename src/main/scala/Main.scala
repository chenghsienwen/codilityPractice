import scala.collection.mutable.ListBuffer

object Solution {
  def solution1(a: Array[Int]): Int = {
    a.length == 0 match {
      case true => 1
      case false => {
        var previous = 0
        var miss = 0
        (0 until a.length).map{ i =>
          previous+1 == a(i) match {
            case true => previous = a(i)
            case false => {
              miss = previous + 1
              previous = miss
            }
          }
        }
        previous == a.last match {
          case true => previous+1
          case false => miss
        }
      }
    }
  }

  def findMissElement(a: Array[Int]): Int = {
    (0 to a.length+1).sum - a.sum
  }

  def TapeEquilibrium(a: Array[Int]): Int = {
    val total = a.sum
    var leftSum = 0
    (0 until a.length-1).map{ i =>
      leftSum += a(i)
      Math.abs((2*leftSum) - total)
    }.min
  }

  def permutation(a: Array[Int]): Int = {
    val setSum = a.toSet.sum
    (1 to a.length).sum - setSum == 0 match {
      case true => 1
      case false => 0
    }
  }

  def frogRiverOne(x: Int, a: Array[Int]): Int = {
    var set = Set[Int]()

    val temp = (0 until a.length).map{ i =>
        a(i) <= x match {
          case true => {
            set += a(i)
            set.size == x match {
              case true => i
              case false => Int.MaxValue
            }
          }
          case false => Int.MaxValue
        }
    }.min
    temp == Int.MaxValue match {
      case true => -1
      case false => temp
    }
  }

  def MissingInteger1(a: Array[Int]): Int = {
    val set = a.toSet.filter(i => i>0)
    set.size == 0 match {
      case true => 1
      case false => {
        (1 to set.max + 1).map { i =>
          set.exists(x => x == i) match {
            case true => Int.MaxValue
            case false => i
          }
        }.min
      }
    }
  }

  def MissingInteger2(a: Array[Int]): Int = {

    val set = a.toSet.filter(i => i>0).toList.sorted
    set.size == 0 match {
      case true => 1
      case false => {
        var min = 1
        (0 until set.length).map{ i =>
          set(i) == min match {
            case true => min+=1
            case false =>
          }
        }
        min
      }
    }
  }

  def MaxCounters1(n: Int, a: Array[Int]): Array[Int] = {
    def trace(a:Array[Int])(max:Int, accu: Array[Int]): Array[Int] = {
      a.headOption match {
        case Some(x) => {
          x match {
            case x if x <= n => {
              accu(x-1) += 1
              val currentMax = accu(x-1)>max match {
                case true => accu(x-1)
                case fale => max
              }
              trace(a.tail)(currentMax, accu)
            }
            case x if x == n+1 => {
              trace(a.tail)(max, accu.map(j => max))
            }
            case _ => trace(a.tail)(max, accu)
          }
        }
        case None => accu
      }
    }
    trace(a)(0, new Array[Int](n))
  }

  def MaxCounters2(n: Int, a: Array[Int]): Array[Int] = {
    def trace(a:Array[Int])(lastMax: Int, max:Int, accu: Array[Int]): Array[Int] = {
      a.headOption match {
        case Some(x) => {
          x match {
            case x if x <= n => {
              accu(x-1)=Math.max(accu(x-1), lastMax)
              accu(x-1)+=1
              val currentMax = Math.max(accu(x-1), max)
              trace(a.tail)(lastMax, currentMax, accu)
            }
            case x if x == n+1 => {
              trace(a.tail)(max, max, accu)
            }
            case _ => trace(a.tail)(lastMax, max, accu)
          }
        }
        case None => {
          accu.map{ i =>
            Math.max(i, lastMax)
          }
        }
      }
    }
    trace(a)(0, 0, new Array[Int](n))
  }

  def PassingCars1(a: Array[Int]): Int = {
    def trace(a: Array[Int])(accu: Int): Int = {
      a.headOption match {
        case Some(x) => x == 0 match {
          case true => trace(a.tail)(accu + a.tail.sum)
          case false => trace(a.tail)(accu)
        }
        case None => accu
      }
    }
    trace(a)(0)
  }

  def PassingCars2(a: Array[Int]): Int = {
    def trace(a: Array[Int])(countZero: Int, accu: Int): Int = {
      a.headOption match {
        case Some(x) => x == 0 match {
          case true => trace(a.tail)(countZero + 1, accu)
          case false => trace(a.tail)(countZero, accu + countZero)
        }
        case None => accu
      }
    }
    trace(a)(0, 0)
  }

  def PassingCars3(a: Array[Int]): Int = {
    var zCount = 0
    var total = 0
    (0 until a.length).map{ i =>
      a(i) match{
        case 0 =>zCount +=1
        case 1 =>total += zCount
      }
    }
    Math.abs(total) > 1000000000 match {
      case true => -1
      case fale => total
    }
  }

  def PassingCars4(a: Array[Int]): Int = {
    def trace(a: List[Int])(countZero: Int, accu: Int): Int = {
      a.headOption match {
        case Some(x) => x == 0 match {
          case true => trace(a.tail)(countZero + 1, accu)
          case false => trace(a.tail)(countZero, accu + countZero)
        }
        case None => accu
      }
    }
    val result = trace(a.toList)(0, 0)
    Math.abs(result) > 1000000000 match {
      case true => -1
      case fale => result
    }
  }

  def GenomicRangeQuery1(s: String, p: Array[Int], q: Array[Int]): Array[Int] = {
    val impactCode = s.toList.map{ i =>
      i match {
        case 'A' => 1
        case 'C' => 2
        case 'G' => 3
        case 'T' => 4
      }
    }
    (0 until p.size).map{ i =>
      (p(i) to q(i)).map(impactCode(_)).min
    }.toArray
  }

  def GenomicRangeQuery2(s: String, p: Array[Int], q: Array[Int]): Array[Int] = {
    val genre = Map('A' -> 1, 'C'->2, 'G'->3, 'T'->4)
    val occur = Array.fill[Int](s.length, 4){-1}
    s.indices.map{ i =>
      i > 0 match {
        case true => occur(i).indices.map{ j =>
          occur(i)(j) = occur(i-1)(j)
        }
        case false =>
      }
      occur(i)(genre.get(s.charAt(i)).getOrElse(0) - 1) = i
    }
    val result = new Array[Int](p.length)
    (0 until p.length).map{ i =>
      for (j <- occur(q(i)).indices if result(i) == 0) {
        if (occur(q(i))(j) >= p(i)) {
          result(i) = j + 1
        }
      }
    }
    result
  }

  def MinAvgTwoSlice2(a: Array[Int]): Int = {
    var minAvg = Double.MaxValue
    var minIndex = 0
    a.indices.map{ i =>
      val avg2 = i < a.length-1 match {
        case true => (a(i) + a(i+1)).toDouble/2
        case false => Double.MaxValue
      }
      val avg3 = i < a.length-2 match {
        case true => (a(i) + a(i+1) + a(i+2)).toDouble/3
        case false => Double.MaxValue
      }
      val min = Math.min(avg2, avg3)
      min < minAvg match {
        case true => {
          minAvg = min
          minIndex = i
        }
        case false =>
      }
    }
    minIndex
  }

  def MinAvgTwoSlice1(a: Array[Int]): Int = {

    def findMinAvg(a: List[Int]): Double = {
      var accu:Double = a(0)
      var minAvg = Double.MaxValue
      (1 until a.length).map{ i =>
        if (i < 3) {
          accu += a(i)
          val avg = accu/(1+i)
          if (avg < minAvg) {
            minAvg = avg
          }
        }
      }
      minAvg
    }
    def trace(a: List[Int], index: Int)(minIndex: Int, avg:Double):Int = {
      a match {
        case head :: tail => {
          val tempAvg = findMinAvg(a)
          tempAvg < avg match {
            case true => trace(tail, index+1)(index, tempAvg)
            case false => trace(tail, index+1)(minIndex, avg)
          }
        }
        case Nil => minIndex
      }
    }

    trace(a.toList, 0)(0, Int.MaxValue)
  }

  def CountDiv1(a: Int, b: Int, k: Int): Int = {
    var count = 0
    (a to b).map{ i =>
      i%k == 0 match {
        case true => count+=1
        case false =>
      }
    }
    count
  }

  def CountDiv2(a: Int, b: Int, k: Int): Int = {
    val div = (b / k - a / k)
    val mul =  a % k == 0 match {
      case true => 1
      case false => 0
    }

    div + mul
  }

  def Distinct(a: Array[Int]): Int = {
    a.distinct.size
  }

  def MaxProductOfThree(a: Array[Int]): Int = {
    val sort = a.sorted
    val test1 = sort.takeRight(3).foldLeft(1)(_*_)
    val test2 = sort.take(2).foldLeft(1)(_*_)*sort.last
    Math.max(test1, test2)
  }

  def Triangle(a: Array[Int]): Int = {
    val list = a.sorted.map(i => i.toLong)
    (2 until list.length).map{ i =>
      val a = list(i-2)
      val b = list(i-1)
      val c = list(i)
      (a+b>c, b+c>a, c+a>b) match {
        case (true, true, true) => return 1
        case (_, _, _) => 0
      }
    }
    return 0
  }

  def NumberOfDiscIntersections1(a: Array[Int]): Int = {
    (0 until a.length).map{ i =>
      (0 until a.length).map{ j =>
        val dist = Math.abs(i-j)
        dist>0 match {
          case true => {
            a(i) + a(j) >= dist match {
              case true => 1
              case false => 0
            }
          }
          case false => 0
        }
      }
    }.flatten.sum/2
  }

  def NumberOfDiscIntersections2(a: Array[Int]): Int = {
    //https://rafal.io/posts/codility-intersecting-discs.html
    val list1 = new Array[Long](a.length)
    val list2 = new Array[Long](a.length)
    (0 until a.length).map{ i =>
      list1(i) = i+a(i)
      list2(i) = -(a(i)-i)
    }
    val list2Sort = list2.toList.sorted
    def trace(list: List[Long], check: Long)(accu: Long): Long = {
      list match {
        case head::tail => head > check match {
          case true => accu
          case false => trace(tail, check)(accu+1)
        }
        case Nil => accu
      }
    }
    val count = list1.map{ i =>
      trace(list2Sort, i)(0)
    }.sum
    val sub = (a.length.toLong)*(a.length.toLong+1)/2
    val result = count - sub
    result > 10000000 match {
      case true => -1
      case _ => result.toInt
    }
  }

  def Brackets(s: String): Int = {
    def isPair(a: Char, b: Char):Boolean = {
      (a,b) match {
        case ('(', ')') | ('[', ']') | ('{', '}') => true
        case _ => false
      }
    }
    def trace(list: List[Char])(accu: List[Char]): Int = {
      list match {
        case head::tail => {
          head match {
            case '[' | '{' | '(' => trace(tail)(head :: accu)
            case ']' | '}' | ')' => {
              accu.isEmpty match {
                case true => -1
                case false => {
                  isPair(accu.head,head) match {
                    case true => trace(tail)(accu.tail)
                    case false => trace(tail)(accu)
                  }
                }
              }
            }
            case _ => trace(tail)(accu)
          }
        }
        case Nil => accu.size
      }
    }
    trace(s.toList)(List.empty[Char]) == 0 match {
      case true => 1
      case false => 0
    }
  }

  def fish1(a: Array[Int], b: Array[Int]): Int = {
    def trace(a: List[Int], b: List[Int])(accuA: List[Int], accuB:List[Int]): List[Int] = {
      a match {
        case head::tail => {
          (accuB.headOption.getOrElse(-1), b.head) match {
            case (1, 0) => {
              accuA.head > head match {
                case true => trace(tail, b.tail)(accuA, accuB)
                case false =>trace(tail, b.tail)(head::accuA.tail, b.head::accuB.tail)
              }
            }
            case (_, _) => trace(tail, b.tail)(head::accuA, b.head::accuB)
          }
        }
        case Nil => accuA
      }
    }
    trace(a.toList, b.toList)(List.empty[Int], List.empty[Int]).size
  }

  def fish2(a: Array[Int], b: Array[Int]): Int = {
    def trace(a: List[Int], b: List[Int])(accuA: List[Int], accuB:List[Int]): List[Int] = {
      a match {
        case head::tail => {
          (accuB.headOption.getOrElse(-1), b.head) match {
            case (1, 0) => {
              accuA.head > head match {
                case true => trace(tail, b.tail)(accuA, accuB)
                case false =>trace(a, b)(accuA.tail, accuB.tail)
              }
            }
            case (_, _) => trace(tail, b.tail)(head::accuA, b.head::accuB)
          }
        }
        case Nil => accuA
      }
    }
    trace(a.toList, b.toList)(List.empty[Int], List.empty[Int]).size
  }

  def maxAdjecentIndex1(a: Array[Int]): Int = {
    val sort = a.indices.map{ i =>
      a(i) -> i
    }.toList.sorted

    a.indices.map{ i =>
      var maxIndex = 0
      var minDis = Int.MaxValue
      sort.map{ j =>
        val temp = a(i)-j._1
        val tempDiffIndex = Math.abs(i-j._2)
        (temp < minDis, tempDiffIndex>maxIndex) match {
          case (true, true) => {
            minDis = temp
            maxIndex = tempDiffIndex
          }
          case (_, _) =>
        }
      }
      maxIndex-1
    }.max
  }

  def maxAdjecentIndex2(a: Array[Int]): Int = {
    if (a.length < 2) return -1
    val sort = a.indices.map{ i =>
      a(i) -> i
    }.toList.sorted

    a.indices.map{ i =>
      var maxIndex = 0
      var minDis = Int.MaxValue
      sort.map{ j =>
        val temp = a(i)-j._1
        temp == 0 match {
          case true =>
          case false => {
            val tempDiffIndex = Math.abs(i-j._2)
            (temp < minDis, tempDiffIndex>maxIndex) match {
              case (true, true) => {
                minDis = temp
                maxIndex = tempDiffIndex
              }
              case (_, _) =>
            }
          }
        }
      }
      maxIndex-1
    }.max
  }

  def maxBiValueSize(a: Array[Int]): Int = {
    val list = a.toList
    def trace(check: Int, a: List[Int])(bi: List[Int], accu:Int): Int = {
      a match {
        case head::tail => bi.size match {
          case 1 => trace(check, tail)(head::bi, accu+1)
          case 2 => bi.contains(head) match {
            case true => trace(check, tail)(bi, accu+1)
            case false => accu
          }
        }
        case Nil => accu
      }
    }
    list.indices.map{i =>
      trace(list(i), list.takeRight(list.length-i+1))(List(list(i)), 0)
    }.max
  }

  def StoneWall1(h: Array[Int]): Int = {
    def trace(h: List[Int])(block: List[Int],accu: List[Int]): Int = {
      //println("block " + block + " accu " + accu)
      h match {
        case head:: tail => {
          accu.isEmpty match {
            case true => trace(tail)(head::block, head::accu)
            case false => {
              val blockHead = block.headOption.getOrElse(0)
              blockHead == head match {
                case true => trace(tail)(block, accu)
                case false => blockHead > head match {
                      case true => trace(h)(block.tail, accu)
                      case false => trace(tail)(head::block, head:: accu)
                    }
                  }

              }
            }
          }

        case Nil => accu.size
      }
    }
    trace(h.toList)(List.empty[Int], List.empty[Int])
  }
}