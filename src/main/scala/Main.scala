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

  def EquiLeader(a: Array[Int]): Int = {
    val t = a.groupBy(i => i).map(i => (i._1 -> i._2.size))
    val leader = t.isEmpty match {
      case true => (-1,-1)
      case false => t.maxBy(_._2)
    }
    val length = a.length
    val leaderNum = leader._1
    val leaderCount = leader._2

    def trace(index: Int, a:List[Int])(leftCount: Int, accu: Int): Int = {
      //println("index "+ index + " accu " + accu)
      a match {
        case head::tail => {
          head == leaderNum match {
            case true => {
              val leaderInLeftPart = leftCount+1
              val leaderInRightPart = leaderCount-leaderInLeftPart
              val left = (index+1)
              val right = (length -index -1)
              //println("leaderInLeftPart " + leaderInLeftPart + " left " + left + " leaderInRightPart " + leaderInRightPart + " right "+right)
              (leaderInLeftPart*2 > left, leaderInRightPart*2 > right) match {
                case (true, true) =>  trace(index+1, tail)(leaderInLeftPart, accu+1)
                case (_, _) =>  trace(index+1, tail)(leaderInLeftPart, accu)
              }
            }
            case false =>  {
              val leaderInLeftPart = leftCount
              val leaderInRightPart = leaderCount-leaderInLeftPart
              val left = (index+1)
              val right = (length -index -1)
              (leaderInLeftPart*2 > left, leaderInRightPart*2 > right) match {
                case (true, true) => trace(index+1, tail)(leftCount, accu+1)
                case (_, _) => trace(index+1, tail)(leftCount, accu)
              }

            }
          }
        }
        case Nil => accu
      }
    }
    a.isEmpty match {
      case true => 0
      case false => trace(0, a.toList)(0, 0)
    }

  }

  def Dominator(a: Array[Int]): Int = {
    val t = a.indices.map(i => i->a(i)).groupBy(i => i._2)
    t.isEmpty match {
      case true => -1
      case false => {
        val max = t.maxBy(_._2.size)
        max._2.size*2 > a.length match {
          case true => max._2.head._1
          case false => -1
        }
      }
    }
  }

  def MaxSliceSum1(a: Array[Int]): Int = {
    def trace(a: List[Int])(accu: Int, max: Int): Int = {
      a match {
        case head :: tail => {
          val temp = accu + head
          temp > max match {
            case true => trace( tail )( temp, temp )
            case false => trace( tail )( temp, max )
          }
        }
        case Nil => max
      }

    }
    def traceByIndex(a: List[Int])(max:Int):Int = {
      a match {
        case head::tail => {
          val tempMax = trace(a)(0, max)
          tempMax > max match {
            case true => traceByIndex(tail)(tempMax)
            case false => traceByIndex(tail)(max)
          }
        }
        case Nil => max
      }
    }
    traceByIndex(a.toList)(Int.MinValue)
  }

  def MaxSliceSum2(a: Array[Int]): Int = {
    def trace(a: List[Int])(accu: Int, max: Int): Int = {
      a match {
        case head :: tail => {
          val temp = Math.max(accu + head, head)
          trace( tail )( temp, Math.max(temp, max) )

        }
        case Nil => max
      }
    }
    trace(a.toList)(0, Int.MinValue)
  }

  def MaxProfit(a: Array[Int]): Int = {
    def trace(a: List[Int])(slice:Int, max: Int): Int = {
      a match {
        case head :: tail => {
          val temp = Math.min(slice, head)
          trace( tail )(temp, Math.max(head - temp, max))
        }
        case Nil => max
      }
    }
    trace(a.toList)(Int.MaxValue, 0)
  }

  def MaxDoubleSliceSum(a: Array[Int]): Int = {
    //https://en.wikipedia.org/wiki/Maximum_subarray_problem
    val list = a.toList
    def trace(a: List[Int])(k: List[Int]):List[Int] = {
      a match {
        case head::tail => {
          trace(tail)(Math.max(k.head+head,0)::k)
        }
        case Nil => k
      }
    }
    val k1 = trace(list.tail)(List(0)).reverse
    val k2 = trace(list.reverse.tail)(List(0))
    //println("k1:"+k1.mkString(",")+" k2: "+ k2.mkString(","))
    (1 until list.length-1).foldLeft(0){(z,i) =>
      //println(i+" k1(i-1) "+ k1(i-1) +" k2(i+1) "+ k2(i+1))
      Math.max(z, k1(i-1)+k2(i+1))
    }
  }

  def CountFactors(n: Int): Int = {
    def trace(n:Int, div: Int)(accu: Int):Int = {
      div*div  match {
        case s if s == n => trace(n, div+1)(accu+1)
        case s if s<n => n%div == 0 match {
          case true => trace(n, div+1)(accu+2)
          case false => trace(n, div+1)(accu)
        }
        case _ => accu
      }
    }
    trace(n, 1)(0)
  }

  def MinPerimeterRectangle(n: Int): Int = {
    def trace(n:Int, div:Int)(min:Int):Int = {
      div*div <= n match {
        case true => n%div == 0 match {
          case true => trace(n, div+1)(Math.min(min, div + (n/div)))
          case false => trace(n, div+1)(min)
        }
        case false => min
      }
    }
    trace(n, 1)(Int.MaxValue)*2
  }

  def Peaks(a: Array[Int]): Int = {
    def check(item:Int, previous:Int):Int = {
      item match {
        case s if s > previous => 1
        case s if s == previous => 0
        case _ => -1
      }
    }
    def trace(a: List[Int])(previous: Int, status: Int, accu: Int): Int = {
      a match {
        case head::tail => {
          val s = check(head, previous)
          (status, s) match {
            case (1, -1) => trace(tail)(head, s, accu+1)
            case (_, _) => trace(tail)(head, s, accu)
          }
        }
        case Nil => accu
      }
    }
    val peaks = trace(a.toList)(0, 0, 0)
    peaks == 0 match {
      case true => 0
      case false => {
        (1 to peaks).map{ i =>
          (a.length%i == 0) match {
            case true => i
            case false => 0
          }
        }.max
      }
    }
  }

  def CountSemiprimes1(n: Int, p: Array[Int], q: Array[Int]): Array[Int] = {
    val primes = 2 #:: Stream.from(3,2)
    def isPrime(n: Int): Boolean =
      primes.takeWhile(p => p*p <= n).forall(n % _ != 0)
    val items = primes.filter(isPrime).takeWhile(i => i*2<=n).toList
    //println("items " + items.mkString(","))
    val semiPrimes = (for {
      x <- items
      y <- items
    } yield {
      x*y
    }).toSet.filter(x => x<=n)
    (0 until p.length).map{ i =>
      semiPrimes.count(x => x >= p(i) && x <= q(i))
    }.toArray
  }

  def CountSemiprimes2(n: Int, p: Array[Int], q: Array[Int]): Array[Int] = {
    val primes = 2 #:: Stream.from(3,2)
    def isPrime(n: Int): Boolean =
      primes.takeWhile(p => p*p <= n).forall(n % _ != 0)
    val items = primes.filter(isPrime).takeWhile(i => i*2<=n).toList
    //println("items " + items.mkString(","))
    val semiPrimes = (for {
      x <- items
      y <- items
    } yield {
      x*y
    }).toSet.filter(x => x<=n)
    def trace(a:List[Int])(accu:Int, result: List[Int]): List[Int] = {
      a match {
        case head::tail => semiPrimes.contains(head) match {
          case true => val add = accu+1;trace(tail)(add, add::result)
          case false => trace(tail)(accu, accu::result)
        }
        case Nil => result
      }
    }
    val accumlative = trace((0 to n).toList)(0,List.empty[Int]).reverse
    //println("accumlative" + accumlative.mkString(","))
    (0 until p.length).map{ i =>
      accumlative(q(i)) - accumlative(p(i)-1)
    }.toArray
  }

  def CountNonDivisible1(a: Array[Int]): Array[Int] = {
    def divisor(a:Int): Set[Int] = {
      (1 to Math.sqrt(a).toInt).map{ i =>
        a%i == 0 match {
          case true => i
          case false => -1
        }
      }.toSet
    }
    val length = a.length
    a.map{ i =>
      val div = divisor(i) + i
      //println("div "+div.mkString(","))
      length - a.count(j => div.contains(j))
    }
  }

  def ChocolatesByNumbers1(n: Int, m: Int): Int = {
    def trace(n:Int, m:Int)(step:Int, segment:Int, accu:Int): Int = {
      println("step " + step + " segment " + segment + " accu " + accu)
      (segment == 2) match {
        case true => accu
        case false => step * m % n == 0 match {
          case true => trace( n, m )( step + 1, segment+1, accu + 1 )
          case false => trace( n, m )( step + 1, segment, accu + 1 )
        }
      }
    }
    Math.min(trace(n,m)(0, 0, 0)-1, n)
  }

  def ChocolatesByNumbers2(n: Int, m: Int): Int = {
    def gcd(a: Int, b: Int): Int = {
      a < b match {
        case true => gcd(b, a)
        case false => {
          val modulo = a % b
          modulo == 0 match {
            case true => b
            case false => gcd(b, modulo)
          }
        }
      }
    }
    n / gcd(n, m)
  }

  def CommonPrimeDivisors1(a: Array[Int], b: Array[Int]): Int = {
    def gcd(a: Int, b: Int): Int = {
      a < b match {
        case true => gcd(b, a)
        case false => {
          val modulo = a % b
          modulo == 0 match {
            case true => b
            case false => gcd(b, modulo)
          }
        }
      }
    }

    def surplus(x:Int, gcd: Set[Int]):Int = {
      gcd.nonEmpty match {
        case true => x%gcd.head == 0 match {
          case true => surplus(x/gcd.head, gcd)
          case false => surplus(x, gcd.tail)
        }
        case false => x
      }
    }

    (0 until a.length).map{ i =>
      val g = gcd(a(i), b(i))

      val items = (2 to g).map{ i => g%i == 0 match {
        case true => i
        case false => -1
      }
      }.toSet.filterNot(i => i == -1)
      val surplus1 = surplus(a(i)/g, items)
      val surplus2 = surplus(b(i)/g, items)
      (surplus1 == 1, surplus2 == 1) match {
        case (true, true) => 1
        case (_, _) => 0
      }
    }.sum
  }

  def CommonPrimeDivisors2(a: Array[Int], b: Array[Int]): Int = {
    def gcd(a: Int, b: Int): Int = {
      a < b match {
        case true => gcd(b, a)
        case false => {
          val modulo = a % b
          modulo == 0 match {
            case true => b
            case false => gcd(b, modulo)
          }
        }
      }
    }

    def surplus(x:Int, gcd: Set[Int]):Int = {
      gcd.nonEmpty match {
        case true => x%gcd.head == 0 match {
          case true => surplus(x/gcd.head, gcd)
          case false => surplus(x, gcd.tail)
        }
        case false => x
      }
    }

    def factors(n: Int, index: Int)(accu:Set[Int]): Set[Int] = {
      index <= n match {
        case true => n%index == 0 match {
          case true => factors(n/index, 2)(accu + index)
          case false => factors(n, index+1)(accu)
        }
        case false => accu
      }
    }

    (0 until a.length).map{ i =>
      a(i) == b(i) match {
        case true => 1
        case false => {
          val g = gcd(a(i), b(i))
          val factor = factors(g, 2)(Set.empty[Int])
          //println("factor "+ factor.mkString(","))
          val surplus1 = surplus(a(i)/g, factor)
          val surplus2 = surplus(b(i)/g, factor)
          (surplus1 == 1, surplus2 == 1) match {
            case (true, true) => 1
            case (_, _) => 0
          }
        }
      }
    }.sum
  }
}