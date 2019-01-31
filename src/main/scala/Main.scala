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
    def trace(a: Array[Int])(countZero: Int, countOne: Int, accu: Int): Int = {
      a.headOption match {
        case Some(x) => x == 0 match {
          case true => trace(a.tail)(countZero + 1, countOne, accu + countOne)
          case false => trace(a.tail)(countZero, countOne-1, accu + countZero)
        }
        case None => accu
      }
    }
    trace(a)(0, a.sum, 0)/2
  }
}