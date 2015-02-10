package monteCarlo

import monteCarlo.Levels._

import scala.collection.mutable._

/**
 * Created by Yang Song on 2015/2/6.
 * To denote the departments
 */
object Departments {

  trait departments {
    var father: departments = _
    var children: List[departments] = Nil
    //This one needs to be modified when newbies are coming in
    val members: ListBuffer[Person] = ListBuffer.empty
    // waitList is used for determining how many people are recruiting
    val waitList: ListBuffer[(levels,Int)] = ListBuffer.empty
    // register is used to note how many people are present
    val register: Map[levels,Int] = Map.empty
    val capacity: Map[levels, Int]
  }

  case class Null() extends departments {
    val capacity: Map[levels, Int] = Map.empty
  }

  case class CEO()
    extends departments {
    val capacity: Map[levels, Int] = Map(Senior() -> 1, Junior() -> 1, Branch() -> 1, Clerk() -> 1)
  }

  case class Research() extends departments {
    val capacity: Map[levels, Int] = Map(Senior() -> 1, Clerk() -> 1, EEmployee() -> 2)
  }

  case class CIO() extends departments {
    val capacity: Map[levels, Int] = Map(Senior() -> 2, EEmployee() -> 10, Branch() -> 2)
  }

  case class CFO() extends departments {
    val capacity: Map[levels, Int] = Map(Senior() -> 2, EEmployee() -> 10, Branch() -> 2)
  }

  case class HR() extends departments {
    val capacity: Map[levels, Int] = Map(Senior() -> 1, Clerk() -> 1, EEmployee() -> 2)
  }

  case class VP() extends departments {
    val capacity: Map[levels, Int] = Map(Senior() -> 1, Junior() -> 1, Division() -> 1, Clerk() -> 1)
  }

  case class Facilities() extends departments {
    val capacity: Map[levels, Int] = Map(Senior() -> 1, Clerk() -> 1, EEmployee() -> 2)
  }

  case class Marketing() extends departments {
    val capacity: Map[levels, Int] = Map(Senior() -> 1, Clerk() -> 1, EEmployee() -> 2)
  }

  case class Networks() extends departments {
    val capacity: Map[levels, Int] = Map(Junior() -> 2, Branch() -> 2, Clerk() -> 2, EEmployee() -> 4, IEmployee() -> 2, Division() -> 2)
  }

  case class Information() extends departments {
    val capacity: Map[levels, Int] = Map(Junior() -> 2, Branch() -> 2, Clerk() -> 2, EEmployee() -> 4, IEmployee() -> 2, Division() -> 2)
  }

  case class Program() extends departments {
    val capacity: Map[levels, Int] = Map(Junior() -> 2, Branch() -> 2, Clerk() -> 2, EEmployee() -> 4, IEmployee() -> 2, Division() -> 2)
  }

  case class Product() extends departments {
    val capacity: Map[levels, Int] = Map(Junior() -> 2, Branch() -> 4, EEmployee() -> 2, IEmployee() -> 2, Clerk() -> 2, Division() -> 2)
  }

  case class Green() extends departments {
    val capacity: Map[levels, Int] = Map(Junior() -> 2, Branch() -> 2, Clerk() -> 2, EEmployee() -> 4, IEmployee() -> 2, Division() -> 2)
  }

  case class Blue() extends departments {
    val capacity: Map[levels, Int] = Map(Junior() -> 2, Branch() -> 2, Clerk() -> 2, EEmployee() -> 4, IEmployee() -> 2, Division() -> 2)
  }

  case class Regional() extends departments {
    val capacity: Map[levels, Int] = Map(Junior() -> 2, Branch() -> 2, Division() -> 2, EEmployee() -> 2, IEmployee() -> 4, Clerk() -> 2)
  }

  case class Worldwide() extends departments {
    val capacity: Map[levels, Int] = Map(Junior() -> 2, Branch() -> 2, Division() -> 2, EEmployee() -> 2, IEmployee() -> 4, Clerk() -> 2)
  }

  case class Internet() extends departments {
    val capacity: Map[levels, Int] = Map(Junior() -> 2, Branch() -> 2, Division() -> 2, EEmployee() -> 2, IEmployee() -> 4, Clerk() -> 2)
  }

  class D() extends departments {
    val capacity: Map[levels, Int] = Map(Division() -> 1, EEmployee() -> 1, IEmployee() -> 1, Clerk() -> 1)
  }

  class B() extends departments {
    val capacity: Map[levels, Int] = Map(EEmployee() -> 4, IEmployee() -> 10)
  }
}
