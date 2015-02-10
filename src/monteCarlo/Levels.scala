package monteCarlo

/**
 * Created by Yang Song on 2015/2/6.
 * To denote level of positions
 */
object Levels {

  trait levels {
    val timeThresh = 24
    val salary: Double
    val trainingCost: Double
    val totalNumber: Int
    val recruitmentCost: Double
    val recruitmentTime: Int
    val rank: Int
    val churnRate: Double = 0.016401583188387914
  }

  val highestRank = 6

  case class Senior() extends levels {
    val salary = 8.0
    val trainingCost = 0.5
    val totalNumber = 10
    val recruitmentCost = 1.2
    val recruitmentTime = 7
    val rank = 6
  }

  case class Junior() extends levels {
    val recruitmentTime = 6
    val recruitmentCost = 0.7
    val totalNumber = 20
    val salary = 4.0
    val trainingCost = 0.6
    val rank = 5
    override val churnRate = 0.029285530376777613
  }

  case class Branch() extends levels {
    val recruitmentTime = 5
    val recruitmentCost = 0.6
    val totalNumber = 25
    val salary = 2.0
    val trainingCost = 0.2
    val rank = 4
    override val churnRate = 0.029285530376777613
  }

  case class Division() extends levels {
    val recruitmentTime = 4
    val recruitmentCost = 0.6
    val totalNumber = 25
    val salary = 1.5
    val trainingCost = 0.3
    val rank = 3
    //override val churnRate = 0.029601261615544328
  }

  case class EEmployee() extends levels {
    val recruitmentTime = 3
    val recruitmentCost = 0.3
    val totalNumber = 110
    val salary = 1.0
    val trainingCost = 0.1
    val rank = 2
  }

  case class IEmployee() extends levels {
    val recruitmentTime = 1
    val recruitmentCost = 0.1
    val totalNumber = 150
    val salary = 0.9
    val trainingCost = 0.3
    val rank = 1
  }

  case class Clerk() extends levels {
    val recruitmentTime = 2
    val recruitmentCost = 0.3
    val totalNumber = 30
    val salary = 0.9
    val trainingCost = 0.05
    val rank = 1
  }

}
