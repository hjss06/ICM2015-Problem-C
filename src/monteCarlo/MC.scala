package monteCarlo

/**
 * Created by Yang on 2015/2/8.
 */
object MC {
  def sample(time: Int, times: Int,p:Array[Double],v:Array[Int],l:Array[Array[Int]]): Unit ={
    var averageProfit = 0.0
    var averageVac = 0
    for(i <- 1 to times){
      val eng = new Company
      eng.start(time,p,v,l)
      averageProfit += eng.profit
      averageVac += eng.maxPeople - eng.people.size
    }
    averageProfit /= times
    averageVac /= times
    println(s"Average Profit for ${time}th month: ${averageProfit}")
    println(s"Average Vacancy for ${time}th month: ${averageVac}")
  }
}
