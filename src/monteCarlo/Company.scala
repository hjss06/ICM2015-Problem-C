package monteCarlo

/**
 * Created by Yang Song on 2015/2/6.
 *
 * For the company object. The interface for Monte Carlo simulation.
 */

import java.io.PrintWriter

import breeze.linalg.min
import breeze.numerics.inf
import breeze.stats.distributions._
import monteCarlo.Departments._
import monteCarlo.Levels.levels

import scala.collection.mutable._
import scala.util.Random
import scala.util.control.Breaks._

class Company {
  val out = new PrintWriter("res.dat")
  val profitToSalary:Double = 0.4 / 12
  var profit: Double = 0.0
  var averageProfit: Double = 0.0
  var averageVac: Int = 0
  // The graph of the whole company
  var company: List[departments] = List()
  // The list of all staffs
  val people: ListBuffer[Person] = ListBuffer.empty
  val maxPeople = 370
  // The current time
  var month = 0
  // Recruitment power
  val recruitCap = 35
  var recruitNum = 0
  // Visited for travelling through the dept graph
  val visited: Map[departments,Boolean] = Map.empty

  // The cost of talent management
  var cost:Double = 0.0
  def initTreeStructure() = {
    // The Structure of CEO
    val ceo = CEO();
    ceo.father = Null()
    val research = Research();
    research.father = ceo
    val cio = CIO();
    cio.father = ceo
    val cfo = CFO();
    cfo.father = ceo
    val hr = HR();
    hr.father = ceo
    val vp = VP();
    vp.father = ceo
    val facilities = Facilities();
    facilities.father = ceo
    val marketing = Marketing();
    marketing.father = ceo
    ceo.children = research :: cio :: cfo :: hr :: vp :: facilities :: marketing :: Nil

    // The Structure of Research
    val networks = Networks();
    networks.father = research
    val information = Information();
    information.father = research
    research.children = networks :: information :: Nil

    // The Structure of VP
    val program = Program();
    program.father = vp
    val product = Product();
    product.father = vp
    vp.children = program :: product :: Nil

    // The Structure of Facilities
    val blue = Blue();
    blue.father = facilities
    val green = Green();
    green.father = facilities
    facilities.children = blue :: green :: Nil

    // The Structure of Sales Marketing
    val regional = Regional();
    regional.father = marketing
    val worldwide = Worldwide();
    worldwide.father = marketing
    val internet = Internet();
    internet.father = marketing
    marketing.children = regional :: worldwide :: internet :: Nil

    // The Structure of Production Manager
    val d1 = new D();
    d1.father = product
    val d2 = new D();
    d2.father = product
    val d3 = new D();
    d3.father = product
    val d4 = new D();
    d4.father = product
    val d5 = new D();
    d5.father = product
    val d6 = new D();
    d6.father = product
    product.children = d1 :: d2 :: d3 :: d4 :: d5 :: d6 :: Nil

    // The Structure of Directors
    val b1 = new B();
    b1.father = d1
    val b2 = new B();
    b2.father = d1
    d1.children = b1 :: b2 :: Nil

    val b3 = new B();
    b3.father = d2
    val b4 = new B();
    b4.father = d2
    d2.children = b3 :: b4 :: Nil

    val b5 = new B();
    b5.father = d3
    val b6 = new B();
    b6.father = d3
    val b7 = new B();
    b7.father = d3
    d3.children = b5 :: b6 :: b7 :: Nil

    val b8 = new B();
    b8.father = d4
    d4.children = b8 :: Nil

    val b9 = new B();
    b9.father = d5
    val b10 = new B();
    b10.father = d5
    d5.children = b9 :: b10 :: Nil

    val b11 = new B();
    b11.father = d6
    val b12 = new B();
    b12.father = d6
    d6.children = b11 :: b12 :: Nil

    company = {
      ceo :: research :: cio :: cfo :: hr :: vp :: facilities :: marketing ::
        networks :: information :: program :: product :: blue :: green :: regional :: worldwide :: internet ::
        d1 :: d2 :: d3 :: d4 :: d5 :: d6 :: b1 :: b2 :: b3 :: b4 :: b5 ::
        b6 :: b7 :: b8 :: b9 :: b10 :: b11 :: b12 :: Nil
    }
  }

  def initTraverseTheTree(root:departments){
    for((level,limit) <- root.capacity){
      root.register += level -> 0
    }
    visited += root -> false
    for(child <- root.children) initTraverseTheTree(child)
  }

  def initPeople() = {
    for (i <- 1 to maxPeople)
      if (Random.nextDouble() <= 0.85)
        breakable {
          while (true) {

            // Something interesting here, will CEO appear?
            val dept = company(Random.nextInt(company.size))
            for (item <- dept.capacity; level = item._1; limit = item._2) {
              if (dept.members.count(_.level == level) < limit) {
                val newbie: Person = new Person()
                newbie.dept = dept
                newbie.level = level
                newbie.churnRate = level.churnRate
                newbie.loyalty = 0
                newbie.relations = Nil
                newbie.time = 0
                // Randomize the initial working time
                newbie.timeOnPosition = Random.nextInt(newbie.level.timeThresh + 1)
                // Evaluate the performance of the newbie
                newbie.evaluation = Gaussian(0.0,0.194).icdf(Uniform(Gaussian(0.0,0.194).cdf(-0.5),Gaussian(0.0,0.194).cdf(0.5)).draw()) + 0.5
                // Register the newbie
                dept.members += newbie
                people += newbie
                dept.register(newbie.level) += 1
                break
              }
            }
          }
        }

  }

  def init(): Unit = {
    initTreeStructure()
    initTraverseTheTree(company(0))
    initPeople()
  }


  def churnDiffuse(dept: departments, rate: Double = 0.008): Unit = {
    if(dept == Null()) return
    if(visited(dept)) return
    visited(dept) = true
    for(one <- dept.members)  {
      one.churnRate += rate
      if(one.churnRate > 1) one.churnRate = 1
      else if(one.churnRate < 0) one.churnRate = 0
    }
    churnDiffuse(dept.father,rate / 2.0)
    for(child <- dept.children) churnDiffuse(child,rate / 2.0)
  }
  var lucknum = 0
  def promote(dept:departments, lev:levels, queue: Queue[(departments,levels)]): Unit = {
    // Only promote one guy each time. No recursive!
    val ok = {
      people.filter(a => (a.level.rank == lev.rank - 1) && (a.timeOnPosition >= a.level.timeThresh))
    }
    //////////
    /*
    if(!ok.isEmpty)
      out.println(s"Promotion for: dept: $dept, level: $lev")
    for(one <- ok; lev = one.level; time = one.timeOnPosition){
      out.println(s"Level: $lev, Postime: $time")
    }*/
    /////////
    var maxi = -inf
    var theOne: Person = new Person
    theOne.dept = Null()
    for(one <- ok; score = one.evaluation; if score > maxi){
      maxi = score
      theOne = one
    }
    if(theOne.dept != Null()){
      // The effects on churn rates of promotion
      /*
      visited.foreach(a => visited(a._1) = false)
      churnDiffuse(theOne.dept,-0.006)
      */
      theOne.churnRate -= min(theOne.churnRate, 0.004)

      // Remove the lucky guy from the old department
      theOne.dept.members -= theOne
      theOne.dept.register(theOne.level) -= 1
      // Add the old position to the queue to be promoted.
      queue += theOne.dept -> theOne.level

      // Add the lucky guy to the new department
      theOne.timeOnPosition = 0
      theOne.level = lev
      theOne.dept = dept
      theOne.dept.register(lev) += 1
      theOne.dept.members += theOne


      lucknum += 1
    }
  }

  def recruit(rank: Int): Unit ={

    def candidate(rank: Int, cmp: Int): Boolean = {
      // To judge whether there is a candidate with rank rank that can be
      // promoted within cmp time.
      val ok = people.filter(_.level.rank == rank)
      var mini = inf
      var theOne = new Person()
      theOne.dept = Null()
      for{
        one <- ok
        time = one.level.timeThresh - one.timeOnPosition
        if(time < mini)
      }
      {
        mini = time
        theOne = one
      }
      if(theOne.dept == Null() || mini > cmp) return false
      return true
    }

    for {
      dept <- company
      (level, now) <- dept.register
      if(level.rank == rank)
      wait = dept.waitList.count(_._1 == level)
      if(wait + now < dept.capacity(level) && !candidate(rank - 1, level.recruitmentTime) && recruitNum < recruitCap)
    }
    {
      for{
        i <- 1 to (dept.capacity(level) - wait - now)
        if(recruitNum < recruitCap)
      }
      {
        dept.waitList += level -> (level.recruitmentTime - 1)
        profit -= level.recruitmentCost
        recruitNum += 1

        cost += level.recruitmentCost
      }
    }
  }

  def simulate(): Unit = {
    // Time advancing
    profit = 0
    lucknum = 0
    month += 1
    cost = 0
    // Traverse the list of people
   // out.println(s"Month begins: $month\n")


    val runAway: ListBuffer[Person] = ListBuffer.empty
    for(one <- people; run = one.churnRate)
      if(Random.nextDouble() <= run){
        people -= one
        one.dept.members -= one
        one.dept.register(one.level) -= 1
        //Store for calculating the churn of this coward
        runAway += one
      }
    else one.time += 1

   // out.println(s"${runAway.size} people ran away")

    // Calculating the churn of people run away

    visited.foreach(a => visited(a._1) = false)
    for(coward <- runAway)
      churnDiffuse(coward.dept)


    var tmp = 0
    // Update the waitLists of each department
    for(dept <- company)
    {
      val tempList: ListBuffer[(levels,Int)] = ListBuffer.empty
      for((coming,time) <- dept.waitList)
      {
        dept.waitList -= coming -> time
        if (time == 0) {
          tmp += 1
          val newbie = new Person()
          newbie.dept = dept
          newbie.level = coming
          newbie.churnRate = coming.churnRate
          newbie.time = 1
          newbie.timeOnPosition = 0
          newbie.evaluation = Gaussian(0.0, 0.194).icdf(Uniform(Gaussian(0.0, 0.194).cdf(-0.5), Gaussian(0.0, 0.194).cdf(0.5)).draw()) + 0.5

          people += newbie
          newbie.dept.members += newbie
          newbie.dept.register(newbie.level) += 1

          recruitNum -= 1
        }
        else if (time >= 1)
          tempList += coming -> (time - 1)
      }
      dept.waitList ++= tempList
    }
    // out.println(s"$tmp newbies came in")
    // Traverse the departments of the company for promotion
    val promoteOpps: Queue[(departments,levels)] = Queue.empty
    for{
      dept <- company
      (level,now) <- dept.register
      lim = dept.capacity(level)
      waitingNum = dept.waitList.count(_._1 == level)
      if(now + waitingNum < lim)
    }
    {
      for(i <- 1 to (lim - now - waitingNum))
        promoteOpps += (dept -> level)
    }

    while(!promoteOpps.isEmpty){
      val (dept,lev) = promoteOpps.dequeue()
      promote(dept,lev,promoteOpps)
    }

    // Recruiting people from the lowest ranks

    breakable {
      for (i <- 1 to Levels.highestRank; if(i != 4 && i != 5)) {
        if(recruitNum >= recruitCap) break
        recruit(i)
      }
    }

    // Calculate the time on position
    people.foreach(_.timeOnPosition += 1)

    //////
    // Here to sum up the profits of the company
    for(one <- people){
      profit += (one.level.salary + one.level.trainingCost) * profitToSalary * one.evaluation
      cost += one.level.trainingCost / 12
    }
    //////

  }

  def info(p:Array[Double],v:Array[Int],l:Array[Array[Int]]): Unit ={
    p(month) += profit
    v(month) += maxPeople - people.size
    for(one <- people; rank = one.level.rank){
      l(rank)(month) += 1
    }

    /*
    out.println(s"Promote Num: $lucknum")
    out.println(s"Vacant People: ${370 - people.size}")
    out.println(s"Profit: ${profit}")
    out.println(s"Month ends: $month\n")
    */
    /*
    out.println("Department registers:")
    for(dept <- company; (level,now) <- dept.register){
      out.println(s"$dept\t$level:\t$now")
    }
    out.println("Waiting Lists:")
    for(dept <- company; (level,time) <- dept.waitList)
      out.println(s"Dept: $dept, Level: $level, Time: $time")
    */
  }

  def start(time: Int,p:Array[Double],v:Array[Int],l:Array[Array[Int]]): Unit ={
    init()
    for(i <- 1 to time) {
      simulate()
      info(p,v,l)
      averageProfit += profit
      averageVac += maxPeople - people.size
    }
    averageProfit /= time
    averageVac /= time
    out.close()
  }

  //////////////////////////
  // For debug purpose!
  object debug{
    var sum = 0
    var nodes = 0
    var ok: Boolean = true
    val allLevels: Map[Levels.levels,Int] = Map.empty

    def traverseTree(root: departments): Unit = {
      nodes += 1
      var lsum = 0
      for (map <- root.capacity; level = map._1; num = map._2) {
        sum += num
        lsum += num
        allLevels.get(level) match {
          case None => allLevels += level -> num
          case Some(one) => allLevels(level) = num + one
        }
      }
      if (lsum != 4 && lsum != 14) {
        ok = false
      }


      for (child <- root.children) traverseTree(child)
    }

    def verifyLevels(): Unit ={
      for( (level,now) <- allLevels; limit = level.totalNumber)
        if(now != limit)  throw new Exception(level.toString)
    }

    def printPeople(): Unit = {
      sum = 0
      for (one <- people) {
        sum += 1
        println(s"Person $sum Dept: ${one.dept} Level: ${one.level}")
      }
    }
  }
  //////////////////////////
}
