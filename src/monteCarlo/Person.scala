package monteCarlo

/**
 * Created by Yang Song on 2015/2/6.
 *
 * For modeling every individual
 */
class Person {
  var level: Levels.levels = _
  var dept: Departments.departments = _
  var evaluation: Double = _
  // churnRate for each month, not for years!
  var churnRate: Double = _
  var loyalty: Double = _
  var time: Int = _
  var timeOnPosition: Int = _

  //reserved for further use.
  var relations = List()
}
