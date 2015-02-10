package main

import java.io.PrintWriter

import monteCarlo.MC
/**
 * Created by Yang Song on 2015/2/6.
 * For ICM Problem C
 */
object Main {
  def main(args: Array[String]) = {
    var profit = Array.fill[Double](24*5+1)(0.0)
    var vacancy = Array.fill[Int](121)(0)

    val levels = Array.fill[Int](7,121)(0)

    MC.sample(120,1000,profit,vacancy,levels)
    profit = profit.map(_/1000)
    vacancy = vacancy.map(_/1000)

    for(rank <- 1 to 6; month <- 1 to 120){
      levels(rank)(month) /= 1000
    }

    val fout = new PrintWriter("Task5.txt")

    for(rank <- 1 to 6)
    {
      fout.println(s"Rank: $rank\n")
      for(month <- 1 to 120) {
        fout.println(s"${levels(rank)(month)},")
      }
    }
    //fout.println("Profit:")
    //for(a <- profit) fout.println(s"$a,")
    //fout.println("Vacancy:")
    //for(a <- vacancy) fout.println(s"$a,")

    fout.close()
  }
}

/*
Experimental results

35 %
Profit average for 24th month in 1000 mc samples: 117.79661322150014
Vacant places for 24th month in 1000 mc samples: 46

35%
Promotion available for at least 2 years
Profit average for 24th month in 1000 mc samples: 103.64963061912646
Vacant places for 24th month in 1000 mc samples: 101

18 %
Profit average for 24th month in 1000 mc samples: 141.9111605322884
Vacant places for 24th month in 1000 mc samples: 30

18 %
Promotion available for at least 2 years
Profit average for 24th month in 1000 mc samples: 124.0331985490739
Vacant places for 24th month in 1000 mc samples: 47
 */
