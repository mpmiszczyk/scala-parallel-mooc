package notes

object MonteCarlo {

  import scala.util.Random

  def mcCount(iterations: Int): Int = {

    val randomX = new Random
    val randomY = new Random

    var hits = 0
    for (_ <- 0 until iterations){
      val x = randomX.nextDouble()
      val y = randomY.nextDouble()
      if (x*x + y*y < 1) hits += 1
    }
    hits
  }

  def monteCarloPiSeq(iterations: Int): Double = 
    4.0 * mcCount(iterations) / iterations

  def monteCarloPiParallel(iterations: Int): Double = {
    val ((p1, p2), (p3, p4)) =
      (parallel(parallel(mcCount(iterations/4),
                         mcCount(iterations/4)),
                parallel(mcCount(iterations/4),
                         mcCount(iterations - 3 * (iterations / 4)))))
    4.0 * (p1 + p2 + p3 + p4) / iterations
  }

  def parallel[A,B](taskA: => A, taksB: => B): (A,B) = ???
  

  def monteCarloPiWithTasks(iterations: Int): Double = {
    val taskIteratoins = iterations/4
    val p1 = task(mcCount(taskIteratoins))
    val p2 = task(mcCount(taskIteratoins))
    val p3 = task(mcCount(taskIteratoins))
    val p4 = task(mcCount(iterations - 3 * taskIteratoins))

    4.0 * (p1 + p2 + p3 + p4) / iterations
  }

  def task[A](a: => A): Task[A] = ???

  trait Task[A] {
    def join : A
  }

  // changes `Task[A]` itno `A` when needed
  implicit def getJoin[A](x: Task[A]): A = x.join
}
