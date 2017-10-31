object state_monad {

def problem1(v: Int) = {
	println("in problem")
	v + 1
}                                                 //> problem1: (v: Int)Int
val p1 = problem1(1)                              //> in problem
                                                  //| p1  : Int = 2

def problem2(v: Int, log: Any => Unit) = {
	log("in problem")
	v + 1
}                                                 //> problem2: (v: Int, log: Any => Unit)Int

val p2 = problem2(1, Console.println _)           //> in problem
                                                  //| p2  : Int = 2

val problem3 = (problem2 _).curried               //> problem3  : Int => ((Any => Unit) => Int) = scala.Function2$$Lambda$10/24065
                                                  //| 0537@1cd072a9

val problem4 = (v: Int) => (log: String) => (v + 1, log + ";in problem")
                                                  //> problem4  : Int => (String => (Int, String)) = state_monad$$$Lambda$11/20880
                                                  //| 51243@4c203ea1
// Int => String => (Int, String)
// A => S => (A, S)
def lift[A, S](a: A): S => (A, S) = (s: S) => (a, s)
                                                  //> lift: [A, S](a: A)S => (A, S)

val problem5 = lift[Int, String](1)               //> problem5  : String => (Int, String) = state_monad$$$Lambda$12/1239731077@213
                                                  //| 3c8f8
// how to combine value with fuction?
// I don't want to have function in problem body's function!

// In general how to combine S => (A, S) with A => B?

def map[S, A, B](sa: S => (A, S), f: A => B): S => (B, S) =
  (s: S) => {
  	val (a, newState) = sa(s)
  	(f(a), newState)
  }                                               //> map: [S, A, B](sa: S => (A, S), f: A => B)S => (B, S)

// functor law

val foo = (i: Int) => i + 0.42                    //> foo  : Int => Double = state_monad$$$Lambda$13/997110508@1e643faf
val bar = (d: Double) => d + " everything"        //> bar  : Double => String = state_monad$$$Lambda$14/1854778591@7a79be86

val leftf = map(map(lift[Int, String](42), foo), bar)
                                                  //> leftf  : String => (String, String) = state_monad$$$Lambda$15/885951223@b684
                                                  //| 286
val rightf = map(lift[Int, String](42), foo andThen bar)
                                                  //> rightf  : String => (String, String) = state_monad$$$Lambda$15/885951223@3f3
                                                  //| afe78

map(map(lift[Int, String](42), foo), bar)("") == map(lift[Int, String](42), foo andThen bar)("")
                                                  //> res0: Boolean = true

// But how to play with state?
// we need to combine S => (A, S) with A => S => (B, S)

def fmap[S, A, B](sa: S => (A, S), f: A => S => (B, S)): S => (B, S) =
  (s: S) => {
    val (a, newState) = sa(s)
    f(a)(newState)
  }                                               //> fmap: [S, A, B](sa: S => (A, S), f: A => (S => (B, S)))S => (B, S)

// monad law

val mfoo = (i: Int) => (s: String) => (foo(i), s + ";mfoo")
                                                  //> mfoo  : Int => (String => (Double, String)) = state_monad$$$Lambda$17/74516
                                                  //| 0567@246ae04d
val mbar = (d: Double) => (s: String) => (bar(d), s + ";mbar")
                                                  //> mbar  : Double => (String => (String, String)) = state_monad$$$Lambda$18/16
                                                  //| 44443712@5315b42e

val leftm = fmap(fmap(lift[Int, String](42), mfoo), mbar)
                                                  //> leftm  : String => (String, String) = state_monad$$$Lambda$19/788117692@5d6
                                                  //| 24da6
val rightm = fmap(lift[Int, String](42), (i: Int) => fmap(mfoo(i), mbar))
                                                  //> rightm  : String => (String, String) = state_monad$$$Lambda$19/788117692@60
                                                  //| addb54

leftm("side effects")._2                          //> res1: String = side effects;mfoo;mbar
leftm("") == rightm("")                           //> res2: Boolean = true


// infix representation
// straight forward
class State[S, A](val sa: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = new State(s => {
    val (a, newState) = sa(s)
    (f(a), newState)
  })
  def fmap[B](f: A => State[S, B]): State[S, B] = new State(s => {
  	val (a, newState) = sa(s)
  	f(a).sa(newState)
  })
  def apply(s: S) = sa(s)
}

val sfoo = (i: Int) => new State((s: String) => (foo(i), s + ";mfoo"))
                                                  //> sfoo  : Int => state_monad.State[String,Double] = state_monad$$$Lambda$23/4
                                                  //| 23031029@6615435c
val sbar = (d: Double) => new State((s: String) => (bar(d), s + ";mbar"))
                                                  //> sbar  : Double => state_monad.State[String,String] = state_monad$$$Lambda$2
                                                  //| 4/1225373914@3a03464

val lefts = (new State[String, Int](lift(42))).fmap(sfoo).fmap(sbar)
                                                  //> lefts  : state_monad.State[String,String] = state_monad$State$1@617c74e5
val rights = (new State[String, Int](lift(42))).fmap((i: Int) => sfoo(i).fmap(sbar))
                                                  //> rights  : state_monad.State[String,String] = state_monad$State$1@67b6d4ae

lefts("side effects")._2                          //> res3: String = side effects;mfoo;mbar

lefts("") == rights("")                           //> res4: Boolean = true

// If this is monad...

trait Monad[A, M[_]] {
  def map[B](ma: M[A], f: A => B): M[B]
  def fmap[B](ma: M[A], f: A => M[B]): M[B]
}

implicit class addMonad[S, T: ({type MM[C] = Monad[C, ({type ST[A] = S => (A, S)})#ST]})#MM](ft: S => (T, S)) {
	def map[B](f: T => B): S => (B, S) =
	  implicitly[Monad[T, ({type ST[C] = S => (C, S)})#ST]].map(ft, f)
	def fmap[B](f: T => S => (B, S)): S => (B, S) =
	  implicitly[Monad[T, ({type ST[C] = S => (C, S)})#ST]].fmap(ft, f)
}

implicit def stateMonad[S, A] = new Monad[A, ({type ST[C] = S => (C, S)})#ST] {
  def map[B](ma: S => (A, S), f: A => B) = s => {
    val (a, newState) = ma(s)
    (f(a), newState)
  }

  def fmap[B](ma: S => (A, S), f: A => S => (B, S)) = s => {
    val (a, newState) = ma(s)
    f(a)(newState)
  }
}                                                 //> stateMonad: [S, A]=> state_monad.Monad[A,[C]S => (C, S)]

val leftmm = lift[Int, String](42).fmap(mfoo).fmap(mbar)
                                                  //> leftmm  : String => (String, String) = state_monad$$anon$1$$Lambda$29/10189
                                                  //| 37824@35fb3008
val rightmm = lift[Int, String](42).fmap((i:Int) => mfoo(i).fmap(mbar))
                                                  //> rightmm  : String => (String, String) = state_monad$$anon$1$$Lambda$29/1018
                                                  //| 937824@54a097cc

leftmm("side effects")._2                         //> res5: String = side effects;mfoo;mbar
leftmm("") == rightmm("")                         //> res6: Boolean = true
}