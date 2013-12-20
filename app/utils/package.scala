package object utils {
  implicit class Pipe[T](val t: T) extends AnyVal {
    def |>[B](fun: T => B) = fun(t) 
  }
}