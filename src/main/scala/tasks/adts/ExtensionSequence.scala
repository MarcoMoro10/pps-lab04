package tasks.adts

object ExtensionSequence extends App:
  enum Sequence[A]:
    case Nil()
    case Cons(head: A, tail: Sequence[A])

  object SequenceExtensions:

    import Sequence.*

    extension [A](seq: Sequence[A])
      def map[B](f: A => B): Sequence[B] = seq match
        case Nil() => Nil()
        case Cons(h, t) => Cons(f(h), t.map(f))

      def filter(pred: A => Boolean): Sequence[A] = seq match
        case Nil() => Nil()
        case Cons(h, t) =>
          if pred(h) then Cons(h, t.filter(pred)) else t.filter(pred)

      def contains(value: A): Boolean = seq match
        case Nil() => false
        case Cons(h, t) => h == value || t.contains(value)

      def distinct: Sequence[A] =
        def go(seen: Sequence[A], remaining: Sequence[A]): Sequence[A] = remaining match
          case Nil() => Nil()
          case Cons(h, t) =>
            if seen.contains(h) then go(seen, t)
            else Cons(h, go(Cons(h, seen), t))

        go(Nil(), seq)