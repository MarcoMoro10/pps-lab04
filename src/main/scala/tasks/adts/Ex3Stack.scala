package tasks.adts

import u03.Sequences.*
import u03.Optionals.*

/*  Exercise 3: 
 *  Implement a Stack ADT
 *  Suggestion: 
 *  - push adds an element and returns the new stack
 *  - pop returns:
 *  -- empty optional is stack is empty
 *  -- a pair of top of the stack and the new stack after removal if not empty
 */
object Ex3Stacks:

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] // factory
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]

  object StackImpl extends StackADT:
    enum StackImpl[A]:
      case Empty()
      case Node(head: A, tail: Stack[A])

    type Stack[A] = StackImpl[A]
    def empty[A]: Stack[A] = StackImpl.Empty()
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A] = StackImpl.Node(a, stack)
      def pop(): Optional[(A, Stack[A])] = stack match
        case StackImpl.Empty() => Optional.Empty()
        case StackImpl.Node(h,t) => Optional.Just((h,t))
      def asSequence(): Sequence[A] = stack match
        case StackImpl.Empty() => Sequence.Nil()
        case StackImpl.Node(h, t) => Sequence.Cons(h, t.asSequence())