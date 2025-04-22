package tasks.adts

import tasks.adts.Ex1ComplexNumbers.BasicComplexADT.SequenceImpl

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:
    case class SequenceImpl(real: Double, immaginary: Double)

    // Change assignment below: should probably define a case class and use it?
    type Complex = SequenceImpl
    def complex(re: Double, im: Double): Complex = SequenceImpl(re,im)
    extension (complex: Complex)
      def re(): Double = complex.real
      def im(): Double = complex.immaginary
      def sum(other: Complex): Complex =
        SequenceImpl(complex.real + other.real, complex.immaginary + other.immaginary)
      def subtract(other: Complex): Complex =
        SequenceImpl(complex.real - other.real, complex.immaginary - other.immaginary)
      def asString(): String = s"${complex.real} + ${complex.immaginary}i"
