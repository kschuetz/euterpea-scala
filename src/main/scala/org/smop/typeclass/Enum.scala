package org.smop.typeclass

trait Enum[A] {
  def toEnum(i: Int): A
  def fromEnum(a: A): Int
}

object Enum {
  def toEnum[A](i: Int)(implicit e: Enum[A]) = e.toEnum(i)
  def fromEnum[A](a: A)(implicit e: Enum[A]) = e.fromEnum(a)
}