package euterpea.music.note

import euterpea.music.note.Music._

object MoreMusic {
  def line[A](ml: List[Music[A]]): Music[A] = ml.foldRight(rest[A](0)){case (l, r) => :+:(l, r)}
  def pMap[A,B](pa: Primitive[A])(f: A => B): Primitive[B] = pa match {
    case Note(d, x) => Note(d, f(x))
    case r@Rest(_) => r
  }
  def mMap[A,B](ma: Music[A])(f: A => B): Music[B] = ma match {
    case Prim(p) => Prim(pMap(p)(f))
    case :+:(m1, m2) => :+:(mMap(m1)(f), mMap(m2)(f))
    case :=:(m1, m2) => :=:(mMap(m1)(f), mMap(m2)(f))
    case Modify(c, m) => Modify(c, mMap(m)(f))
  }
  type Volume = Int
  sealed trait NoteAttribute
  object NoteAttribute {
    case class Volume(v: Int) extends NoteAttribute
    case class Fingering(f: Int) extends NoteAttribute
    case class Dynamics(d: String) extends NoteAttribute
    case class Params(ps: List[Double]) extends NoteAttribute
  }
}
