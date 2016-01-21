package euterpea.music.note

import euterpea.music.note.Music._

object MoreMusic {
  def line[A](ml: List[Music[A]]): Music[A] = ml.foldRight(rest[A](0)){case (l, r) => :+:(l, r)}

  def chord[A](ml: List[Music[A]]): Music[A] = ml.foldRight(rest[A](0)){case (l, r) => :=:(l, r)}

  def delayM[A](d: Dur)(m: => Music[A]): Music[A] =
    :+:(rest(d), m)

  def timesM[A](n: Int)(m: => Music[A]): Music[A] =
    if(n <= 0) rest[A](0)
    else :+:(m, timesM(n - 1)(m))

  def dur[A](music: Music[A]): Dur = music match {
    case Prim(Note(d, _)) => d
    case Prim(Rest(d)) => d
    case :+:(m1, m2) => dur(m1) + dur(m2)
    case :=:(m1, m2) => dur(m1).max(dur(m2))
    case Modify(Control.Tempo(r), m) => dur(m) / r
    case Modify(_, m) => dur(m)
    case m: Lazy[A] => dur(m.value)
  }

  def takeM[A](d: Dur)(music: => Music[A]): Music[A] =
    if(d <= 0) rest[A](0)
    else music match {
      case Prim(Note(oldD, p)) => note(oldD.min(d), p)
      case Prim(Rest(oldD)) => rest(oldD.min(d))
      case :=:(m1, m2) => :=:(takeM(d)(m1), takeM(d)(m2))
      case :+:(m1, m2) =>
        val m1a = takeM(d)(m1)
        val m2a = takeM(d - dur(m1a))(m2)
        :+:(m1a, m2a)
      case Modify(Control.Tempo(r), m) => tempo(r)(takeM(d * r)(m))
      case Modify(c, m) => Modify(c, takeM(d)(m))
      case m: Lazy[A] => takeM(d)(m.value)
    }

  def repeatM[A](music: Music[A]): Music[A] =
    :+:(music, Lazy(repeatM(music)))

  def lineToList[A](music: Music[A]): List[Music[A]] = music match {
    case Prim(Rest(d)) if d.isZero => Nil
    case :+:(n, ns) => n :: lineToList(ns)
    case lm: Lazy[A] => lineToList(lm.value)
    case _ => throw new IllegalArgumentException("lineToList: argument not created by function line")
  }

  def invert(music: Music[Pitch]): Music[Pitch] = {
    val l@(Prim(Note(_, r)) :: _) = lineToList(music)
    val r2 = 2 * absPitch(r)

    line(l.collect {
      case Prim(Note(d, p)) => note(d, pitch(r2 - absPitch(p)))
      case p@Prim(Rest(_)) => p
    })
  }

  def mkLn[A](n: Int, p: A, d: Dur): Music[A] = {
    val music = note(d, p)
    (0 until n).foldLeft(rest[A](0)){
      case (result, _) => :=:(music, result)
    }
  }

  def pMap[A,B](pa: Primitive[A])(f: A => B): Primitive[B] = pa match {
    case Note(d, x) => Note(d, f(x))
    case r@Rest(_) => r
  }

  def mMap[A,B](ma: Music[A])(f: A => B): Music[B] = ma match {
    case Prim(p) => Prim(pMap(p)(f))
    case :+:(m1, m2) => :+:(mMap(m1)(f), mMap(m2)(f))
    case :=:(m1, m2) => :=:(mMap(m1)(f), mMap(m2)(f))
    case Modify(c, m) => Modify(c, mMap(m)(f))
    case m: Lazy[A] => mMap(m.value)(f)
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
