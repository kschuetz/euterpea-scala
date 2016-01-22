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

  def dropM[A](d: Dur)(music: => Music[A]): Music[A] =
    if(d < 0) music
    else music match {
      case Prim(Note(oldD, p)) => note((oldD - d).max(0), p)
      case Prim(Rest(oldD)) => rest((oldD - d).max(0))
      case :=:(m1, m2) => :=:(dropM(d)(m1), dropM(d)(m2))
      case :+:(m1, m2) =>
        val m1a = dropM(d)(m1)
        val m2a = dropM(d - dur(m1a))(m2)
        :+:(m1a, m2a)
      case Modify(Control.Tempo(r), m) => tempo(r)(dropM(d * r)(m))
      case Modify(c, m) => Modify(c, dropM(d)(m))
      case m: Lazy[A] => dropM(d)(m.value)
    }

  def cut[A](d: Dur)(music: => Music[A]): Music[A] =
    dropM[A](d)(music)

  def removeZeros[A](music: => Music[A]): Music[A] = music match {
    case p: Prim[A] => p
    case m0 @ :+:(m1, m2) =>
      val m1a = removeZeros(m1)
      val m2a = removeZeros(m2)
      (m1a, m2a) match {
        case (Prim(Note(d, _)), m) if d.isZero => m
        case (Prim(Rest(d)), m) if d.isZero => m
        case (m, Prim(Note(d, _))) if d.isZero => m
        case (m, Prim(Rest(d))) if d.isZero => m
        case _ => m0
      }
    case m0 @ :=:(m1, m2) =>
      val m1a = removeZeros(m1)
      val m2a = removeZeros(m2)
      (m1a, m2a) match {
        case (Prim(Note(d, _)), m) if d.isZero => m
        case (Prim(Rest(d)), m) if d.isZero => m
        case (m, Prim(Note(d, _))) if d.isZero => m
        case (m, Prim(Rest(d))) if d.isZero => m
        case _ => m0
      }
    case Modify(c, m) => Modify(c, removeZeros(m))
    case m: Lazy[A] => Lazy(removeZeros(m.value))
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
    case lm: Lazy[A] => lm.value match {
      case m: Compound[A] => Lazy(mMap(m)(f))
      case m => mMap(m)(f)
    }
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
