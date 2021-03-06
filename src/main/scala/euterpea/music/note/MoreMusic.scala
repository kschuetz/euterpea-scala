package euterpea.music.note

import euterpea.music.note.Music._
import spire.math.Rational

object MoreMusic {

  def serial[A](l: Music[A], r: Music[A]): Music[A] =
    :+:(l, r)

  def parallel[A](l: Music[A], r: Music[A]): Music[A] =
    :=:(l, r)

  def line[A](ml: List[Music[A]]): Music[A] = ml.foldRight(rest[A](0))(serial)

  def line1[A](ml: List[Music[A]]): Music[A] = ml.reduceRight(serial)

  def chord[A](ml: List[Music[A]]): Music[A] = ml.foldRight(rest[A](0))(parallel)

  def chord1[A](ml: List[Music[A]]): Music[A] = ml.reduceRight(parallel)

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

  type LazyDur = List[Dur]

  def durL[A](music: Music[A]): LazyDur = music match {
    case m: Prim[A] => List(dur(m))
    case :+:(m1, m2) =>
      val d1 = durL(m1)
      val d2 = durL(m2)
      d1 ++ d2.map(_ + d1.last)
    case :=:(m1, m2) => mergeLD(durL(m1), durL(m2))
    case Modify(Control.Tempo(r), m) => durL(m).map(_ / r)
    case Modify(_, m) => durL(m)
    case m: Lazy[A] => durL(m.value)
  }

  def mergeLD(dur1: LazyDur, dur2: LazyDur): LazyDur = (dur1, dur2) match {
    case (Nil, ld) => ld
    case (ld, Nil) => ld
    case (ld1@(d1::ds1), ld2@(d2::ds2)) =>
      if(d1 < d2) d1 :: mergeLD(ds1, ld2)
      else d2 :: mergeLD(ld1, ds2)
  }

  def minL(dur1: LazyDur, dur2: Dur): Dur = (dur1, dur2) match {
    case (Nil, d) => d
    case (d1 :: Nil, d2) => d1.min(d2)
    case (d :: ds, d2) =>
      if(d < d2) minL(ds, d2)
      else d2
  }

  def takeML[A](dur1: LazyDur)(music: => Music[A]): Music[A] = dur1 match {
    case Nil => rest(0)
    case (d :: ds) if d <= 0 => takeML(ds)(music)
    case ld => music match {
      case Prim(Note(oldD, p)) => note(minL(ld, oldD), p)
      case Prim(Rest(oldD)) => rest(minL(ld, oldD))
      case :=:(m1, m2) => :=:(takeML(ld)(m1), takeML(ld)(m2))
      case :+:(m1, m2) =>
        val m1a = takeML(ld)(m1)
        val d1 = dur(m1a)
        val m2a = takeML(ld.map(_ - d1))(m2)
        :=:(m1a, m2a)
      case Modify(Control.Tempo(r), m) => tempo(r)(takeML(ld.map(_ * r))(m))
      case Modify(c, m) => Modify(c, takeML(ld)(m))
      case m: Lazy[A] => takeML(ld)(m.value)
    }
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

  def retro(music: Music[Pitch]): Music[Pitch] =
    line(lineToList(music).reverse)

  def retroInvert(music: Music[Pitch]): Music[Pitch] =
    retro(invert(music))

  def invertRetro(music: Music[Pitch]): Music[Pitch] =
    invert(retro(music))

  def mkLn[A](n: Int, p: A, d: Dur): Music[A] = {
    val music = note(d, p)
    (0 until n).foldLeft(rest[A](0)){
      case (result, _) => :=:(music, result)
    }
  }

  def trill(i: Int, sDur: Dur)(music: Music[Pitch]): Music[Pitch] = music match {
    case m@Prim(Note(tDur, p)) =>
      if(sDur >= tDur) m
      else {
        :+:(note(tDur, p),
          trill(-i, sDur)(note(tDur - sDur, trans(i, p))))
      }
    case Modify(Control.Tempo(r), m) => tempo(r)(trill(i, sDur * r)(m))
    case Modify(c, m) => Modify(c, trill(i, sDur)(m))
    case lm: Lazy[Pitch] => trill(i, sDur)(lm.value)
    case _ => throw new IllegalArgumentException("trill: input must be a single note.")
  }

  def trill1(i: Int, sDur: Dur)(music: Music[Pitch]): Music[Pitch] =
    trill(-i, sDur)(transpose(i)(music))

  def trilln(i: Int, nTimes: Int)(music: Music[Pitch]): Music[Pitch] =
    trill(i, dur(music) / nTimes)(music)

  def trilln1(i: Int, nTimes: Int)(music: Music[Pitch]): Music[Pitch] =
    trilln(-i, nTimes)(transpose(i)(music))

  def roll(dur: Dur)(music: Music[Pitch]): Music[Pitch] =
    trill(0, dur)(music)

  def rolln(nTimes: Int)(music: Music[Pitch]): Music[Pitch] =
    trilln(0, nTimes)(music)

  val ssfMel = {
    val l1 = List(trilln(2, 5)(bf(6, en)), ef(7, en), ef(6, en), ef(7, en))
    val l2 = List(bf(6, sn), c(7, sn), bf(6, sn), g(6, sn), bf(5, en))
    val l3 = List(ef(6, sn), f(6, sn), g(6, sn), af(6, sn), bf(6, en), ef(7, en))
    val l4 = List(trill(2, tn)(bf(6, qn)), bf(6, sn), denr)
    line(l1 ++ l2 ++ l3 ++ l4)
  }

  val starsAndStripes = instrument(InstrumentName.Flute)(ssfMel)

  def grace(n: Int, r: Rational)(music: Music[Pitch]): Music[Pitch] = music match {
    case Prim(Note(d, p)) =>
      :+:(note(r * d, trans(n, p)),
          note((1 - r) * d, p))
    case lm: Lazy[Pitch] => grace(n, r)(lm.value)
    case _ => throw new IllegalArgumentException("grace: can only add a grace note to a note")
  }

  def grace2(n: Int, r: Rational)(m1: Music[Pitch], m2: Music[Pitch]): Music[Pitch] = (m1, m2) match {
    case (lm: Lazy[Pitch], _) => grace2(n, r)(lm.value, m2)
    case (_, lm: Lazy[Pitch]) => grace2(n, r)(m1, lm.value)
    case (Prim(Note(d1, p1)), Prim(Note(d2, p2))) =>
      :+:(note(d1-r*d2, p1), :+:(note(r*d2, trans(n, p2)), note(d2, p2)))
    case _ => throw new IllegalArgumentException("grace2: can only add a grace note to a note")
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

  class MoreMusicOps[A](music: Music[A]) {
    def /=:(m2: Music[A]): Music[A] =
      :+:(takeML(durL(m2))(music), takeML(durL(music))(m2))
  }

  implicit def musicToMoreMusicOps[A](music: Music[A]): MoreMusicOps[A] = new MoreMusicOps(music)
}
