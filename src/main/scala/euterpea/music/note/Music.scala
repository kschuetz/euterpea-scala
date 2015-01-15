package euterpea.music.note

import spire.math.Rational

object Music {
  type Octave = Int
  type Pitch = (PitchClass, Octave)
  type Dur = Rational
  sealed trait PitchClass
  case object Cff extends PitchClass
  case object Cf extends PitchClass
  case object C extends PitchClass
  case object Dff extends PitchClass
  case object Cs extends PitchClass
  case object Df extends PitchClass
  case object Css extends PitchClass
  case object D extends PitchClass
  case object Eff extends PitchClass
  case object Ds extends PitchClass
  case object Ef extends PitchClass
  case object Fff extends PitchClass
  case object Dss extends PitchClass
  case object E extends PitchClass
  case object Ff extends PitchClass
  case object Es extends PitchClass
  case object F extends PitchClass
  case object Gff extends PitchClass
  case object Ess extends PitchClass
  case object Fs extends PitchClass
  case object Gf extends PitchClass
  case object Fss extends PitchClass
  case object G extends PitchClass
  case object Aff extends PitchClass
  case object Gs extends PitchClass
  case object Af extends PitchClass
  case object Gss extends PitchClass
  case object A extends PitchClass
  case object Bff extends PitchClass
  case object As extends PitchClass
  case object Bf extends PitchClass
  case object Ass extends PitchClass
  case object B extends PitchClass
  case object Bs extends PitchClass
  case object Bss extends PitchClass
  // deriving (Show, Eq, Ord, Read, Enum, Bounded)

  sealed trait Primitive[+A]
  case class Note[A](dur: Dur, value: A) extends Primitive[A]
  case class Rest(dur: Dur) extends Primitive[Nothing]
  // deriving (Show, Eq, Ord)

  sealed trait Music[+A]
  case class Prim[A](prim: Primitive[A]) extends Music[A]
  case class :+:[A](left: Music[A], right: Music[A]) extends Music[A]
  case class :=:[A](left: Music[A], right: Music[A]) extends Music[A]
  case class Modify[A](control: Control, music: Music[A]) extends Music[A]
  // deriving (Show, Eq, Ord)

  sealed trait Control
  object Control {
    case class Tempo(tempo: Rational) extends Control
    case class Transpose(absPitch: AbsPitch) extends Control
    case class Instrument(instrumentName: InstrumentName) extends Control
    case class Phrase(attrs: List[PhraseAttribute]) extends Control
    case class Player(playerName: PlayerName) extends Control
    case class KeySig(pitchClass: PitchClass, mode: Mode) extends Control
  }
  // deriving (Show, Eq, Ord)

  type PlayerName = String

  sealed trait Mode
  case object Major extends Mode
  case object Minor extends Mode

  sealed trait InstrumentName
  case object AcousticGrandPiano extends InstrumentName
  case object Vibraphone extends InstrumentName
  case object AcousticBass extends InstrumentName
  case object Flute extends InstrumentName
  case object TenorSax extends InstrumentName
  case object AcousticGuitarSteel extends InstrumentName
  case object Viola extends InstrumentName
  case object StringEnsemble1 extends InstrumentName

  sealed trait PhraseAttribute
  case class Dyn(dyn: Dynamic) extends PhraseAttribute
  case class Tmp(tmp: Tempo) extends PhraseAttribute
  case class Art(art: Articulation) extends PhraseAttribute
  case class Orn(orn: Ornament) extends PhraseAttribute

  sealed trait Dynamic
  object Dynamic {
    case class Accent(acc: Rational) extends Dynamic
    case class Crescendo(cres: Rational) extends Dynamic
    case class Diminuendo(dim: Rational) extends Dynamic
    case class StdLoudness(l: Music.StdLoudness) extends Dynamic
    case class Loudness(l: Rational) extends Dynamic
  }

  sealed trait StdLoudness
  sealed trait Tempo
  sealed trait Articulation
  case class Staccato(r: Rational) extends Articulation
  case class Legato(r: Rational) extends Articulation

  sealed trait Ornament
  sealed trait NoteHead

  def note[A](d: Dur, p: A): Music[A] = Prim(Note(d, p))
  def rest(d: Dur): Music[Nothing] = Prim(Rest(d))
  def tempo[A](r: Dur, m: Music[A]): Music[A] = Modify(Control.Tempo(r), m)

  val bn: Dur = 2
  val wn: Dur = 1
  val hn: Dur = wn/2
  val qn: Dur = wn/4
  val en: Dur = wn/8
  val sn: Dur = wn/16
  val tn: Dur = wn/32
  val sfn: Dur = wn/64

  type AbsPitch = Int
  def absPitch(p: Pitch): AbsPitch = p match {
    case (pc, oct) => 12*oct + pcToInt(pc)
  }
  def pcToInt(pc: PitchClass): Int = pc match {
    case Cff => -2
    case Cf => -1
    case C => 0
    case Dff => 0
    case Cs => 1
    case Df => 1
    case Css => 2
    case D => 2
    case Eff => 2
    case Ds => 3
    case Ef => 3
    case Fff => 3
    case Dss => 4
    case E => 4
    case Ff => 4
    case Es => 5
    case F => 5
    case Gff => 5
    case Ess => 6
    case Fs => 6
    case Gf => 6
    case Fss => 7
    case G => 7
    case Aff => 7
    case Gs => 8
    case Af => 8
    case Gss => 9
    case A => 9
    case Bff => 9
    case As => 10
    case Bf => 10
    case Ass => 11
    case B => 11
    case Bs => 12
    case Bss => 13
  }
}