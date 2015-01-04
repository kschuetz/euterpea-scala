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

  sealed trait Music[A]
  case class Prim[A](prim: Primitive[A]) extends Music[A]
  case class :+:[A](left: Music[A], right: Music[A]) extends Music[A]
  case class :=:[A](left: Music[A], right: Music[A]) extends Music[A]
  case class Modify[A](control: Control, music: Music[A]) extends Music[A]
  // deriving (Show, Eq, Ord)

  sealed trait Control
  case class Tempo(tempo: Rational) extends Control
  case class Transpose(absPitch: AbsPitch) extends Control
  case class Instrument(instrumentName: InstrumentName) extends Control
  case class Phrase(attrs: List[PhraseAttribute]) extends Control
  case class Player(playerName: PlayerName) extends Control
  case class KeySig(pitchClass: PitchClass, mode: Mode) extends Control
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

  def note[A](d: Dur, p: A): Music[A] = Prim(Note(d, p))
  def rest(d: Dur): Music[Nothing] = Prim(Rest(d))
  def tempo[A](r: Dur, m: Music[A]): Music[A] = Modify(Tempo(r), m)

  type AbsPitch = Int
  def absPitch(p: Pitch): AbsPitch = p match {
    case (pc, oct) => 12*oct + pcToInt(pc)
  }
  def pcToInt(pc: PitchClass): Int = pc match {
    case Cff => -2
  }
}