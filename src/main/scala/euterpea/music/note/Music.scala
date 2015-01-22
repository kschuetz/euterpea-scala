package euterpea.music.note

import spire.math.Rational

object Music {
  type Octave = Int
  type Pitch = (PitchClass, Octave)
  type Dur = Rational
  sealed trait PitchClass extends Product with Serializable
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
  object InstrumentName {
    case object AcousticGrandPiano extends InstrumentName
    case object BrightAcousticPiano extends InstrumentName
    case object ElectricGrandPiano extends InstrumentName
    case object HonkyTonkPiano extends InstrumentName
    case object RhodesPiano extends InstrumentName
    case object ChorusedPiano extends InstrumentName
    case object Harpsichord extends InstrumentName
    case object Clavinet extends InstrumentName
    case object Celesta extends InstrumentName
    case object Glockenspiel extends InstrumentName
    case object MusicBox extends InstrumentName
    case object Vibraphone extends InstrumentName
    case object Marimba extends InstrumentName
    case object Xylophone extends InstrumentName
    case object TubularBells extends InstrumentName
    case object Dulcimer extends InstrumentName
    case object HammondOrgan extends InstrumentName
    case object PercussiveOrgan extends InstrumentName
    case object RockOrgan extends InstrumentName
    case object ChurchOrgan extends InstrumentName
    case object ReedOrgan extends InstrumentName
    case object Accordion extends InstrumentName
    case object Harmonica extends InstrumentName
    case object TangoAccordion extends InstrumentName
    case object AcousticGuitarNylon extends InstrumentName
    case object AcousticGuitarSteel extends InstrumentName
    case object ElectricGuitarJazz extends InstrumentName
    case object ElectricGuitarClean extends InstrumentName
    case object ElectricGuitarMuted extends InstrumentName
    case object OverdrivenGuitar extends InstrumentName
    case object DistortionGuitar extends InstrumentName
    case object GuitarHarmonics extends InstrumentName
    case object AcousticBass extends InstrumentName
    case object ElectricBassFingered extends InstrumentName
    case object ElectricBassPicked extends InstrumentName
    case object FretlessBass extends InstrumentName
    case object SlapBass1 extends InstrumentName
    case object SlapBass2 extends InstrumentName
    case object SynthBass1 extends InstrumentName
    case object SynthBass2 extends InstrumentName
    case object Violin extends InstrumentName
    case object Viola extends InstrumentName
    case object Cello extends InstrumentName
    case object Contrabass extends InstrumentName
    case object TremoloStrings extends InstrumentName
    case object PizzicatoStrings extends InstrumentName
    case object OrchestralHarp extends InstrumentName
    case object Timpani extends InstrumentName
    case object StringEnsemble1 extends InstrumentName
    case object StringEnsemble2 extends InstrumentName
    case object SynthStrings1 extends InstrumentName
    case object SynthStrings2 extends InstrumentName
    case object ChoirAahs extends InstrumentName
    case object VoiceOohs extends InstrumentName
    case object SynthVoice extends InstrumentName
    case object OrchestraHit extends InstrumentName
    case object Trumpet extends InstrumentName
    case object Trombone extends InstrumentName
    case object Tuba extends InstrumentName
    case object MutedTrumpet extends InstrumentName
    case object FrenchHorn extends InstrumentName
    case object BrassSection extends InstrumentName
    case object SynthBrass1 extends InstrumentName
    case object SynthBrass2 extends InstrumentName
    case object SopranoSax extends InstrumentName
    case object AltoSax extends InstrumentName
    case object TenorSax extends InstrumentName
    case object BaritoneSax extends InstrumentName
    case object Oboe extends InstrumentName
    case object EnglishHorn extends InstrumentName
    case object Bassoon extends InstrumentName
    case object Clarinet extends InstrumentName
    case object Piccolo extends InstrumentName
    case object Flute extends InstrumentName
    case object Recorder extends InstrumentName
    case object PanFlute extends InstrumentName
    case object BlownBottle extends InstrumentName
    case object Shakuhachi extends InstrumentName
    case object Whistle extends InstrumentName
    case object Ocarina extends InstrumentName
    case object Lead1Square extends InstrumentName
    case object Lead2Sawtooth extends InstrumentName
    case object Lead3Calliope extends InstrumentName
    case object Lead4Chiff extends InstrumentName
    case object Lead5Charang extends InstrumentName
    case object Lead6Voice extends InstrumentName
    case object Lead7Fifths extends InstrumentName
    case object Lead8BassLead extends InstrumentName
    case object Pad1NewAge extends InstrumentName
    case object Pad2Warm extends InstrumentName
    case object Pad3Polysynth extends InstrumentName
    case object Pad4Choir extends InstrumentName
    case object Pad5Bowed extends InstrumentName
    case object Pad6Metallic extends InstrumentName
    case object Pad7Halo extends InstrumentName
    case object Pad8Sweep extends InstrumentName
    case object FX1Train extends InstrumentName
    case object FX2Soundtrack extends InstrumentName
    case object FX3Crystal extends InstrumentName
    case object FX4Atmosphere extends InstrumentName
    case object FX5Brightness extends InstrumentName
    case object FX6Goblins extends InstrumentName
    case object FX7Echoes extends InstrumentName
    case object FX8SciFi extends InstrumentName
    case object Sitar extends InstrumentName
    case object Banjo extends InstrumentName
    case object Shamisen extends InstrumentName
    case object Koto extends InstrumentName
    case object Kalimba extends InstrumentName
    case object Bagpipe extends InstrumentName
    case object Fiddle extends InstrumentName
    case object Shanai extends InstrumentName
    case object TinkleBell extends InstrumentName
    case object Agogo extends InstrumentName
    case object SteelDrums extends InstrumentName
    case object Woodblock extends InstrumentName
    case object TaikoDrum extends InstrumentName
    case object MelodicDrum extends InstrumentName
    case object SynthDrum extends InstrumentName
    case object ReverseCymbal extends InstrumentName
    case object GuitarFretNoise extends InstrumentName
    case object BreathNoise extends InstrumentName
    case object Seashore extends InstrumentName
    case object BirdTweet extends InstrumentName
    case object TelephoneRing extends InstrumentName
    case object Helicopter extends InstrumentName
    case object Applause extends InstrumentName
    case object Gunshot  extends InstrumentName
    case object Percussion extends InstrumentName
    case class Custom(s: String) extends InstrumentName
  }

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
  def rest[A](d: Dur): Music[A] = Prim(Rest(d))
  def tempo[A](r: Dur, m: Music[A]): Music[A] = Modify(Control.Tempo(r), m)

  def cff(o: Octave, d: Dur) = note(d, (Cff,  o))
  def cf(o: Octave, d: Dur) = note(d, (Cf,   o))
  def c(o: Octave, d: Dur) = note(d, (C,    o))
  def cs(o: Octave, d: Dur) = note(d, (Cs,   o))
  def css(o: Octave, d: Dur) = note(d, (Css,  o))
  def dff(o: Octave, d: Dur) = note(d, (Dff,  o))
  def df(o: Octave, d: Dur) = note(d, (Df,   o))
  def d(o: Octave, d: Dur) = note(d, (D,    o))
  def ds(o: Octave, d: Dur) = note(d, (Ds,   o))
  def dss(o: Octave, d: Dur) = note(d, (Dss,  o))
  def eff(o: Octave, d: Dur) = note(d, (Eff,  o))
  def ef(o: Octave, d: Dur) = note(d, (Ef,   o))
  def e(o: Octave, d: Dur) = note(d, (E,    o))
  def es(o: Octave, d: Dur) = note(d, (Es,   o))
  def ess(o: Octave, d: Dur) = note(d, (Ess,  o))
  def fff(o: Octave, d: Dur) = note(d, (Fff,  o))
  def ff(o: Octave, d: Dur) = note(d, (Ff,   o))
  def f(o: Octave, d: Dur) = note(d, (F,    o))
  def fs(o: Octave, d: Dur) = note(d, (Fs,   o))
  def fss(o: Octave, d: Dur) = note(d, (Fss,  o))
  def gff(o: Octave, d: Dur) = note(d, (Gff,  o))
  def gf(o: Octave, d: Dur) = note(d, (Gf,   o))
  def g(o: Octave, d: Dur) = note(d, (G,    o))
  def gs(o: Octave, d: Dur) = note(d, (Gs,   o))
  def gss(o: Octave, d: Dur) = note(d, (Gss,  o))
  def aff(o: Octave, d: Dur) = note(d, (Aff,  o))
  def af(o: Octave, d: Dur) = note(d, (Af,   o))
  def a(o: Octave, d: Dur) = note(d, (A,    o))
  def as(o: Octave, d: Dur) = note(d, (As,   o))
  def ass(o: Octave, d: Dur) = note(d, (Ass,  o))
  def bff(o: Octave, d: Dur) = note(d, (Bff,  o))
  def bf(o: Octave, d: Dur) = note(d, (Bf,   o))
  def b(o: Octave, d: Dur) = note(d, (B,    o))
  def bs(o: Octave, d: Dur) = note(d, (Bs,   o))
  def bss(o: Octave, d: Dur) = note(d, (Bss,  o))
  
  val bn: Dur = 2
  val wn: Dur = 1
  val hn: Dur = wn/2
  val qn: Dur = wn/4
  val en: Dur = wn/8
  val sn: Dur = wn/16
  val tn: Dur = wn/32
  val sfn: Dur = wn/64

  val dwn: Dur = wn*3/2
  val dhn: Dur = wn*3/4
  val dqn: Dur = wn*3/8

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