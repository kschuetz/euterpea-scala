package euterpea.io.midi

import euterpea.music.note.Music._
import euterpea.music.note.Performance.{Performance, Performable, defToPerf}
import ExportMidiFile._
import scodec.midi.Midi._

import scalaz.effect.IO

object ToMidi {
  type UserPatchMap = Map[InstrumentName, Channel]
  def makeGMMap(insts: List[InstrumentName]): UserPatchMap = ???
  def toMidi(pf: Performance, upm: UserPatchMap): Midi = {
    val split = splitByInst(pf)
    val insts = split.map(_._1)
    val rightMap = if (allValid(upm, insts)) upm else makeGMMap(insts)
    Midi(if (split.length == 1) SingleTrack else MultiTrack, TicksPerBeat(division), split.map(p => fromAbsTime(performToMEvs(rightMap, p))))
  }
  val division = 96
  def allValid(upm: UserPatchMap, insts: List[InstrumentName]): Boolean = ???
  def splitByInst(pf: Performance): List[(InstrumentName, Performance)] = ???
  type MEvent = (Ticks, Message)
  val defST = 500000
  def performToMEvs(upm: UserPatchMap, ip: (InstrumentName, Performance)): List[MEvent] = ???
  val defUpm: UserPatchMap = Map(
    (AcousticGrandPiano,1),
    (Vibraphone,2),
    (AcousticBass,3),
    (Flute,4),
    (TenorSax,5),
    (AcousticGuitarSteel,6),
    (Viola,7),
    (StringEnsemble1,8),
    (AcousticGrandPiano,9))
  def testMidi[A: Performable](m: Music[A]): Midi = toMidi(defToPerf(m), defUpm)
  def test[A: Performable](m: Music[A]): IO[Unit] = exportMidiFile("test.mid", testMidi(m))
}
