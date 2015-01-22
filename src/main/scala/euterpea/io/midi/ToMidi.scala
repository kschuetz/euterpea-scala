package euterpea.io.midi

import euterpea.music.note.Music._
import euterpea.music.note.Performance._
import GeneralMidi._
import ExportMidiFile._
import scodec.midi.Midi.Message.{NoteOff, NoteOn, TempoChange, ProgramChange}
import scodec.midi.Midi._

import scalaz.effect.IO

object ToMidi {
  type ProgNum = Int
  type UserPatchMap = Map[InstrumentName, Channel]
  def makeGMMap(insts: List[InstrumentName]): UserPatchMap = ???
  def upmLookup(upm: UserPatchMap, iName: InstrumentName): (Channel, ProgNum) = {
    val chan = upm.getOrElse(iName, sys.error(s"instrument $iName not in patch map"))
    (chan, toGM(iName))
  }
  def toMidi(pf: Performance, upm: UserPatchMap): Midi = {
    val split = splitByInst(pf)
    val insts = split.map(_._1)
    val rightMap = if (allValid(upm, insts)) upm else makeGMMap(insts)
    Midi(if (split.length == 1) SingleTrack else MultiTrack, TicksPerBeat(division), split.map(p => fromAbsTime(performToMEvs(rightMap, p))))
  }
  val division = 96
  def allValid(upm: UserPatchMap, insts: List[InstrumentName]): Boolean = insts.forall(lookupB(upm))
  def lookupB(upm: UserPatchMap)(x: InstrumentName): Boolean = upm.isDefinedAt(x)
  def splitByInst(pf: Performance): List[(InstrumentName, Performance)] = pf match {
    case Nil => Nil
    case h :: t =>
      val i = h.eInst
      val (pf1, pf2) = pf.partition(_.eInst == i)
      (i, pf1) :: splitByInst(pf2)
  }
  type MEvent = (Ticks, Message)
  val defST = 500000
  def performToMEvs(upm: UserPatchMap, ip: (InstrumentName, Performance)): List[MEvent] = {
    val (inm, pf) = ip
    val (chan, progNum) = upmLookup(upm, inm)
    val setupInst = (0, ProgramChange(chan, progNum))
    val setTempo = (0, TempoChange(defST))
    def loop(events: List[Event]): List[MEvent] = events match {
      case Nil => Nil
      case e :: es =>
        val (mev1, mev2) = mkMEvents(chan, e)
        mev1 :: insertMEvent(mev2, loop(es))
    }
    setupInst :: setTempo :: loop(pf)
  }
  def mkMEvents(mChan: Channel, event: Event): (MEvent, MEvent) = {
    val Event(t, _, p, d, v, _) = event
    val vp = 0.max(127.min(v))
    ((toDelta(t), NoteOn(mChan, p, vp)), (toDelta(t+d), NoteOff(mChan, p, vp)))
  }
  def toDelta(t: PTime): Ticks = (t * 2 * division).round.toInt
  def insertMEvent(mev1: MEvent, mevs: List[MEvent]): List[MEvent] = mevs match {
    case Nil => List(mev1)
    case (mev2@(t2, _)) :: mevsp =>
      if (mev1._1 <= t2)
        mev1 :: mevs
      else
        mev2 :: insertMEvent(mev1, mevsp)
  }
  val defUpm: UserPatchMap = {
    import InstrumentName._
    Map(
      (AcousticGrandPiano,1),
      (Vibraphone,2),
      (AcousticBass,3),
      (Flute,4),
      (TenorSax,5),
      (AcousticGuitarSteel,6),
      (Viola,7),
      (StringEnsemble1,8),
      (Percussion,9))
  }
  def testMidi[A: Performable](m: Music[A]): Midi = toMidi(defToPerf(m), defUpm)
  def test[A: Performable](m: Music[A]): IO[Unit] = exportMidiFile("test.mid", testMidi(m))
}
