package scodec.midi

import org.smop.typeclass.ListOps._
import scodec.bits.ByteVector
import scodec.midi.Midi._
import spire.algebra._
import spire.math._
import spire.implicits._

object Midi {
  case class Midi(fileType: FileType, timeDiv: TimeDiv, tracks: List[Track[Ticks]])
  sealed trait FileType
  case object SingleTrack extends FileType
  case object MultiTrack extends FileType
  case object MultiPattern extends FileType
  type Track[A] = List[(A, Message)]
  sealed trait TimeDiv
  case class TicksPerBeat(ticks: Int) extends TimeDiv
  case class TicksPerSecond(framesPerSecond: Int, ticksPerFrame: Int) extends TimeDiv

  type Ticks = Int
  type Channel = Int
  type Key = Int
  type Velocity = Int
  type Pressure = Int
  type Preset = Int
  type Bank = Int
  type PitchWheel = Int
  type Tempo = Int
  sealed trait Message
  object Message {
    case class NoteOff(channel: Channel, key: Key, velocity: Velocity) extends Message
    case class NoteOn(channel: Channel, key: Key, velocity: Velocity) extends Message
    case class KeyPressure(channel: Channel, key: Key, pressure: Pressure) extends Message
    case class ControlChange(channel: Channel, controllerNumber: Int, controllerValue: Int) extends Message
    case class ProgramChange(channel: Channel, preset: Preset) extends Message
    case class ChannelPressure(channel: Channel, pressure: Pressure) extends Message
    case class PitchWheel(channel: Channel, pitchWheel: PitchWheel) extends Message
    // Meta Messages
    case class SequenceNumber(sn: Int) extends Message
    case class Text(t: String) extends Message
    case class Copyright(c: String) extends Message
    case class TrackName(tn: String) extends Message
    case class InstrumentName(in: String) extends Message
    case class Lyrics(l: String) extends Message
    case class Marker(m: String) extends Message
    case class CuePoint(cp: String) extends Message
    case class ChannelPrefix(c: Channel) extends Message
    case class ProgramName(pn: String) extends Message
    case class DeviceName(dn: String) extends Message
    case object TrackEnd extends Message
    case class TempoChange(t: Tempo) extends Message
    case class SMPTEOffset(h: Int, m: Int, s: Int, f: Int, sf: Int) extends Message
    case class TimeSignature(num: Int, den: Int, cpc: Int, npb: Int) extends Message
    case class KeySignature(root: Int, mode: Int) extends Message
    case class Reserved(i: Int, data: ByteVector) extends Message
    // System Exclusive Messages
    case class Sysex(w: Int, bv: ByteVector) extends Message // 0xF0 or 0xF7
  }
  def fromAbsTime[A: Numeric](trk: Track[A]): Track[A] = {
    val (ts, ms) = trk.unzip
    val (_, tsp) = mapAccumL(Numeric[A].zero, ts)((acc, t) => (t, t - acc))
    tsp.zip(ms)
  }
}