package scodec.midi

import scodec.midi.Midi._


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
  sealed trait Message
  def fromAbsTime[A: Numeric](track: Track[A]): Track[A] = ???
}