package euterpea.io.midi

import java.nio.file.{Paths, Files}

import scodec.bits._
import scodec.codecs._
import scodec.midi.Midi.Message._
import scodec.midi.Midi._

import scalaz.effect._
import scalaz.std.effect.closeable._

object ExportMidiFile {
  def makeFile(midi: Midi): ByteVector = {
    println(midi)
    val ticksPerQn = midi.timeDiv match {
      case TicksPerBeat(x) => x
      case TicksPerSecond(_, _) => sys.error("(makeFile) Don't know how to handle TicksPerSecond yet.")
    }
    val header = makeHeader(midi.fileType, midi.tracks.length, ticksPerQn)
    val body = ByteVector.concat(midi.tracks.map(makeTrack))
    header ++ body
  }
  val midiHeaderConst = ascii.encodeValid("MThd") ++ uint32.encodeValid(6L)
  type TrackCount = Int
  type TicksPerQN = Int
  def makeHeader(ft: FileType, numTracks: TrackCount, ticksPerQn: TicksPerQN): ByteVector = {
    val ftp = ft match {
      case SingleTrack => hex"0000".bits
      case MultiTrack => hex"0001".bits
      case MultiPattern => sys.error("(makeHeader) don't know how to handle multi-pattern yet.")
    }
    val numTracksp = uint16.encodeValid(numTracks)
    val ticksPerQNp = uint16.encodeValid(ticksPerQn)
    (midiHeaderConst ++ ftp ++ numTracksp ++ ticksPerQNp).toByteVector
  }
  def makeTrack(t: Track[Ticks]): ByteVector = {
    val body = makeTrackBody(t)
    val header = makeTrackHeader(body)
    header ++ body
  }
  val trackHeaderConst = ascii.encodeValid("MTrk")
  def makeTrackHeader(tBody: ByteVector): ByteVector = {
    (trackHeaderConst ++ uint32.encodeValid(tBody.length)).toByteVector
  }
  def makeTrackBody(t: Track[Ticks]): ByteVector = t match {
    case Nil => endOfTrack
    case (ticks, msg) :: rest =>
      val b = msgToBytes(msg)
      if (b.length > 0) {
        val bp = List(to7Bits(ticks), b, makeTrackBody(rest))
        ByteVector.concat(bp)
      }
      else
        makeTrackBody(rest)
  }
  val endOfTrack = to7Bits(96) ++ hex"FF2F00"
  def to7Bits(i: Long): ByteVector = {
    if (i == 0L)
      hex"00"
    else {
      val bits = int64.encodeValid(i)
      val trimmed = bits.drop(bits.indexOfSlice(BitVector.one))
      val length = trimmed.length
      val paddedLength = ((length+6) / 7)*7
      val padded = trimmed.padLeft(paddedLength)
      val vec = padded.grouped(7).map(BitVector.one ++ _).toVector
      val (bytes, Vector(last)) = vec.splitAt(vec.length - 1)
      BitVector.concat(bytes :+ last.clear(0)).toByteVector
    }
  }
  def msgToBytes(m: Message): ByteVector = m match {
    case NoteOn(c, k, v) => ((hex"90".bits | uint8.encodeValid(c)) ++ uint8.encodeValid(k) ++ uint8.encodeValid(v)).toByteVector
    case NoteOff(c, k, v) => ((hex"80".bits | uint8.encodeValid(c)) ++ uint8.encodeValid(k) ++ uint8.encodeValid(v)).toByteVector
    case ProgramChange(c, p) => ((hex"c0".bits | uint8.encodeValid(c)) ++ uint8.encodeValid(p)).toByteVector
    case ControlChange(c, n, v) => ((hex"b0".bits | uint8.encodeValid(c)) ++ uint8.encodeValid(n) ++ uint8.encodeValid(v)).toByteVector
    case TempoChange(t) => (hex"ff5103".bits ++ uint24.encodeValid(t)).toByteVector
  }
  def exportMidiFile(filePath: String, midi: Midi): IO[Unit] = writeFile(filePath, makeFile(midi))
  def writeFile(fileName: String, bv: ByteVector): IO[Unit] = IO {
    Files.newOutputStream(Paths.get(fileName))
  }.using(os => IO(bv.copyToStream(os)))
}
