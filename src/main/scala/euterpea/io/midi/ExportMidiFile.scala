package euterpea.io.midi

import java.nio.file.{Paths, Files}

import scodec.{Err, Attempt}
import scodec.bits._
import scodec.codecs._
import scodec.midi.Midi.Message._
import scodec.midi.Midi._

import scalaz.effect._
import scalaz.std.effect.closeable._

object ExportMidiFile {
  def makeFile(midi: Midi): Attempt[ByteVector] = {
    println(midi)
    val ticksPerQn = midi.timeDiv match {
      case TicksPerBeat(x) => x
      case TicksPerSecond(_, _) => sys.error("(makeFile) Don't know how to handle TicksPerSecond yet.")
    }
    val header = makeHeader(midi.fileType, midi.tracks.length, ticksPerQn)
    val body = midi.tracks.foldLeft(Attempt.successful(List.empty[ByteVector])) {
      case (Attempt.Successful(res), track) => makeTrack(track).map(_ :: res)
      case (failure, _) => failure
    } map { t =>
      ByteVector.concat(t.reverse)
    }

    for {
      h <- header
      b <- body
    } yield h ++ b
  }
  val midiHeaderConst = for {
    h1 <- ascii.encode("MThd")
    h2 <- uint32.encode(6L)
  } yield h1 ++ h2

  type TrackCount = Int
  type TicksPerQN = Int
  def makeHeader(ft: FileType, numTracks: TrackCount, ticksPerQn: TicksPerQN): Attempt[ByteVector] = {
    val ftp = ft match {
      case SingleTrack => hex"0000".bits
      case MultiTrack => hex"0001".bits
      case MultiPattern => sys.error("(makeHeader) don't know how to handle multi-pattern yet.")
    }
    for {
      hc <- midiHeaderConst
      numTracksp <- uint16.encode(numTracks)
      ticksPerQNp <- uint16.encode(ticksPerQn)
    } yield {
      (hc ++ ftp ++ numTracksp ++ ticksPerQNp).toByteVector
    }
  }

  def makeTrack(t: Track[Ticks]): Attempt[ByteVector] = for {
    body <- makeTrackBody(t)
    header <- makeTrackHeader(body)
  } yield header ++ body

  val trackHeaderConst = ascii.encode("MTrk")
  def makeTrackHeader(tBody: ByteVector): Attempt[ByteVector] = for {
    thc <- trackHeaderConst
    len <- uint32.encode(tBody.length)
  } yield (thc ++ len).toByteVector

  def makeTrackBody(t: Track[Ticks]): Attempt[ByteVector] = t match {
    case Nil => endOfTrack
    case (ticks, msg) :: rest =>
      msgToBytes(msg).flatMap { b =>
        if (b.length > 0) {
          for {
            t <- to7Bits(ticks)
            more <- makeTrackBody(rest)
          } yield ByteVector.concat(t :: b :: more :: Nil)
        }
        else
          makeTrackBody(rest)
      }
  }

  val endOfTrack = to7Bits(96).map(_ ++ hex"FF2F00")
  def to7Bits(i: Long): Attempt[ByteVector] = {
    if (i == 0L)
      Attempt.successful(hex"00")
    else {
      int64.encode(i).map { bits =>
        val trimmed = bits.drop(bits.indexOfSlice(BitVector.one))
        val length = trimmed.length
        val paddedLength = ((length+6) / 7)*7
        val padded = trimmed.padLeft(paddedLength)
        val vec = padded.grouped(7).map(BitVector.one ++ _).toVector
        val (bytes, Vector(last)) = vec.splitAt(vec.length - 1)
        BitVector.concat(bytes :+ last.clear(0)).toByteVector
      }
    }
  }

  private val msgNoteOn = hex"90".bits
  private val msgNoteOff = hex"80".bits
  private val msgProgramChange = hex"c0".bits
  private val msgControlChange = hex"b0".bits
  private val msgTempoChange = hex"ff5103".bits

  def msgToBytes(m: Message): Attempt[ByteVector] = m match {
    case NoteOn(c, k, v) => for {
      cb <- uint8.encode(c)
      kb <- uint8.encode(k)
      vb <- uint8.encode(v)
    } yield ((msgNoteOn | cb) ++ kb ++ vb).toByteVector

    case NoteOff(c, k, v) => for {
      cb <- uint8.encode(c)
      kb <- uint8.encode(k)
      vb <- uint8.encode(v)
    } yield ((msgNoteOff | cb) ++ kb ++ vb).toByteVector

    case ProgramChange(c, p) => for {
      cb <- uint8.encode(c)
      pb <- uint8.encode(p)
    } yield ((msgProgramChange | cb) ++ pb).toByteVector

    case ControlChange(c, n, v) => for {
      cb <- uint8.encode(c)
      nb <- uint8.encode(n)
      vb <- uint8.encode(v)
    } yield ((msgNoteOff | cb) ++ nb ++ vb).toByteVector

    case TempoChange(t) => for {
      tb <- uint24.encode(t)
    } yield (msgTempoChange ++ tb).toByteVector

    case other => Attempt.failure(Err(s"Message unsupported: $other"))
  }
  def exportMidiFile(filePath: String, midi: Midi): IO[Unit] = writeFile(filePath, makeFile(midi).require)
  def writeFile(fileName: String, bv: ByteVector): IO[Unit] = IO {
    Files.newOutputStream(Paths.get(fileName))
  }.using(os => IO(bv.copyToStream(os)))
}
