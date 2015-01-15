package euterpea.io.midi

import java.nio.file.{Paths, Files}

import scodec.bits._
import scodec.midi.Midi._

import scalaz.effect._
import scalaz.std.effect.closeable._

object ExportMidiFile {
  def makeFile(midi: Midi): ByteVector = {
    val ticksPerQn = midi.timeDiv match {
      case TicksPerBeat(x) => x
      case TicksPerSecond(_, _) => sys.error("(makeFile) Don't know how to handle TicksPerSecond yet.")
    }
    val header = makeHeader(midi.fileType, midi.tracks.length, ticksPerQn)
    val body = ByteVector.concat(midi.tracks.map(makeTrack))
    header ++ body
  }
  type TrackCount = Int
  type TicksPerQN = Int
  def makeHeader(ft: FileType, numTracks: TrackCount, ticksPerQn: TicksPerQN): ByteVector = ???
  def makeTrack(t: Track[Ticks]): ByteVector = ???
  def exportMidiFile(filePath: String, midi: Midi): IO[Unit] = writeFile(filePath, makeFile(midi))
  def writeFile(fileName: String, bv: ByteVector): IO[Unit] = IO {
    Files.newOutputStream(Paths.get(fileName))
  }.using(os => IO(bv.copyToStream(os)))
}
