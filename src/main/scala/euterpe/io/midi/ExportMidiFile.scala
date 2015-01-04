package euterpe.io.midi

import scodec.midi.Midi.Midi

import scalaz.effect.IO

object ExportMidiFile {
  def exportMidiFile(filePath: String, midi: Midi): IO[Unit] = ???
}
