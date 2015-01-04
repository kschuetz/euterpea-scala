package euterpe.io.midi

import euterpea.music.note.Music._
import euterpea.music.note.Performance.Performable
import ExportMidiFile._
import scodec.midi.Midi._

import scalaz.effect.IO

object ToMidi {
  type UserPatchMap = Map[InstrumentName, Channel]
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
  def testMidi[A: Performable](m: Music[A]): Midi = ???
  def test[A: Performable](m: Music[A]): IO[Unit] = exportMidiFile("test.mid", testMidi(m))
}
