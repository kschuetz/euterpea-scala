import euterpea.music.note.Music._
import euterpea.io.midi.ToMidi._
import euterpea.music.note.Performance.Performable

object Demo extends App {
  val o: Int = 6
  val pitches = List((Fs, o), (As, o), (Cs, o+1), (Fs, o+1))
  val chord: Music[Pitch] = pitches.map(p => note(1.0, p)).reduceLeft(:=:(_, _))
  println(chord)
  println(qn)
  test(chord).unsafePerformIO()
}
