import euterpea.music.note.Music._
import euterpea.music.note.MoreMusic._
import euterpea.io.midi.ToMidi._
import euterpea.music.note.Performance.Performable

object Demo extends App {
  val o: Int = 6
  val pitches = List((Fs, o), (As, o), (Cs, o+1), (Fs, o+1))
  val chord = pitches.map(p => note(1.0, p)).reduceLeft(:=:(_, _))
  println(chord)
  //test(chord).unsafePerformIO()
  val sonataInC = line(List(c(5, wn), e(5, hn), g(5, hn), b(4, dhn), c(5, en), d(5, en), c(5, hn), rest(hn),
  a(5, wn), g(5, hn), c(6, hn), g(5, hn), f(5, en), g(5, en), e(5, en), f(5, en), e(5, hn), rest(hn),
  a(4, qn), b(4, en), c(5, en), d(5, en), e(5, en), f(5, en), g(5, en), a(5, en),
  g(5, en), f(5, en), e(5, en), d(5, en), c(5, en), b(4, en), a(4, en),
  g(4, qn), a(4, en), b(4, en), c(5, en), d(5, en), e(5, en), f(5, en), g(5, en),
  f(5, en), e(5, en), d(5, en), c(5, en), b(4, en), a(4, en), g(4, en),
  f(4, qn), g(4, en), a(4, en), b(4, en), c(5, en), d(5, en), e(5, en), f(5, en),
  e(5, en), d(5, en), c(5, en), b(4, en), a(4, en), g(4, en), f(4, en),
  e(4, qn), f(4, en), g(4, en), a(4, en), b(4, en), c(5, en), d(5, en), e(5, en),
  d(5, en), c(5, en), b(4, en), a(4, en), g(4, en), f(4, en), e(4, en),
  d(4, qn), e(4, en), f(4, en), g(4, en), a(4, en), b(4, en), cs(5, en),
  d(5, en), a(4, en), b(4, en), cs(5, en), d(5, en), e(5, en), f(5, en), g(5, en),
  a(5, en), b(5, en), c(6, en), b(5, en), a(5, en), g(5, en), f(5, en), e(5, en),
  f(5, en), g(5, en), a(5, en), g(5, en), f(5, en), e(5, en), d(5, en), c(5, en),
  b(4, qn), g(5, qn), e(5, qn), c(5, qn), d(5, qn), g(5, qn), e(5, qn), c(5, qn),
  d(5, hn), g(5, hn), g(4, hn), rest(hn),
  fs(4, en), g(4, en), fs(4, en), g(4, en), fs(4, en), g(4, en), fs(4, en), g(4, en),
  f(4, en), g(4, en), f(4, en), g(4, en), f(4, en), g(4, en), f(4, en), g(4, en),
  g(5, qn), e(5, qn), c(5, dhn), d(5, en), e(5, en), d(5, qn), c(5, qn),
  c(5, dqn), b(4, en), b(4, hn), rest(wn), g(5, qn), e(5, qn), c(5, dhn),
  d(5, en), e(5, en), d(5, qn), c(5, qn), c(5, dqn), b(4, en), b(4, hn), rest(wn),
  g(5, en), e(3, en),g(3, en), c(4, en), e(4, en), g(5, en), e(5, en), c(5, en),
  a(4, en), f(3, en), a(3, en), c(4, en), f(4, en), a(4, en), c(5, en), a(4, en),
  f(5, en), d(3, en), f(3, en), b(3, en), d(4, en), f(5, en), d(5, en), b(4, en),
  g(4, en), e(3, en), g(3, en), b(3, en), e(4, en), g(4, en), b(4, en), g(4, en),
  e(5, en), c(4, en), e(4, en), a(4, en), c(5, en), e(5, en), c(5, en), a(4, en),
  f(4, en), d(4, en), f(4, en), a(4, en), d(5, en), f(4, en), a(4, en), f(4, en),
  d(6, en), b(3, en), d(4, en), g(4, en), b(4, en), d(6, en), b(5, en), g(5, en),
  e(5, en), c(4, en), e(4, en), g(4, en), c(5, en), c(6, en), g(5, en), e(5, en),
  d(5, wn), d(5, hn), d(5, hn), a(5, wn), a(5, hn), a(5, hn), g(5, qn), a(5, en),
  b(5, en), c(6, en), d(6, en), e(6, en), d(6, en), c(6, en), b(5, en), a(5, en),
  g(5, en), f(5, en), e(5, en), d(5, en), c(5, en), e(5, en), d(5, en), e(5, en),
  d(5, en), e(5, en), d(5, en), e(5, en), d(5, en), e(5, en), d(5, en), e(5, en),
  d(5, en), e(5, en), d(5, en), c(5, en), d(5, en), c(5, hn), c(5, en), g(4, en),
  c(5, en), e(5, en), g(5, en), e(5, en), c(5, en), e(5, en), f(5, en), d(5, en),
  b(4, en), d(5, en), c(5, hn), c(4, en), g(3, en), c(4, en), e(4, en), g(4, en),
  e(4, en), c(4, en), e(4, en), f(4, en), d(4, en), b(3, en), d(4, en), c(4, hn),
  c(5, hn), c(4, hn)))
  test(sonataInC).unsafePerformIO()
}
