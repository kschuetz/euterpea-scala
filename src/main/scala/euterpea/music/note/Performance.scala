package euterpea.music.note

import Music._
import MoreMusic._
import euterpea.music.note.MoreMusic.{NoteAttribute => NA}
import euterpea.music.note.MoreMusic.Volume
import euterpea.music.note.Music.{Control => Ctrl}
import spire.math.Rational

object Performance {
  type Performance = List[Event]
  case class Event( eTime: PTime,
                    eInst: InstrumentName,
                    ePitch: AbsPitch,
                    eDur: DurT,
                    eVol: Volume,
                    eParams: List[Double])
  type PTime = Rational
  type DurT = Rational
  case class Context[A]( cTime: PTime,
                         cPlayer: Player[A],
                         cInst: InstrumentName,
                         cDur: DurT,
                         cPch: AbsPitch,
                         cVol: Volume,
                         cKey: (PitchClass, Mode))
  def metro(setting: Int, dur: Dur): DurT = 60 / (setting * dur)
  type PMap[A] = PlayerName => Player[A]
  def merge(p1: Performance, p2: Performance): Performance = (p1, p2) match {
    case (Nil, es2) => es2
    case (es1, Nil) => es1
    case (a@(e1::es1), b@(e2::es2)) =>
      if (e1.eTime < e2.eTime)
        e1 :: merge(es1, b)
      else
        e2 :: merge(a, es2)
  }
  def perf[A](pm: PMap[A], c: Context[A], m: Music[A]): (Performance, DurT) = {
    val Context(t, pl, _, dt, k, _, _) = c
    m match {
      case Prim(Note(d, p)) => (pl.playNote(c)(d)(p), d*dt)
      case Prim(Rest(d)) => (Nil, d*dt)
      case :+:(m1, m2) =>
        val (pf1, d1) = perf(pm, c, m1)
        val (pf2, d2) = perf(pm, c.copy(cTime = t+d1), m2)
        (pf1 ++ pf2, d1+d2)
      case :=:(m1, m2) =>
        val (pf1, d1) = perf(pm, c, m1)
        val (pf2, d2) = perf(pm, c, m2)
        (merge(pf1, pf2), d1.max(d2))
      case Modify(Ctrl.Tempo(r), m) => perf(pm, c.copy(cDur = dt / r), m)
      case Modify(Ctrl.Transpose(p), m) => perf(pm, c.copy(cPch = k + p), m)
      case Modify(Ctrl.Instrument(i), m) => perf(pm, c.copy(cInst = i), m)
      case Modify(Ctrl.KeySig(pc, mo), m) => perf(pm, c.copy(cKey = (pc, mo)), m)
      case Modify(Ctrl.Player(pn), m) => perf(pm, c.copy(cPlayer = pm(pn)), m)
      case Modify(Ctrl.Phrase(pas), m) => pl.interpPhrase(pm, c, pas, m)
    }
  }
  type Note1 = (Pitch, List[NoteAttribute])
  case class Player[A](pName: PlayerName,
                         playNote: NoteFun[A],
                         interpPhrase: PhraseFun[A],
                         notatePlayer: NotateFun[A] = false)
  type NoteFun[A] = Context[A] => Dur => A => Performance
  type PhraseFun[A] = (PMap[A], Context[A], List[PhraseAttribute], Music[A]) => (Performance, DurT)
  type NotateFun[A] = Boolean
  val defPlayer: Player[Note1] = Player("Default", defPlayNote(defNasHandler), defInterpPhrase(defPasHandler))
  def defPlayNote[A](nasHandler: Context[(Pitch, List[A])] => (A, Event) => Event): NoteFun[(Pitch, List[A])] = {
    c: Context[(Pitch, List[A])] => d: Dur => a: (Pitch, List[A]) => {
      val (p, nas) = a
      val Context(cTime, cPlayer, cInst, cDur, cPch, cVol, cKey) = c
      val initEv = Event(cTime, cInst, absPitch(p) + cPch, d * cDur, cVol, Nil)
      List(nas.foldRight(initEv)(nasHandler(c)))
    }
  }
  def defNasHandler[A](c: Context[A])(na: NoteAttribute, ev: Event): Event = na match {
    case NA.Volume(v) => ev.copy(eVol = v)
    case NA.Params(pms) => ev.copy(eParams = pms)
    case _ => ev
  }
  def defInterpPhrase[A](pasHandler: (PhraseAttribute, Performance) => Performance): PhraseFun[A] = {
    (pm: PMap[A], context: Context[A], pas: List[PhraseAttribute], m: Music[A]) => {
      val (pf, dur) = perf(pm, context, m)
      (pas.foldRight(pf)(pasHandler), dur)
    }
  }
  def defPasHandler(pa: PhraseAttribute, p: Performance): Performance = pa match {
    case Dyn(Dynamic.Accent(x)) => p.map(e=>e.copy(eVol=(x * e.eVol).round.toInt))
    case Art(Staccato(x)) => p.map(e=>e.copy(eDur = x * e.eDur))
    case Art(Legato(x)) => p.map(e=>e.copy(eDur = x * e.eDur))
    case _ => p
  }
  val fancyPlayer: Player[(Pitch, List[NoteAttribute])] = Player(
    pName = "Fancy",
    playNote = defPlayNote(defNasHandler),
    interpPhrase = fancyInterpPhrase,
    notatePlayer = false
  )
  val defCon: Context[Note1] = Context(
    cTime = 0,
    cPlayer = fancyPlayer,
    cInst = AcousticGrandPiano,
    cDur = metro(120, qn),
    cPch = 0,
    cKey = (C, Major),
    cVol = 127
  )
  def fancyInterpPhrase[A](pm: PMap[A], c: Context[A], pass: List[PhraseAttribute], m: Music[A]): (Performance, DurT) = {
    if (pass.isEmpty)
      perf(pm, c, m)
    else {
      val Context(t, pl, i, dt, k, v, _) = c
      val (pa :: pas) = pass
      val pfd@(pf, dur) = fancyInterpPhrase(pm, c, pas, m)
      def loud(x: Rational) = fancyInterpPhrase(pm, c, Dyn(Dynamic.Loudness(x)) :: pas, m)
      def stretch(x: Rational) = {
        val t0 = pf.head.eTime
        val r = x / dur
        def upd(e: Event): Event = {
          val Event(t, _, _, d, _, _) = e
          val dt = t-t0
          val tp = (1+dt*r)*dt+t0
          val dp = (1+(2*dt+d)*r)*d
          e.copy(eTime = tp, eDur = dp)
        }
        (pf.map(upd), (1+x)*dur)
      }
      def inflate(x: Rational) = {
        val t0 = pf.head.eTime
        val r = x/dur
        def upd(e: Event): Event = {
          val Event(t, _, _, _, v, _) = e
          e.copy(eVol = ((1+(t-t0)*r)*v).round.toInt)
        }
        (pf.map(upd), (1+x)*dur)
      }
      pa match {
        case Dyn(Dynamic.Accent(x)) => (pf.map(e => e.copy(eVol=(x*e.eVol).round.toInt)), dur)
      }
    }
  }
  trait Performable[A] {
    def perfDur(pm: PMap[Note1], c: Context[Note1], m: Music[A]): (Performance, DurT)
  }
  object Performable {
    implicit val performablePitchVolume = new Performable[Pitch] {
      override def perfDur(pm: PMap[Note1], c: Context[Note1], m: Music[Pitch]): (Performance, DurT) = ???
    }
  }
  def defToPerf[A: Performable](m: Music[A]): Performance = ???
}
