package euterpea.music.note

import Music._
import MoreMusic._
import euterpea.music.note.MoreMusic.{NoteAttribute => NA}
import euterpea.music.note.MoreMusic.Volume
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
  type PMap[A] = PlayerName => Player[A]
  type Note1 = (Pitch, List[NoteAttribute])
  sealed trait Player[A]
  case class MkPlayer[A](pName: PlayerName,
                         playNote: NoteFun[A],
                         interpPhrase: PhraseFun[A],
                         notatePlayer: NotateFun[A] = ()) extends Player[A]
  type NoteFun[A] = Context[A] => Dur => A => Performance
  type PhraseFun[A] = (PMap[A], Context[A], List[PhraseAttribute], Music[A]) => (Performance, DurT)
  type NotateFun[A] = Unit
  //val defPlayer: Player[Note1] = MkPlayer("Default", defPlayNote(defNasHandler), defInterpPhrase(defPasHandler))
  def defPlayNote[A](nasHandler: Context[(Pitch, List[A])] => A => Event => Event): NoteFun[(Pitch, List[A])] = {
    c: Context[(Pitch, List[A])] => d: Dur => a: (Pitch, List[A]) => {
      val (p, nas) = a
      val Context(cTime, cPlayer, cInst, cDur, cPch, cVol, cKey) = c
      val initEv = Event(cTime, cInst, absPitch(p) + cPch, /* d * cDur */ d, cVol, Nil)
      List(nas.foldRight(initEv)((a, ev) => nasHandler(c)(a)(ev)))
    }
  }
  def defNasHandler[A](c: Context[A])(na: NoteAttribute, ev: Event): Event = na match {
    case NA.Volume(v) => ev.copy(eVol = v)
    case NA.Params(pms) => ev.copy(eParams = pms)
    case _ => ev
  }

  trait Performable[A] {
    def perfDur(pm: PMap[Note1], c: Context[Note1], m: Music[A]): (Performance, DurT)
  }
  def defToPerf[A: Performable](m: Music[A]): Performance = ???
}
