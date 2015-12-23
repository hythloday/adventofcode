package advent

import scala.collection.mutable

/**
  * Created by james on 22/12/2015.
  */
object States {
  sealed trait Buff {
    def apply(to: ActorState, playerTurn: Boolean): ActorState
  }
  sealed trait Spell {
    def cost: Int
    def cast(self: ActorState, other: ActorState): (ActorState, ActorState)
  }
  case class Actor(damage: Int)
  case class ActorState(actor: Actor, hp: Int, mana: Int = 0, buffs: Map[Buff, Int] = Map())
}

object Config {
  def debug(s: String) = ()
  def debugln(s: String) = ()

  import States._
  case object ShieldBuff extends Buff {
    override def apply(self: ActorState, playerTurn: Boolean): ActorState = {
      debug("A shield shimmers; ")
      self
    }
  }
  case object HardmodeDebuff extends Buff {
    override def apply(to: ActorState, playerTurn: Boolean): ActorState = if (playerTurn) to.copy(hp = to.hp - 1) else to
  }
  case object PoisonDebuff extends Buff {
    override def apply(to: ActorState, playerTurn: Boolean): ActorState = {
      debug("Poison deals 3 damage; ")
      to.copy(hp = to.hp - 3)
    }
  }
  case object RechargeBuff extends Buff {
    override def apply(to: ActorState, playerTurn: Boolean): ActorState = {
      debug("Recharge provides 101 mana; ")
      to.copy(mana = to.mana + 101)
    }
  }
  case object MagicMissile extends Spell {
    override val cost = 53
    override def cast(self: ActorState, other: ActorState): (ActorState, ActorState) = {
      debugln(s"Player casts Magic Missile, dealing 4 damage.")
      (self.copy(mana = self.mana - cost), other.copy(hp = other.hp - 4))
    }
  }
  case object Drain extends Spell {
    override val cost = 73
    override def cast(self: ActorState, other: ActorState): (ActorState, ActorState) = {
      debugln(s"Player casts Drain, dealing 2 damage, and healing 2 hit points.")
      (self.copy(mana = self.mana - cost, hp = self.hp+2), other.copy(hp = other.hp-2))
    }
  }
  case object Shield extends Spell {
    override val cost: Int = 113
    override def cast(self: ActorState, other: ActorState): (ActorState, ActorState) = {
      debugln(s"Player casts Shield.")
      (self.copy(mana = self.mana - cost, buffs = self.buffs + (ShieldBuff -> 6)), other)
    }
  }
  case object Poison extends Spell {
    override val cost: Int = 173
    override def cast(self: ActorState, other: ActorState): (ActorState, ActorState) = {
      debugln(s"Player casts Poison.")
      (self.copy(mana = self.mana - cost), other.copy(buffs = other.buffs + (PoisonDebuff -> 6)))
    }
  }
  case object Recharge extends Spell {
    override val cost: Int = 229
    override def cast(self: ActorState, other: ActorState): (ActorState, ActorState) = {
      debugln(s"Player casts Recharge.")
      (self.copy(mana = self.mana - cost, buffs = self.buffs + (RechargeBuff -> 5)), other)
    }
  }
  val spells = List(MagicMissile, Drain, Shield, Poison, Recharge)
  def abbrev(s: Spell) = s.getClass.getSimpleName.filter(_.isUpper).head.toString
}

object Day22 extends Advent {
  import States._
  import Config._

  def resolveBuffActions(state: ActorState, playerAndPlayerTurn: Boolean): ActorState = {
    state.buffs.foldLeft(state) { case (actor, (buff, turns)) =>
      val buffedActor = buff.apply(actor, playerAndPlayerTurn)
      debugln(s"its timer is now ${turns - 1}")
      val replacementBuff = if (turns > 1) {
        Map(buff -> (turns - 1))
      } else {
        debugln(s"${buff.getClass.getSimpleName} wears off.")
        Map()
      }
      val tickedBuff = (actor.buffs - buff) ++ replacementBuff
      buffedActor.copy(buffs = tickedBuff)
    }
  }

  def resolveRound(player: ActorState, boss: ActorState, playerAttack: Option[Spell]): (ActorState, ActorState) = {
    if (playerAttack.nonEmpty) debugln(s"-- Player turn --")
    else debugln(s"-- Boss turn --")
    debugln(s"- Player has ${player.hp} hit points, ${player.buffs.get(ShieldBuff).map(_ => 7).getOrElse(0)} armor, ${player.mana} mana")
    debugln(s"- Boss has ${boss.hp} hit points")
    val (p, b) = (resolveBuffActions(player, playerAttack.isDefined), resolveBuffActions(boss, false))
    if (winner(p, b, playerAttack).isDefined) (p, b)
    else if (playerAttack.nonEmpty) {
      playerAttack.get.cast(p, b)
    }
    else {
      if (p.buffs.keys.toList contains ShieldBuff) {
        val dmg = Math.max(boss.actor.damage - 7, 1)
        debugln(s"Boss attacks for $dmg damage")
        (p.copy(hp = p.hp - dmg), b)
      } else {
        debugln(s"Boss attacks for ${boss.actor.damage} damage")
        (p.copy(hp = p.hp - b.actor.damage), b)
      }
    }
  }

  def winner(player: ActorState, boss: ActorState, playerAttack: Option[Spell]): Option[Actor] =
    if (boss.hp <= 0) {
      debugln("Boss dies")
      Some(player.actor)
    }
    else if (player.hp <= 0) {
      debugln("Player dies")
      Some(boss.actor)
    }
    else if (playerAttack.isDefined && player.mana <= spells.map(_.cost).min) {
      debugln("Player runs out of mana")
      Some(boss.actor)
    }
    else if (playerAttack.isDefined && player.mana <= playerAttack.get.cost) {
      debugln("Player can't cast spell")
      Some(boss.actor)
    }
    else None

  object P extends ActorState(Actor(0), hp=50, mana=500)
  object B extends ActorState(Actor(9), hp=51, 0)

  case class Fight(player: ActorState, boss: ActorState, rounds: List[Option[Spell]])

  implicit object FightOrder extends Ordering[Fight] {
    override def compare(x: Fight, y: Fight): Int = y.rounds.flatten.map(_.cost).sum.compare(x.rounds.flatten.map(_.cost).sum)
  }

  def printFightHistory(fight: Fight) =
    s"${fight.rounds.flatten.reverse.map(abbrev).mkString}: ⚔${fight.rounds.flatten.map(_.cost).sum}"

  def cheapestWinningSequence(firstRounds: List[Fight]): Fight = {
    val fightQ = mutable.PriorityQueue[Fight]()
    firstRounds.foreach(fightQ.+=)

    var loop = true
    var fight: Option[Fight] = None
    while (loop) {
      val f = fightQ.dequeue()
      val rv = resolveRound(f.player, f.boss, f.rounds.head)
      rv match {
        case (player, _) if player.hp <= 0 => ()
        case (_, boss) if boss.hp <= 0 =>
          fight = Some(f)
          loop = false
        case (player, boss) if f.rounds.head.isEmpty =>
          val choices = spells.filter {
            case Poison if boss.buffs.get(PoisonDebuff).exists(_ > 1) => false
            case Recharge if player.buffs.get(RechargeBuff).exists(_ > 1) => false
            case Shield if player.buffs.get(ShieldBuff).exists(_ > 1) => false
            case _ => true
          }
          choices.map(Some.apply).map(choice => Fight(player, boss, choice :: f.rounds)).foreach(fightQ.+=)
        case (player, boss) => fightQ += Fight(player, boss, None :: f.rounds)
      }
    }
    fight.get
  }

  def part1 = printFightHistory(cheapestWinningSequence(spells.map(s => Fight(P, B, List(Some(s))))))
  def part2 = printFightHistory(cheapestWinningSequence(spells.map(s => Fight(P.copy(buffs = Map(HardmodeDebuff -> 1000)), B, List(Some(s))))))
}
