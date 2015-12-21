package advent

import scala.annotation.tailrec

/**
  * Created by james on 21/12/2015.
  */
object Day21 extends Advent {

  case class Item(name: String, cost: Int, damage: Int, armour: Int) {
    def +(other: Item) = Item(s"$name/${other.name}", cost+other.cost, damage+other.damage, armour+other.armour)
  }

  case class Actor(hp: Int, damage: Int, armour: Int)

  lazy val weapons = Set(Item("Dagger", 8, 4, 0), Item("Shortsword", 10, 5, 0),
    Item("Warhammer", 25, 6, 0), Item("Longsword", 40, 7, 0), Item("Greataxe", 74, 8, 0))
  lazy val armour = Set( Item("Leather", 13, 0, 1), Item("Chainmail", 31, 0, 2),
    Item("Splintmail", 53, 0, 3), Item("Bandedmail", 75, 0, 4), Item("Platemail", 102, 0, 5))
  lazy val rings = Set( Item("Damage +1",  25, 1, 0), Item("Damage +2",  50, 2, 0), Item("Damage +3",  100, 3, 0),
    Item("Defense +1",  20, 0, 1), Item("Defense +2",  40, 0, 2), Item("Defense +3",  80, 0, 3))
  lazy val empty = Item("Empty", 0, 0, 0)

  def purchases = for {
    weapon <- weapons
    armour <- armour.map(Option.apply) + None
    ring1 <- rings.map(Option.apply) + None
    ring2 <- rings.map(Option.apply) - ring1 + None
  } yield weapon + armour.getOrElse(empty) + ring1.getOrElse(empty) + ring2.getOrElse(empty)

  def ttk(hp: Int, damage: Int, armour: Int) = (hp.toDouble / Math.max(damage - armour, 1)).ceil.toInt

  def bossWins(player: Actor) = ttk(boss.hp, player.damage, boss.armour) <=
    ttk(player.hp, boss.damage, player.armour)

  lazy val boss = Actor(103, 9, 2)

  def part1 = purchases.toList.sortBy(_.cost).map(inv => Actor(100, inv.damage, inv.armour)).dropWhile(bossWins).head
  def part2 = ???
}
