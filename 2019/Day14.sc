import $file.Util
import $ivy.`org.scalatest::scalatest:3.2.2`

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable.{Map => MMap}

val rawInput = Util.inputForDay(14)

type Ingredient = String

case class IngredientValue(val amount: Int, val ingredient: Ingredient)

case class Recipe(
    val product: IngredientValue,
    val requirements: Set[IngredientValue]
)

def parseInput(rawInput: List[String]): List[Recipe] = {
  rawInput.map { line =>
    {
      val Array(before, after) = line.split(" => ")
      def parseTuple(tuple: String) = {
        val Array(amountStr, ingredient) = tuple.split(" ")
        IngredientValue(amountStr.toInt, ingredient)
      }
      Recipe(
        product = parseTuple(after),
        requirements = before.split(", ").map(parseTuple).toSet
      )
    }
  }
}

val START = "FUEL"
val END = "ORE"

def part1(rawInput: List[String]): Int = {
  // build map of ingredients to recipes
  val recipes = parseInput(rawInput)
  val ingredientMap = recipes.map { recipe =>
    recipe.product.ingredient -> recipe
  }.toMap

  // recurse backwards and see how many ORE we need to get there
  countOre(IngredientValue(1, START), ingredientMap, MMap.empty)
}

def countOre(
    target: IngredientValue,
    recipeMap: Map[Ingredient, Recipe],
    remainders: MMap[Ingredient, Int]
): Int = {
  if (target.ingredient == END) {
    target.amount
  } else {
    val existingRemainder = remainders.get(target.ingredient).getOrElse(0)
    remainders.remove(target.ingredient)
    val recipe = recipeMap.get(target.ingredient).get
    // find out how many we need to make
    val recipeMultiple: Int = if (existingRemainder >= target.amount) {
      0
    } else {
      Math
        .ceil(
          (target.amount.toDouble - existingRemainder) / recipe.product.amount
        )
        .toInt
    }

    val remainder =
      recipe.product.amount * recipeMultiple - (target.amount - existingRemainder)

    remainders += (target.ingredient -> remainder)

    if (recipeMultiple > 0) {
      recipe.requirements.map { requirement =>
        countOre(
          target = IngredientValue(
            requirement.amount * recipeMultiple,
            requirement.ingredient
          ),
          recipeMap,
          remainders
        )
      }.sum
    } else {
      // we don't have to make more of this ingredient, we have enough leftover from others making it
      0
    }

  }
}

println(part1(rawInput))

// unit tests

val testCases = Seq(
  (
    """157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT""",
    13312
  ),
  (
    """2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF""",
    180697
  ),
  (
    """171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX""",
    2210736
  )
)

class Spec extends AnyFlatSpec {
  "parseInput" should "work" in {
    parseInput(List("17 NVRVD, 3 JNWZP => 8 VPVL")) should be(
      List(
        Recipe(
          requirements = Set(
            IngredientValue(17, "NVRVD"),
            IngredientValue(3, "JNWZP")
          ),
          product = IngredientValue(8, "VPVL")
        )
      )
    )
  }

  "part1" should "pass test cases" in {
    testCases.foreach {
      case (rawInput, expected) => {
        part1(rawInput.split("\n").toList) should be(expected)
      }
    }
  }
}

(new Spec()).execute()
