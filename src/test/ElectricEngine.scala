package test

object Tester extends App {

  //	trait EngineComp { self : TechnologyComp =>
  //	  def hybrid = electric & gas 
  //	}

  trait EngineComp { self: Assembly =>
    def hybrid = electric & gas
  }

  //	trait TechnologyComp { this : SubstanceComp with EngineComp =>
  //	  def electric: Boolean
  //	  def gas: Boolean
  //	  def highTech= sustainable & (hybrid || electric)
  //	}

  trait TechnologyComp { this: Assembly =>
    def electric: Boolean
    def gas: Boolean
    def highTech = sustainable & (hybrid || electric)
  }

  //	trait SubstanceComp { this : TechnologyComp =>
  //	  def sustainable= electric || hydrogen
  //	  def hydrogen: Boolean
  //	}

  trait SubstanceComp { this: Assembly =>
    def sustainable = electric || hydrogen
    def hydrogen: Boolean
  }

  trait Assembly extends EngineComp with TechnologyComp with SubstanceComp

  object ElectricEngine extends Assembly {
    def electric = true
    def hydrogen = false
    def gas = false
    override def toString() = "ElectricEngine -> hybrid= "+hybrid+" , electric= "+electric+" , gas= "+gas+", sustainable= "+sustainable
  }

  println(ElectricEngine)
}
