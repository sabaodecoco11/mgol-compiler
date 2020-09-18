package Enum

object AutomataAction extends Enumeration {
  type ActionOverAutomata = Int

  val INITIAL_STATE = 0;
  val TRANSITION_NOT_FOUND = -1;
  val SYMBOL_NOT_FOUND = -2;
}
