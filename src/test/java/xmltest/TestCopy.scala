package xmltest

import dataprocess.defaultimpl.DefaultState
import torch.torch.TensorDef

object TestCopy extends App {
  case class Pointer(a: Int, b: Int)  {
  }

  val state: DefaultState = new DefaultState()
  state.data(1)(2)(3) = 2

  val p0: Pointer = Pointer(1, 2)
  p0.copy(3)

  val copyState = state.clone().asInstanceOf[DefaultState]
  copyState.data(2)(3)(4) = 3

  println("Orig data: " + state.data(2)(3)(4) + ", " + state.data(1)(2)(3))
  println("New data: " + copyState.data(2)(3)(4) + ", " + copyState.data(1)(2)(3))

//  val tester = new TensorDef()
//  println("Get data type: {}", tester.getDataType)
}
