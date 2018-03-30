initialCommands in console := "jline.TerminalFactory.get.init"

/*
// https://github.com/sbt/sbt/issues/3841#issuecomment-355791771
console in Compile := (console in Compile)
      .dependsOn(Def task jline.TerminalFactory.get.init)
      .andFinally(jline.TerminalFactory.get.restore)
      .value
*/

/*
// https://github.com/sbt/sbt/issues/3841#issuecomment-355794813
jlineWorkarounds
def jlineWorkarounds = Def.settings(
  Seq(
    console in Compile,
    console in Test,
  ) map { key =>
    key := key
      .dependsOn(Def.task { jline.TerminalFactory.get.init })
      .andFinally(jline.TerminalFactory.get.restore)
      .value
  }: _*)
*/

/*
(Compile/console) := Def.task {
  val wrapped = (Compile/console).value
  jline.TerminalFactory.get.init
  println("inited")
  wrapped
}
*/

/*
  .dependsOn(Def.task {
  })
  .andFinally(Def.task {
    jline.TerminalFactory.get.restore
    println("restore")
  })
  .value
*/
