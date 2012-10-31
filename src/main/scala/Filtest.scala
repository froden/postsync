package no.posten.dpost.sync

import java.io.File

object Filtest extends App {
  new File("/tmp").listFiles.foreach(f => println(f.getAbsolutePath))
  println(new File("/tmp/æøå").exists)
}