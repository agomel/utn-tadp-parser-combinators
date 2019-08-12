package practicCase

import MusicParser._

object PlayAudio extends App {
  val felizCumpleanios = "4C1/4 4C1/4 4D1/2 4C1/4 4F1/2 4E1/2 4C1/8 4C1/4 4D1/2 4C1/2 4G1/2 4F1/2 4C1/8 4C1/4 5C1/2 4A1/2 4F1/8 4F1/4 4E1/2 4D1/2"
  AudioPlayer.reproducir(melodiaParser(felizCumpleanios).get._1)

  val cancionBonus = "4AM1/8 5C1/8 5C#1/8 5C#1/8 5D#1/8 5C1/8 4A#1/8 4G#1/2 - 4A#1/8 4A#1/8 5C1/4 5C#1/8 4A#1/4 4G#1/2 5G#1/4 5G#1/4 5D#1/2"
  AudioPlayer.reproducir(melodiaParser(cancionBonus).get._1)
}
