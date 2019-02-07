#!/usr/bin/env ruby

=begin
# http://glassarmonica.com/science/frequency_midi.php

21 A0 27.5
24 C1
59 B3
60 C4
69 A4 440.0
71 B4
72 C5
81 A5 880.0
93 A6 1760.0
105 A7 3520.0
108 C8 4186.0

f = 27.5 * 2^((m - 21)/12)
# m = (12/ln(2)) * ln(f/27.5) + 21
=end

OCTAVE = %w(C C# D D# E F F# G G# A A# B)
NOTES = Hash[OCTAVE.map.with_index.to_a]
OCTAVE.reverse.each_cons(2) { |b, as| NOTES["#{b}b"] = NOTES[as] if as =~ /#/ }

def freq(note)
  name, octave = note.split(/(\d)/)
  octave = octave.to_i
  base = NOTES[name]
  midi = base + OCTAVE.size * (1 + octave)
  (27.5 * 2 ** ((midi - 21).to_f/12)).to_i
end

# https://noobnotes.net/super-mario-bros-theme-nintendo/
# + converted ^X -> X5, *X -> X6
# http://www.mariopiano.com/mario-sheet-music-overworld-main-theme.html
MARIO = <<'MARIO'
E5 E5 E5
C5 E5 G5 G4

C5 G4 E4
A4 B4 Bb4 A4
G4 E5 G5 A5
F5 G5 E5 C5 D5 B4

C5 G4 E4
A4 B4 Bb4 A4
G4 E5 G5 A5
F5 G5 E5 C5 D5 B4

G5 F#5 F5 D5 E5
# [sic] G4 A4 C5
G#4 A4 C5
A4 C5 D5
G5 F#5 F5 D5 E5
C6 C6 C6

G5 F#5 F5 D5 E5
# [sic] G4 A4 C5
G#4 A4 C5
A4 C5 D5
D#5 D5 C5

# mariopiano repeats the prior two phrases?

C5 C5 C5
C5 D5 E5 C5 A4 G4
C5 C5 C5
C5 D5 E5

C5 C5 C5
C5 D5 E5 C5 A4 G4
E5 E5 E5
C5 E5 G5
G4

C5 G4 E4
A4 B4 Bb4 A4
G4 E5 G5 A5
F5 G5 E5 C5 D5 B4

C5 G4 E4
A4 B4 Bb4 A4
G4 E5 G5 A5
F5 G5 E5 C5 D5 B4

E5-C5 G4
G4 A4 F5 F5 A4
B4 A5 A5 A5 G5 F5
E5 C5 A4 G4

E5-C5 G4
G4 A4 F5 F5 A4
B4 F5 F5 F5 E5 D5 C5
G4 E4 C4

# not sure what this part is supposed to be?

C5 G4 E4
A4 B4 A4
G#4 Bb4 G#4
G4-F#4-G4
MARIO

q = 12
puts MARIO.
  gsub(/^#.*$/s, '').
  gsub(/-/, " ").
  gsub(/r/, "0 #{q} ").
  gsub(/[A-G][#b]?\d/) { |m| "#{freq(m)} #{q} " }.
  gsub(/h \d+ \d+/) { |m|
    _, note, dur = m.split
    dur = dur.to_i
    # h = half-note triplets -> quarter note + 1/3 quarter rest
    "#{note} #{dur} 0 #{(dur.to_f / 3.0).to_i}"
  }
