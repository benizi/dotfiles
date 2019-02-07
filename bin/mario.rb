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
# + for durations and rests
MARIO = <<'MARIO'
E5 E5 r E5
r C5 E5 r G5 r r r G4 r r r

C5 r r G4 r r E4 r
r A4 r B4 r Bb4 A4 r
h G4 h E5 h G5 A5 r
F5 G5 r E5 r C5 D5 B4 r r

C5 r r G4 r r E4 r
r A4 r B4 r Bb4 A4 r
h G4 h E5 h G5 A5 r
F5 G5 r E5 r C5 D5 B4 r r

r r G5 F#5 F5 D5 r E5
# [sic] r G4 A4 C5
r G#4 A4 C5
r A4 C5 D5
r r G5 F#5 F5 D5 r E5
r C6 r C6 C6 r r r

r r G5 F#5 F5 D5 r E5
# [sic] r G4 A4 C5
r G#4 A4 C5
r A4 C5 D5
r r D#5 r r D5 r r C5 r r r r r r r

# mariopiano repeats the prior two phrases?

C5 C5 r C5
r C5 D5 r E5 C5 r A4 G4 r r r
C5 C5 r C5
r C5 D5 E5 r r r r r r r r

C5 C5 r C5
r C5 D5 r E5 C5 r A4 G4 r r r
E5 E5 r E5
r C5 E5 r G5 r r r
G4 r r r

C5 r r G4 r r E4 r
r A4 r B4 r Bb4 A4 r
h G4 h E5 h G5 A5 r
F5 G5 r E5 r C5 D5 B4 r r

C5 r r G4 r r E4 r
r A4 r B4 r Bb4 A4 r
h G4 h E5 h G5 A5 r
F5 G5 r E5 r C5 D5 B4 r r

E5-C5 r G4
r r G4 r A4 F5 r F5 A4 r r r
h B4 h A5 h A5 h A5 h G5 h F5
E5 C5 r A4 G4 r r r

E5-C5 r G4
r r G4 r A4 F5 r F5 A4 r r r
B4 F5 r F5 h F5 h E5 h D5 C5
G4 r E4 C4 r r r

TODO

# not sure what this part is supposed to be?

C5 G4 E4
A4 B4 A4
G#4 Bb4 G#4
G4-F#4-G4
MARIO

q = 12
puts MARIO.
  gsub(/\A.*SKIP/m, '').
  gsub(/TODO.*\Z/m, '').
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
