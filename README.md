# Zápočtový program

Program čte uživatelský vstup ve dvou různých [formátech](#formáty-vstupu) a pokud je
vstup validní, tak vypíše jak daný akord držet na pětistrunném banju. (banjo je sice pětistrunné, ale pátá struna nelze používat na akordy)

## Formáty vstupu

### Jako akord:
- tvořen třemi částmi postupně:
  - základní tón - C, C#, Db, D, D#, E, F, F#, Gb, G, G#, Ab, A, A#, Bb, B, Cb
  - Hlavní alternace - Moll (značeno jako m), Dur (neznačí se), aug, dim, sus4, sus2
  - Další úpravy akordu - 7, maj7, 9, maj9, add9

Příklady: `Am7`, `Cdim`, `G#maj7`, `Ebdim7`, `F`

### Jako seznam tónů

Musí být předcházen znakem `:` a jednotlivé tńy jsou odděleny mezerou.

Příklady (alternativy příkladů výše): `:A C E G`, `:C D# F#`, `:G# C D# G`, `:D# F# A C#`, `:F A C`

## Fungování

Vstup ve formátu akordu se nejřdíve vyparsuje do datové struktury Chord. Z té se následně
vytvoří seznam tónů a konečně seznam pražců na banju.

Vstup ve formátu seznamu tónů se přímo převede na seznam pražců na banju.

Seznam pražců je pouze jedno z možných držení na banju. Na nalezení jiných se vyzkouší všechny
permutace strun a všechny možné oktávy na banju. Vznikne seznam seznamů, který je seřazen pomocí
ohodnocení jednotlivých držení, kde hodnocení bere v potaz vzdálenost jednotlivých pražců od sebe a
také celkovou vzádlenost pražců od nultého pražce. Uživatel zadá počet a tento počet držení se vytiskne do konsole.

## Příklady

```
Input chord name or list of notes:
Gmaj7
Understood as: G Major Major7
Notes of chord: [G,B,D,F#]

Input number of finger placements to show:
3
Found 336 finger placements, showing 3:
D |---5---|
B |---3---|
G |---4---|
D |---4---|

D |---4---|
B |---3---|
G |---4---|
D |---5---|

D |---4---|
B |---0---|
G |---0---|
D |---0---|
```

```
Input chord name or list of notes:
:C D E
Notes of chord: [C,D,E]

Input number of finger placements to show:
2
Found 576 finger placements, showing 2:
D |---2---|
B |---3---|
G |---5---|
D |---2---|

D |---0---|
B |---1---|
G |---5---|
D |---2---|
```
