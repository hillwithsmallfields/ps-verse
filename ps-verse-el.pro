%%BeginProcSet ps-verse-kshow 0 0

1 setlinewidth

/khbuf (;   : ) def

/kernhack {
  khbuf 3 3 -1 roll put
  khbuf 2 3 -1 roll put
  kernstring khbuf search
  {
    pop pop				% pre and match
    cvr fontscale mul 0 rmoveto
  } {
    pop					% the original string
  } ifelse
} bind def

/ks { {kernhack} exch kshow } bind def

/reference {
  400 100 moveto
  gsave /ZapfChancery-MediumItalic findfont 15 scalefont setfont
        dup stringwidth pop neg 0 rmoveto
        show                                      grestore
} bind def

/stave_lh 9 def

/stave {
  gsave
    currentpoint translate
    0 stave_lh -3 mul translate
    0 0 moveto
    0 stave_lh 3 mul rlineto
    dup 0 moveto
    0 stave_lh 3 mul rlineto
    0 1 3 {
      stave_lh mul
      0 exch moveto
      dup 0 rlineto
    } for
    pop
    stroke
  grestore
  /notesize stave_lh 3 div def
  /hns notesize 2 div def
  /hmul stave_lh 2 div def
} bind def

/square {
  gsave
    currentpoint translate 0 exch hmul mul stave_lh 3 mul sub translate
    newpath 0 hns neg moveto
    0 hns lineto
    notesize hns lineto
    notesize hns neg lineto
    closepath
    gsave fill grestore stroke
  grestore
} bind def

/note {
  exch
  square
  0 rmoveto
} bind def

/falling {
  3 1 roll
  exch
  2 copy
  gsave
    currentpoint translate
    newpath
    0 exch hmul mul stave_lh 3 mul sub hns add moveto
    0 exch hmul mul stave_lh 3 mul sub hns sub lineto
    stroke
  grestore
  2 copy
  gsave
    currentpoint translate
    newpath
    notesize exch hmul mul stave_lh 3 mul sub hns add moveto
    notesize exch hmul mul stave_lh 3 mul sub hns sub lineto
    stroke
  grestore
  square
  notesize 0 rmoveto
  square
  0 rmoveto
} bind def

/rising {
  3 1 roll
  2 copy
  gsave
    currentpoint translate
    newpath
    notesize exch hmul mul stave_lh 3 mul sub moveto
    notesize exch hmul mul stave_lh 3 mul sub lineto
    stroke
  grestore
  square
  square
  0 rmoveto
} bind def

/cpitch {
  exch
  dup 1 add exch 1 sub
  2 copy
  gsave
    currentpoint translate
    newpath
    0 exch hmul mul stave_lh 3 mul sub moveto
    0 exch hmul mul stave_lh 3 mul sub lineto
    stroke
  grestore
  square
  square
  0 rmoveto
} bind def

/flat {
  gsave
    currentpoint translate
    notesize hns add neg exch hmul mul stave_lh 3 mul sub translate
    0 hns neg translate
    newpath
    0 notesize moveto
    notesize hns lineto
    0 0 lineto
    0 notesize 2 mul lineto
    stroke
  grestore
} bind def

% centred show, around currentpoint
/cs {
  gsave
    dup stringwidth pop -0.5 mul 0 rmoveto
    show
  grestore
} bind def

/w {
  stringwidth pop
} bind def

%%EndProcSet
