# Todo

## Todo Now

* Redesign case icon to avoid non-locality.
* Move @ pattern circles so that they are on the same side as the variables.
* Add command line flags for color style, embedding, and whether to draw arrowheads.
* Add wiki pages discussing: Why a visual language?, History of Glance, How to contribute, Code guide [code style, ...], etc..

## Todo Later

### Testing todos

* Fix the arrowheads being too big for SyntaxGraph drawings.

### Visual todos

* Use different line styles (e.g. dashed, solid, wavy) in addition to colors

* Consider improving nested apply icons embedded in case/guard icons.

* Let lines connect to ports other than the original source

* Use diagrams to shrink the drawing until icons start overlapping.

* Make an icon font/library with labeled ports. E.g. the apply icon would have text labels "function", "result", "arg 0", "arg 1", etc.

### SimpSyntaxToSyntaxGraph todos

* Fix applyComposeScore in SimpSyntaxToSyntaxGraph.hs not counting expressions that nest via reference. May need to move compose generation to after SimpSyntaxToSyntaxGraph.

* Add proper RecConstr, and RecUpdate support.

* Special case for otherwise.

### Command line todos

* Tab completion
