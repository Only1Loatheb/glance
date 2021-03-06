# to run

Using Atom, in a terminal run
cd app
stack exec glance-exe -- -l -s Main.hs -o ../output.svg

or if that does not work
<!-- stack build  --exec "glance-exe -o output.svg -w 500" -->
```bash
stack build  --exec "glance-exe ./examples/fact.hs ./images/img.svg 500"
```

View circle.svg with svg-preview plug-in.

To use ghci for the main executable:
stack ghci glance

To use ghci with the test modules:
stack ghci glance:test:glance-test

For all warnings (some warnings duplicated):

```bash
stack clean
stack build --test --no-run-tests --ghc-options -Wall
```

To open documentation for other libraries:
stack haddock --open <package-name>

examples to work with:

```bash
stack build --test  --ghc-options -Wall
stack exec glance-exe -- -c ./examples/tutorial.hs -o ./images/tutorial.svg
stack exec glance-exe -- -c ./examples/advantages.hs -o ./images/advantages.svg
stack exec glance-exe -- -c ./examples/lab1.hs -o ./images/lab1.svg
stack exec glance-exe -- -c ./examples/lab2.hs -o ./images/lab2.svg
stack exec glance-exe -- -c  ./examples/simpleFunctions.hs -o ./images/simpleFunctions.svg
stack exec glance-exe -- -c ./examples/quickSort.hs -o ./images/quickSort.svg
firefox --new-window ./test/test-output/visual-tests.svg & firefox --new-window ./images/ ;

```
