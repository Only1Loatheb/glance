# other diagrams for Glance

Glance is a visual syntax for the programming language Haskell. The goal of this project is to increase programmer happiness and productivity by allowing programmers to create and understand programs in new and different ways. Currently, the Glance executable produces a visual representation of your code in the form of an SVG image
or interactive application available via web browser when given a textual Haskell source file.

The current Glance program is an experiment created to answer the question:  
*Is it possible to create a readable and compact graphical representation of Haskell function and value declarations?*

For small to medium functions, in my opinion Glance produces images good enough to indicate that the answer is yes. For large functions, the results right now are inconclusive due to issues with graph layout. Specifically, large programs unnecessarily become very spread out.

## Try it

First install Graphviz. For instance, in Ubuntu run:

```bash
sudo apt install graphviz
```

Then build and execute glance:

```bash
stack build
stack exec glance-exe -- -c ./examples/simpleFunctions.hs -o ./images/simpleFunctions.svg
```

To see the command line options run:

```bash
stack exec glance-exe -- --help
```

Now display the SVG image in a web browser:

```bash
firefox --new-window ./images/simpleFunctions.svg
```

You should now see in your browser window a visual representation of simple functions. Next, you will probably want to read the Getting Started guide below to help understand the images Glance generates.

## Issues

Glance is still in development, so for the time being, graph layout, line routing, and icon design all have much room for improvement. Here are some specific issues:

* Text may be misaligned with some SVG viewers.
* Only a subset of Haskell is rendered.
* Edges may cover nodes.

## Getting started

Next image is a getting started guide for Glance rendered by Glance itself ([source here](examples/tutorial.hs)).
To start interactive aplication run command below and visit [localhost:3000](http://localhost:3000/).

```bash
stack exec glance-exe -- -i ./examples/tutorial.hs
```

<img src="https://github.com/Only1Loatheb/glance/blob/other_diagrams/examples/tutorial.svg" alt="Introduction to visual representation" />

You should now be able to understand Glance's visual syntax. If you would like to see how visual code might be an improvement over textual code.

## Thanks

Thanks to [Robbie Gleichman](https://github.com/rgleichman) for creating Glance project.

A large thanks to the creators of [diagrams](http://projects.haskell.org/diagrams/), the main Haskell library used in this project.

Thank you to the [Santa Monica Haskell Users Group](https://www.meetup.com/santa-monica-haskell/) for their support and feedback.

Also thanks to the [/r/haskell](https://www.reddit.com/r/haskell/) subreddit for [reviewing a very early design of the language](https://www.reddit.com/r/haskell/comments/35swgl/review_my_introduction_to_glance_a_new_visual/).
