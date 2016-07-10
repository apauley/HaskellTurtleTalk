# Slides for "Haskell Shell Scripting with Turtle"

A [Lambda Luminaries](http://www.meetup.com/lambda-luminaries/) talk:

http://www.meetup.com/lambda-luminaries/events/231686085/

## Generating Slides

If you have `turtle` installed:

```bash
$ ./generate.hs
```

Otherwise use `pandoc` directly:

```bash
$ pandoc -t slidy --self-contained -s slides.md -o slides.html
```

Then open `slides.html` in a web browser.
