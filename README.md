# Miranda
A collection of works in the Miranda functional programming language. I first started programming in Miranda at the tail-end of 2025, creating small scripts in private repositories. Now with a few months of experience writing functional code I wanted to share what I've written in this wonderful language.

Miranda can generally be considered the precursor of the well-known Haskell language and inspired much of the latter's design. And yet many developers have never even heard of it.

To some degree, programming today in Miranda offers a glimpse back in time to a much more bare-bones development experience. There is no syntax-highlighting VSCode extension, no Windows port of the language (WSL solves this, though), and only a minimal set of predefined functions in the standard environment.

And yet oxymoronically none of this detracts from what is a thrilling and efficient way to make rocks do maths.

## 1. Projects

### 1.1. solitaire
`solitaire/`

The challenge here was to implement the game 'peg solitaire' in Miranda. Once I completed this, I decided to extend the game to a version I donned the inspiring name 'soliteverett'.

### 1.2. utils
`utils/`

A custom non-standard library to implement common, reusable functions for the Miranda programming language. Includes the following collections:
- io
- lists
- strings

### 1.3. minesweeper
`minesweeper/`

A functional implementation of minesweeper. Building on the addition of program interactivity that I solved in solitaire, this implementation also requires stateless implementations of flood fills and board preparation. I make further use of algebraic types, and I have begun implementing my utils libraries into the codebase.