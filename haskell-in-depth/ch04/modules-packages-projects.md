# Chapter 4 Development with modules, packages, and projects

Chapter covers:

* Structuring programs with modules

* Custom preludes

* Ideas and approaches behind projects

* Tools for package and project management

## 4.1 Organizing with modules

__Every Haskell application is a collection of modules.__

* `Main` module with the `main` function inside it.

* Only role of a module to be a tool for namespace control and states that any multi-module program can be transformed into a single module program.

### 4.1.1 imports and exports

```haskell
module ModuleName (<export list>) where

-- set of imports

-- declarations for
-- * type classes and instances
-- * type synonyms and data types
-- * functions
-- * etc.

```

* A module's name begin with an uppercase letter;
* Its expected to reside in the `.hs` file with the same name
* The `Main` module which is allowed to be placed else where.

:oncoming_police_car: Imports:

* Import the whole module
* Import only specific names from the module by listing them in parentheses after the module name;
* Import no names at all with an empty list of names;
* Import names with optional or mandatory qualification with an alias `as` or a full module name to avoid name clashes;
* Import all names except those listed after the `hiding` keyword.

:oncoming_police_car: Exports:

* Names of the functions;
* Names of the data types (type constructors), with or without data constructors (`(..)` means all of them);
* Names of the type classes, with or without method names;
* A module name or its synonym prefixedc with the `module` keyword for exporting all names imported from the corresponding module or the namespace identified by the synonym.

```haskell
module ModuleName (
    module X, -- Reexports everything from module X

    DataType1, -- Only the type constructor is exported,
               -- :bomb: cannot do pattern matching on its data constructor
               -- or construct values of this data type

    DataType2 (..), -- Exports the type constructor with all data constructor

    DataType3 (Cons1, Cons2), -- Exports the type constructor with two
                              -- mentioned data constructors

    TypeClass1,

    TypeClass2 (..),

    TypeClass3 (method1, method2),

    function

) where
    ...
```

#### Hierarchy

* Module name can be hierarchical, with components separated by dots:
`Graphic.Rendering.Chart.Backend.Cairo`

* Hierarchical module names are about naming only;

* Use the `-i` flag to specify the  root directory: `ghci B.hs -i..`.

* Common names at the top of hierarchy:
  * `Data` - working with data;
  * `Control` - control structures;
  * `System` - OS-specific interfaces;
  * `Text` - text processing
  * `Network` - network processing

* Inside hierarachy:
  * `Internal` - for implementation detail
  * `Tutorial` - for examples

### 4.1.2 Custom Preludes

